{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Github.Repos
import Github.Issues
import Github.Auth
import Data.Aeson
import Formatting
import Data.Time.Clock
import Options.Applicative
import Data.Function (on)
import System.IO (stderr)
import Data.Maybe (fromMaybe)
import System.Exit (exitFailure)
import Control.Monad (liftM, when)
import Data.List (sortBy, genericLength)
import Data.Aeson.Encode (encodeToTextBuilder)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Builder as T
import qualified Formatting.Internal as FI
import qualified Formatting.ShortFormatters as F

data Verbosity = Verbose | Quiet deriving (Show, Eq)
data Entity = GetUser | GetOrg deriving (Show, Eq)

data MetaInfo a = MetaInfo { metaEntity :: String
                           , metaRepo :: String
                           , metaData :: a
                           } deriving (Show)

data Options = Options { optQuiet :: Verbosity
                       , optEntity :: Entity
                       , optAuthKey :: Maybe GithubAuth
                       , optOutFile :: Maybe FilePath
                       , optName :: String
                       } deriving (Show)

data Results = Results UTCTime String [Repo] [MetaInfo Contributor] [MetaInfo Issue] deriving (Show)

instance Functor MetaInfo where
    fmap fn mi@MetaInfo{..} = mi {metaData = fn metaData}

instance ToJSON (MetaInfo Contributor) where
    toJSON (MetaInfo {metaData = (KnownContributor ccnt avatar username _ _ _)}) =
        object ["avatar" .= avatar, "login" .= username, "count" .= ccnt, "url" .= userUrl username]
    toJSON (MetaInfo {metaData = (AnonymousContributor ccnt username)}) =
        object ["avatar" .= anonUrl, "login" .= username, "count" .= ccnt, "url" .= userUrl username]
        where anonUrl = "#" :: String

instance ToJSON (MetaInfo Issue) where
    toJSON mi@(MetaInfo _ repo Issue{..}) =
        object [ "url" .= issueHtmlUrl
               , "number" .= issueNumber
               , "title" .= issueTitle
               , "labels" .= map (<$ mi) issueLabels
               , "repository" .= repo
               , "created" .= fromGithubDate issueCreatedAt]

instance ToJSON (MetaInfo IssueLabel) where
    toJSON (MetaInfo ent repo (IssueLabel color _ name)) =
        object ["color" .= color, "url" .= labelHtmlUrl ent repo name, "name" .= name]

-- See https://github.com/jwiegley/github/issues/110
instance ToJSON Repo where
    toJSON Repo{..} = object[ "url" .= repoHtmlUrl
                            , "name" .= repoName
                            , "language" .= repoLanguage
                            , "issues" .= repoOpenIssues
                            , "watchers" .= repoWatchers
                            , "forks" .= repoForks
                            , "description" .= repoDescription
                            , "language" .= repoLanguage
                            ]

instance ToJSON Results where
    toJSON (Results time org repos contribs issues) =
        object [ "time" .= time
               , "name" .= org
               , "contributors" .= contribs
               , "issues" .= issues
               , "repositories" .= repos]

userUrl :: String -> String
userUrl = formatToString $ "https://github.com/" % F.s % "/"

labelHtmlUrl :: String -> String -> String -> String
labelHtmlUrl = formatToString $ "https://github.com/" % F.s % "/" % F.s % "/labels/" % F.s

eitherIO :: (Show e) => IO (Either e a) -> IO a
eitherIO = eitherIOCnt 1

eitherIOCnt :: (Show e) => Int -> IO (Either e a) -> IO a
eitherIOCnt cnt io
    | attempts < cnt = hprint stderr ("Gave up after " % F.d % " attempts!") attempts >> exitFailure
    | otherwise = do res <- io
                     case res of
                          (Right a) -> return a
                          (Left e) -> do hprint stderr ("Retrying! [" % F.d % "/" % F.d % "] (" % F.sh % ")\n") cnt attempts e
                                         eitherIOCnt (cnt + 1) io
    where attempts = 5

getName :: Contributor -> String
getName (KnownContributor _ _ name _ _ _) = name
getName (AnonymousContributor _ name) = name

getContributions :: Contributor -> Int
getContributions (KnownContributor cnt _ _ _ _ _) = cnt
getContributions (AnonymousContributor cnt _) = cnt

updateContributions :: Int -> Contributor -> Contributor
updateContributions new (KnownContributor _ a b c d e) = KnownContributor new a b c d e
updateContributions new (AnonymousContributor _ a) = AnonymousContributor new a

nubContributors :: [MetaInfo Contributor] -> [MetaInfo Contributor]
nubContributors [] = []
nubContributors cs@(c:cr) = fmap (updateContributions cnts) c : nubContributors (filter ((name /=) . getName . metaData) cr)
    where cnts = foldl (\rc (metaData -> u) -> if getName u == name then rc + getContributions u else rc) 0 cs
          name = getName $ metaData c

sortWith :: (Ord b) => (a -> b) -> [a] -> [a]
sortWith f = sortBy (compare `on` f)

getInfo :: (Show e) => (String -> IO (Either e [a])) -> ([[MetaInfo a]] -> r) -> [Repo] -> IO r
getInfo get cleanup = liftM cleanup . mapM callGet
    where callGet Repo{..} = liftM (map (MetaInfo (githubOwnerLogin repoOwner) repoName)) . eitherIO $ get repoName

parseOpt :: Parser Options
parseOpt = Options <$> flag Verbose Quiet (long "quiet" <> short 'q' <> help "Enable quiet mode")
                   <*> flag GetOrg GetUser (long "user" <> short 'u' <> help "Get user data")
                   <*> optional (GithubOAuth <$> strOption (long "key" <> short 'a' <> metavar "OAUTHKEY" <> help "Github OAuth key"))
                   <*> optional (strOption $ long "file" <> short 'f' <> metavar "FILE" <> help "Output file name (default ENTITY.json)")
                   <*> strArgument (metavar "ENTITY" <> help "Entity (organization/user) name")

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> parseOpt)
                  (fullDesc <> header "contributor-list - Get issues and cotributors of a Github entity")

-- Call me crazy but I think view patterns make a lot sense here: They prevent a
-- pointless 'where' statment while at the same time keeping type signature specialized
-- preventing accidently mixing the order of argument if I just had 'Int -> Int -> Int -> IO ()'
printStats :: [Repo] -> [MetaInfo Contributor] -> [MetaInfo Issue] -> IO ()
printStats (genericLength -> r) (genericLength -> c) (genericLength -> i) = do
    fprint ("Found " % n % " issues, " % n % " contributors and " % n % " repositories\n") i c r
    fprint (f % " contributors/issue (" % f % " issues/contributor)\n") (c/i) (i/c)
    fprint (f % " issues/repository\n") (i/r)
    fprint (f % " contributors/repository\n") (c/r)
    where f :: Format r (Double -> r)
          f = F.f 1
          n = F.f 0

printIfLn :: Bool -> Format (IO ()) a -> a
printIfLn p m = FI.runFormat m (when p . T.putStrLn . T.toLazyText)

printIf :: Bool -> Format (IO ()) a -> a
printIf p m = FI.runFormat m (when p . T.putStr . T.toLazyText)

main :: IO ()
main = do
    opt@Options{..} <- execParser parserInfo
    let verbose = printIfLn (optQuiet == Verbose)
        blankLine = verbose ""
        fileName = fromMaybe (optName ++ ".json") optOutFile
    start <- getCurrentTime
    verbose ("Generating data for " % F.s) optName
    verbose "Getting repo list.."
    repos <- eitherIO $ if optEntity == GetOrg
                           then organizationRepos' optAuthKey optName
                           else userRepos' optAuthKey optName Public
    blankLine
    issues <- getInfo (getIssues opt) concat repos
    contribs <- getInfo (getContribs opt) (sortWith metaData . nubContributors . concat) repos
    blankLine
    verbose ("Saving data to " % F.s) fileName
    T.writeFile fileName . T.toLazyText . encodeToTextBuilder . toJSON $ Results start optName repos contribs issues
    when (optQuiet == Verbose) $ printStats repos contribs issues
    finish <- getCurrentTime
    verbose ("Done! Took " % F.s) (show $ diffUTCTime finish start)
    where getContribs Options{..} name = do printIfLn (optQuiet == Verbose) ("Contributors to " % F.s) name
                                            contributors' optAuthKey optName name
          getIssues Options{..} name = do printIfLn (optQuiet == Verbose) ("Open issues of " % F.s) name
                                          issuesForRepo' optAuthKey optName name issueLimitations
          issueLimitations = [Open]
