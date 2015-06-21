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
import Control.Retry
import Data.Time.Clock
import Options.Applicative
import Data.Function (on)
import System.IO (stderr)
import Data.Maybe (fromMaybe)
import System.Exit (exitFailure)
import Control.Monad (liftM, when)
import Data.List (sortBy, genericLength)
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Builder as T
import qualified Formatting.Internal as FI
import qualified Formatting.ShortFormatters as F

-- https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/
data Pretty = Pretty | Uglified deriving (Show, Eq)
data Verbosity = Verbose | Quiet deriving (Show, Eq)
data Entity = GetUser | GetOrg deriving (Show, Eq)

data MetaInfo a = MetaInfo { metaEntity :: String
                           , metaRepo :: String
                           , metaData :: a
                           } deriving (Show)

data Options = Options { optQuiet :: Verbosity
                       , optEntity :: Entity
                       , optPretty :: Pretty
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

-- Adventures of abstraction continue!
getInfo :: (Show e)
        => (String -> IO (Either e [a])) -- Function accepting repo name and retriving an array of data
        -> ([[MetaInfo a]] -> r) -- Function that rganises nested per repo list
        -> [Repo] -- Repos to use
        -> IO r
getInfo get cleanup = liftM cleanup . mapM callGet
    where callGet Repo{..} = liftM (map (MetaInfo (githubOwnerLogin repoOwner) repoName)) . eitherIO $ get repoName

parseOpt :: Parser Options
parseOpt = Options <$> flag Verbose Quiet (long "quiet" <> short 'q' <> help "Enable quiet mode")
                   <*> flag GetOrg GetUser (long "user" <> short 'u' <> help "Get user data")
                   <*> flag Uglified Pretty (long "pretty" <> short 'p' <> help "Pretty print json")
                   <*> optional (GithubOAuth <$> strOption (long "key" <> short 'a' <> metavar "OAUTHKEY" <> help "Github OAuth key"))
                   <*> optional (strOption $ long "file" <> short 'f' <> metavar "FILE" <> help "Output file name (default ENTITY.json)")
                   <*> strArgument (metavar "ENTITY" <> help "Entity (organization/user) name")

-- Retry an IO action while it's return a Left value
eitherIO :: (Show e) => IO (Either e a) -> IO a
eitherIO io = do
    res <- retrying policy (const check) io
    case res of
        Left _ -> hprint stderr "Giving up!\n" >> exitFailure
        Right a -> return a
    where policy = exponentialBackoff 500000 <> limitRetries 5
          check (Right _) = return False
          check (Left e) = fprint ("Retrying! (" % F.sh % ")\n") e >> return True

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
    where f = F.f 1 :: Format r (Double -> r)
          n = F.f 0 :: Format r (Double -> r)

printIf :: Bool -> Format (IO ()) a -> a
printIf p m = FI.runFormat m (when p . T.putStrLn . T.toLazyText)

issueLimitations :: [IssueLimitation]
issueLimitations = [Open]

-- Given a repo list get all data
getResults :: Options -> [Repo] -> IO ([MetaInfo Issue], [MetaInfo Contributor])
getResults Options{..} repos =
    (,) <$> getInfo getIssues concat repos
        <*> getInfo getContribs (sortWith metaData . nubContributors . concat) repos
    where verbose = printIf (optQuiet == Verbose)
          getContribs name = verbose ("Contributors to " % F.s) name >> contributors' optAuthKey optName name
          getIssues name = verbose ("Open issues of " % F.s) name >> issuesForRepo' optAuthKey optName name issueLimitations

-- File writing wrapper over getResults
writeData :: Options -> IO ()
writeData opt@Options{..} = do
    start <- getCurrentTime
    verbose ("Generating data for " % F.s) optName
    verbose "Getting repo list..\n"
    repos <- eitherIO $ getRepos optEntity
    (issues,contribs) <- getResults opt repos
    verbose ("\nSaving data to " % F.s) fileName
    writeJson fileName $ Results start optName repos contribs issues
    finish <- getCurrentTime
    verbose ("Done! Took " % F.sh) $ diffUTCTime finish start
    where verbose = printIf (optQuiet == Verbose)
          fileName = fromMaybe (optName ++ ".json") optOutFile
          encoder Pretty = encodePrettyToTextBuilder
          encoder Uglified = encodeToTextBuilder
          getRepos GetOrg = organizationRepos' optAuthKey optName
          getRepos GetUser = userRepos' optAuthKey optName Public
          writeJson name = T.writeFile name . T.toLazyText . encoder optPretty . toJSON

main :: IO ()
main = writeData =<< execParser parserInfo
