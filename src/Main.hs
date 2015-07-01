{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Github.Repos
import Github.Issues
import Github.Auth
import Github.Users
import Data.Aeson
import Data.Monoid
import Formatting
import Control.Retry
import Data.Time.Clock
import Options.Applicative
import Control.Concurrent.Async
import Data.Function (on)
import System.IO (stderr)
import System.Exit (exitFailure)
import Control.Monad (liftM, when, join)
import Data.List (sortBy, genericLength)
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Builder as T
import qualified Formatting.Internal as FI
import qualified Formatting.ShortFormatters as F

type Url = String
type RepoName = String
type LoginName = String
type FullName = String
type Contributions = Int

-- https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/
data Pretty = Pretty | Uglified deriving (Show, Eq)
data Verbosity = Verbose | Quiet deriving (Show, Eq)
data Entity = GetUser | GetOrg deriving (Show, Eq)

data MetaInfo a = MetaInfo { metaEntity :: String
                           , metaRepo :: RepoName
                           , metaData :: a
                           } deriving (Show)

data ContributorData = ContributorData { contribCount :: Int
                                       , loginName :: LoginName
                                       , htmlUrl :: Url
                                       , contributedTo :: [RepoName]
                                       , avatarUrl :: Maybe Url
                                       , fullName :: Maybe FullName
                                       } deriving (Show)

data Options = Options { optQuiet :: Verbosity
                       , optEntity :: Entity
                       , optPretty :: Pretty
                       , optUseAsync :: Bool
                       , optAuthKey :: Maybe GithubAuth
                       , jsonPFunc :: Maybe String
                       , optOutFile :: Maybe FilePath
                       , optName :: String
                       } deriving (Show)

data Results = Results UTCTime
                       String
                       [Repo]
                       [ContributorData]
                       [MetaInfo Issue]
                       deriving (Show)

instance Functor MetaInfo where
    fmap fn mi@MetaInfo{..} = mi {metaData = fn metaData}

instance ToJSON ContributorData where
    toJSON ContributorData{..} =
        object [ "avatar" .= avatarUrl
               , "login" .= loginName
               , "count" .= contribCount
               , "contributed" .= contributedTo
               , "name" .= fullName
               , "url" .= htmlUrl]

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

userUrl :: LoginName -> Url
userUrl = formatToString $ "https://github.com/" % F.s % "/"

labelHtmlUrl :: String -> String -> String -> Url
labelHtmlUrl = formatToString $ "https://github.com/" % F.s % "/" % F.s % "/labels/" % F.s

getName :: Contributor -> LoginName
getName (KnownContributor _ _ login _ _ _) = login
getName (AnonymousContributor _ login) = login

getAvatar :: Contributor -> Maybe Url
getAvatar (KnownContributor _ avatar _ _ _ _) = pure avatar
getAvatar (AnonymousContributor _ _) = Nothing

getContributions :: Contributor -> Contributions
getContributions (KnownContributor ccnt _ _ _ _ _) = ccnt
getContributions (AnonymousContributor ccnt _) = ccnt

filterContrib :: (String -> String -> Bool) -- Compare function
              -> MetaInfo Contributor -- Contributor in question
              -> [MetaInfo Contributor] -- All other contributors
              -> [MetaInfo Contributor] -- Filtered results
filterContrib f = filter . on f (getName . metaData)

nubContributors :: Options -> [MetaInfo Contributor] -> IO [ContributorData]
nubContributors _ [] = return []
nubContributors opt cs@(md:_) = do
    fulln <- getFullName opt c
    let newcs = filterContrib (/=) md cs
        onlyc = filterContrib (==) md cs
        ccnt = sum $ map (getContributions . metaData) onlyc
        contributed = map metaRepo onlyc
        newc = ContributorData { contribCount = ccnt
                                , avatarUrl = getAvatar c
                                , htmlUrl = userUrl $ getName c
                                , contributedTo = contributed
                                , loginName = getName c
                                , fullName = fulln
                                }
    (newc :) <$> nubContributors opt newcs
    where c = metaData md

getFullName :: Options -> Contributor -> IO (Maybe FullName)
getFullName Options{..} (AnonymousContributor _ _) = return Nothing
getFullName Options{..} (KnownContributor _ _ name _ _ _) =
    fmap detailedOwnerName . eitherIO $ do
        printIf (optQuiet == Verbose) ("Full name of " % F.s) name
        userInfoFor' optAuthKey name

sortWith :: (Ord b) => (a -> b) -> [a] -> [a]
sortWith = sortBy . on compare

-- Adventures of abstraction continue!
getInfo :: (Show e)
        => Options
        -> (String -> IO (Either e [a])) -- Function accepting repo name and retriving an array of data
        -> ([[MetaInfo a]] -> r) -- Function that organizes nested per repo list
        -> [Repo] -- Repos to use
        -> IO r
getInfo Options{..} get cleanup = liftM cleanup . (if optUseAsync then mapConcurrently else mapM) callGet
    where callGet Repo{..} = liftM (map (MetaInfo (githubOwnerLogin repoOwner) repoName)) . eitherIO $ get repoName

parseOpt :: Parser Options
parseOpt = Options <$> flag Verbose Quiet (long "quiet" <> short 'q' <> help "Enable quiet mode")
                   <*> flag GetOrg GetUser (long "user" <> short 'u' <> help "Get user data")
                   <*> flag Uglified Pretty (long "pretty" <> short 'p' <> help "Pretty print json")
                   <*> switch (long "async" <> short 's' <> help "Use async requests (experimental)")
                   <*> optional (GithubOAuth <$> strOption (long "key" <> short 'a' <> metavar "OAUTHKEY" <> help "Github OAuth key"))
                   <*> optional (strOption $ long "jsonp" <> short 'j' <> metavar "FUNC" <> help "Function to use for JsonP")
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

-- Can't use putStrLn here because newline is not atomic :(
-- It's this or passing a lock through mapConcurrently
printIf :: Bool -> Format (IO ()) a -> a
printIf p m = FI.runFormat m (when p . T.putStr . T.toLazyText . (<> T.singleton '\n'))

-- Default issue limitations
issueLimitations :: [IssueLimitation]
issueLimitations = [Open]

-- Given a repo list get all data
getResults :: Options -> [Repo] -> IO ([MetaInfo Issue], [ContributorData])
getResults opt@Options{..} repos =
    (,) <$> getInfo opt getIssues concat repos
        <*> join (getInfo opt getContribs processContribs repos)
    where verbose = printIf (optQuiet == Verbose)
          getContribs name = do verbose ("Contributors to " % F.s) name
                                contributors' optAuthKey optName name
          getIssues name = do verbose ("Open issues of " % F.s) name
                              issuesForRepo' optAuthKey optName name issueLimitations
          processContribs = fmap (reverse . sortWith contribCount) . nubContributors opt . concat

-- File writing wrapper over getResults
writeData :: Options -> IO ()
writeData opt@Options{..} = do
    start <- getCurrentTime
    verbose ("Generating data for " % F.s) optName
    verbose "Getting repo list..\n"
    repos <- eitherIO $ getRepos optEntity
    (issues,contribs) <- getResults opt repos
    verbose ("\nSaving data to " % F.s) fileName
    when (isJust jsonPFunc) . verbose ("Wrapping with " % F.s % " function") $ fromJust jsonPFunc
    writeJson fileName $ Results start optName repos contribs issues
    finish <- getCurrentTime
    verbose ("Done! Took " % F.sh) $ diffUTCTime finish start
    where verbose = printIf (optQuiet == Verbose)
          fileName = fromMaybe (optName ++ ".json") optOutFile
          encoder Pretty = encodePrettyToTextBuilder
          encoder Uglified = encodeToTextBuilder
          getRepos GetOrg = organizationRepos' optAuthKey optName
          getRepos GetUser = userRepos' optAuthKey optName Public
          buildJsonP Nothing t = t
          buildJsonP (Just func) t = T.fromString func <> T.singleton '(' <> t <> T.singleton ')'
          writeJson name = T.writeFile name . T.toLazyText . buildJsonP jsonPFunc . encoder optPretty . toJSON

main :: IO ()
main = writeData =<< execParser parserInfo
