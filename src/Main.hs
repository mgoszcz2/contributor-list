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
import Data.Time.Clock
import Options.Applicative
import Data.List (sortBy)
import Text.Printf (printf)
import Data.Function (on)
import Control.Monad (liftM, when)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy as BS

data Verbosity = Verbose | Quiet deriving (Show, Eq)
data Entity = GetUser | GetOrg deriving (Show, Eq)

data MetaInfo a = MetaInfo { metaEntity :: String
                           , metaRepo :: String
                           , metaData :: a
                           } deriving (Show)

data Options = Options { optQuiet :: Verbosity
                       , optEntity :: Entity
                       , optAuthKey :: Maybe GithubAuth
                       , optOutFile :: FilePath
                       , optName :: String
                       } deriving (Show)

data Results = Results UTCTime String [MetaInfo Contributor] [MetaInfo Issue] deriving (Show)

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

instance ToJSON Results where
    toJSON (Results time org contribs issues) =
        object ["time" .= time,"name" .= org, "contributors" .= contribs, "issues" .= issues]

userUrl :: String -> String
userUrl = printf "https://github.com/%s/"

labelHtmlUrl :: String -> String -> String -> String
labelHtmlUrl = printf "https://github.com/%s/%s/labels/%s"

eitherIO :: IO (Either e a) -> IO a
eitherIO = eitherIOCnt 1

eitherIOCnt :: Int -> IO (Either e a) -> IO a
eitherIOCnt cnt io
    | attempts < cnt = hPutStrLn stderr (printf "Gave up after %d attempts!" attempts) >> exitFailure
    | otherwise = do res <- io
                     case res of
                          (Right a) -> return a
                          (Left _) -> do hPutStrLn stderr (printf "Retrying request! (%d/%d)" cnt attempts)
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

getInfo :: (String -> IO (Either e [a])) -> ([[MetaInfo a]] -> r) -> [Repo] -> IO r
getInfo get cleanup = liftM cleanup . mapM callGet
    where callGet Repo{..} = liftM (map (MetaInfo (githubOwnerLogin repoOwner) repoName)) . eitherIO $ get repoName

parseOpt :: Parser Options
parseOpt = Options <$> flag Verbose Quiet (long "quiet" <> short 'q' <> help "Enable quiet mode")
                   <*> flag GetOrg GetUser (long "user" <> short 'u' <> help "Get user data")
                   <*> optional (GithubOAuth <$> strOption (long "key" <> short 'a' <> metavar "OAUTHKEY" <> help "Github OAuth key"))
                   <*> strOption (long "file" <> short 'f' <> metavar "FILE" <> help "Output file name" <> value "data.json" <> showDefault)
                   <*> strArgument (metavar "ENTITY" <> help "Entity (organization/user) name")

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> parseOpt)
                  (fullDesc <> header "contributor-list - Get issues and cotributors of an Github entity")

main :: IO ()
main = do
    opt@Options{..} <- execParser parserInfo
    let verbose = when (optQuiet == Verbose)
    let blankLine = verbose $ putStr "\n"
    now <- getCurrentTime
    verbose $ printf "Generating data for '%s'\n" optName
    verbose $ putStrLn "Getting repo list.."
    repos <- eitherIO $ if optEntity == GetOrg
                           then organizationRepos' optAuthKey optName
                           else userRepos' optAuthKey optName Public
    blankLine
    issues <- getInfo (getIssues opt) concat repos
    contribs <- getInfo (getContribs opt) (sortWith metaData . nubContributors . concat) repos
    blankLine
    verbose $ printf "Saving data to '%s'\n" optOutFile
    BS.writeFile optOutFile . encode $ Results now optName contribs issues
    verbose $ printf "Found %d issues and %d contributors\n" (length issues) (length contribs)
    finish <- getCurrentTime
    verbose $ printf "Done! Took %s\n" . show $ diffUTCTime finish now
    where getContribs Options{..} name = do when (optQuiet == Verbose) $ printf "Contributors to %s..\n" name
                                            contributors' optAuthKey optName name
          getIssues Options{..} name = do when (optQuiet == Verbose) $ printf "Open issues of %s..\n" name
                                          issuesForRepo' optAuthKey optName name issueLimitations
          issueLimitations = [Open]
