{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Github.Repos
import Github.Issues
import Github.Auth
import Data.Aeson
import Data.Time.Clock
import Data.List (sort)
import Text.Printf (printf)
import Control.Monad (liftM)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy as BS

data Results = Results UTCTime String [Contributor] [Issue] deriving (Show)

instance ToJSON Contributor where
    toJSON (KnownContributor ccnt avatar username _ _ _) =
        object ["avatar" .= avatar, "login" .= username, "count" .= ccnt, "url" .= userUrl username ]
    toJSON (AnonymousContributor ccnt username) =
        object ["avatar" .= anonUrl, "login" .= username, "count" .= ccnt, "url" .= userUrl username]
        where anonUrl = "#" :: String

instance ToJSON Issue where
    toJSON Issue{..} =
        object [ "url" .= issueHtmlUrl
               , "number" .= issueNumber
               , "title" .= issueTitle
               , "created" .= fromGithubDate issueCreatedAt]

instance ToJSON Results where
    toJSON (Results time org contribs issues) =
        object ["time" .= time,"name" .= org, "contributors" .= contribs, "issues" .= issues] --FIXME Opening user

userUrl :: String -> String
userUrl = printf "https://github.com/%s/"

githubAuth :: Maybe GithubAuth
githubAuth = Just $ GithubOAuth "d9e6ce7ba9378a580e564e9dd4ecd3b4e1eb20b8"

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

updateContributions :: Contributor -> Int -> Contributor
updateContributions (KnownContributor _ a b c d e) new = KnownContributor new a b c d e
updateContributions (AnonymousContributor _ a) new = AnonymousContributor new a

nubContributors :: [Contributor] -> [Contributor]
nubContributors [] = []
nubContributors cs@(c:cr) = updateContributions c cnts : nubContributors (filter ((name /=) . getName) cr)
    where name = getName c
          cnts = foldl (\rc u -> if getName u == name
                                    then rc + getContributions u
                                    else rc) 0 cs

blankLine :: IO ()
blankLine = putStrLn ""

getInfo :: (String -> IO (Either e b)) -> ([b] -> r) -> [Repo] -> IO r
getInfo get cleanup = liftM cleanup . mapM (eitherIO . get . repoName)

main :: IO ()
main = do
    now <- getCurrentTime
    printf "Generating data for '%s'\n" organization
    repos <- eitherIO $ putStrLn "Getting repo list.." >> organizationRepos' githubAuth organization
    blankLine
    issues <- getInfo getIssues concat repos
    contribs <- getInfo getContribs (sort . nubContributors . concat) repos
    blankLine
    printf "Saving data to '%s'\n" saveFileName
    BS.writeFile saveFileName . encode $ Results now organization contribs issues
    printf "Found %d issues and %d contributors\n" (length issues) (length contribs)
    finish <- getCurrentTime
    printf "Done! Took %s\n" . show $ diffUTCTime finish now
    where getContribs name = do printf "Contributors to %s..\n" name
                                contributors' githubAuth organization name
          getIssues name = do printf "Open issues of %s..\n" name
                              issuesForRepo' githubAuth organization name issueLimitations
          organization = "evercam"
          saveFileName = "saved.json"
          issueLimitations = []
