{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Github.Repos
import Github.Auth
import Data.Aeson
import Control.Monad.Trans.Either
import Control.Monad (liftM)
import Text.Printf (printf)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.List (sort)
import System.Exit (exitFailure)
import qualified Data.ByteString.Lazy as BS

data Results = Results UTCTime String [Contributor] deriving (Show)

instance ToJSON Contributor where
    toJSON (KnownContributor ccnt avatar username _ _ _) = object ["avatar" .= avatar, "login" .= username, "count" .= ccnt ]
    toJSON (AnonymousContributor ccnt username) = object ["avatar" .= anonUrl, "login" .= username, "count" .= ccnt ]
        where anonUrl = "#" :: String

instance ToJSON Results where
    toJSON (Results time org contribs) = object ["time" .= time, "name" .= org, "contributors" .= contribs]

githubAuth :: Maybe GithubAuth
githubAuth = Just $ GithubOAuth "d9e6ce7ba9378a580e564e9dd4ecd3b4e1eb20b8"

eitherIO :: Show e => Either e a -> IO a
eitherIO (Left e) = print e >> exitFailure
eitherIO (Right a) = return a

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

main :: IO ()
main = do
    now <- getCurrentTime
    putStrLn "Getting repo list.."
    out <- liftM (Results now organization) $ runEitherT names >>= eitherIO
    printf "Saving data to '%s'\n" saveFileName
    BS.writeFile saveFileName $ encode out
    putStrLn "Done."
    where repos = EitherT $ organizationRepos' githubAuth organization
          names = liftM (sort . nubContributors . concat) $ repos >>= mapM (EitherT . getContribs)
          organization = "evercam"
          saveFileName = "saved.json"
          getContribs repo = let name = repoName repo
                             in printf "Checking %s..\n" name >> contributors' githubAuth organization name
