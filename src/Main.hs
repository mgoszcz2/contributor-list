import Control.Monad
import Github.Repos
import Github.Auth
import Control.Monad.Trans.Either
import Data.Aeson
import Data.List (nub, sort)
import System.Exit (exitFailure)
import System.Environment  (getArgs)

githubAuth :: Maybe GithubAuth
githubAuth = Just $ GithubOAuth "d9e6ce7ba9378a580e564e9dd4ecd3b4e1eb20b8"

main :: IO ()
main = do
    out <- runEitherT names
    case out of Right x -> mapM_ print x
                Left e -> print e >> exitFailure
    where organization = "evercam"
          repos = EitherT $ organizationRepos' githubAuth organization
          contribs = repos >>= mapM (EitherT . contributors' githubAuth organization . repoName)
          names = liftM (sort . nub .  concat) contribs
