module Propellor.Git where

import Utility.Process
import Utility.Exception
import Utility.Directory
import Utility.Misc
import Utility.PartialPrelude

import Data.Maybe
import Control.Applicative
import Prelude

getCurrentBranch :: IO String
getCurrentBranch = takeWhile (/= '\n')
	<$> readProcess "git" ["symbolic-ref", "--short", "HEAD"]

getCurrentBranchRef :: IO String
getCurrentBranchRef = takeWhile (/= '\n')
	<$> readProcess "git" ["symbolic-ref", "HEAD"]

getCurrentGitSha1 :: String -> IO String
getCurrentGitSha1 branchref = takeWhile (/= '\n')
	<$> readProcess "git" ["show-ref", "--hash", branchref]

setRepoUrl :: String -> IO ()
setRepoUrl "" = return ()
setRepoUrl url = do
	subcmd <- ifM hasOrigin (pure "set-url", pure "add")
	void $ boolSystem "git" [Param "remote", Param subcmd, Param "origin", Param url]
	-- same as --set-upstream-to, except origin branch
	-- may not have been pulled yet
	branch <- getCurrentBranch
	let branchval s = "branch." ++ branch ++ "." ++ s
	void $ boolSystem "git" [Param "config", Param (branchval "remote"), Param "origin"]
	void $ boolSystem "git" [Param "config", Param (branchval "merge"), Param $ "refs/heads/"++branch]

-- `git config --bool propellor.blah` outputs "false" if propellor.blah is unset
-- i.e. the git convention is that the default value of any git-config setting
-- is "false".  So we don't need a Maybe Bool here.
getGitConfigBool :: String -> IO Bool
getGitConfigBool key = do
	value <- catchMaybeIO $
		takeWhile (/= '\n')
			<$> readProcess "git" ["config", "--bool", key]
	return $ case value of
		Just "true" -> True
		_ -> False

getRepoUrl :: IO (Maybe String)
getRepoUrl = getM getGitConfigValue urls
  where
	urls = ["remote.deploy.url", "remote.origin.url"]

hasOrigin :: IO Bool
hasOrigin = catchDefaultIO False $ do
	rs <- lines <$> readProcess "git" ["remote"]
	return $ "origin" `elem` rs

hasGitRepo :: IO Bool
hasGitRepo = doesFileExist ".git/HEAD"

type Version = [Int]

	void $ actionMessage "Pull from central git repository" $
		boolSystem "git" [Param "fetch"]

	oldsha <- getCurrentGitSha1 branchref

	whenM (doesFileExist keyring) $
		ifM (verifyOriginBranch originbranch)
			( do
				putStrLn $ "git branch " ++ originbranch ++ " gpg signature verified; merging"
				hFlush stdout
				void $ boolSystem "git" [Param "merge", Param originbranch]
			, warningMessage $ "git branch " ++ originbranch ++ " is not signed with a trusted gpg key; refusing to deploy it! (Running with previous configuration instead.)"
			)

	newsha <- getCurrentGitSha1 branchref
	return $ oldsha /= newsha
