module Propellor.DotDir where

import Propellor.Message
import Propellor.Bootstrap
import Propellor.Git
import Propellor.Gpg
import Utility.UserInfo
import Utility.Monad
import Utility.Process
import Utility.SafeCommand
import Utility.Exception
import Utility.Path

import Data.Char
import Data.List
import Control.Monad
import Control.Monad.IfElse
import System.Directory
import System.FilePath
import System.Posix.Directory
import System.IO
import Control.Applicative
import Prelude

distdir :: FilePath
distdir = "/usr/src/propellor"

-- A distribution may include a bundle of propellor's git repository here.
-- If not, it will be pulled from the network when needed.
distrepo :: FilePath
distrepo = distdir </> "propellor.git"

-- File containing the head rev of the distrepo.
disthead :: FilePath
disthead = distdir </> "head"

upstreambranch :: String
upstreambranch = "upstream/master"

-- Using the github mirror of the main propellor repo because
-- it is accessible over https for better security.
netrepo :: String
netrepo = "https://github.com/joeyh/propellor.git"

dotPropellor :: IO FilePath
dotPropellor = do
	home <- myHomeDir
	return (home </> ".propellor")

interactiveInit :: IO ()
interactiveInit = ifM (doesDirectoryExist =<< dotPropellor)
	( error "~/.propellor/ already exists, not doing anything"
	, do
		welcomeBanner
		setup
	)

welcomeBanner :: IO ()
welcomeBanner = putStr $ unlines $ map prettify
	[ ""
	, ""
	, "                                 _         ______`|                       ,-.__"
	, " .---------------------------  /   ~___-=O`/|O`/__|                      (____.'"
	, "  - Welcome to              -- ~          / | /    )          _.-'-._"
	, "  -            Propellor!   --  `/-==__ _/__|/__=-|          (       ~_"
	, " `---------------------------   *             ~ | |           '--------'"
	, "                                           (o)  `"
	, ""
	, ""
	]
  where
	prettify = map (replace '~' '\\')
	replace x y c
		| c == x = y
		| otherwise = c

prompt :: String -> [(String, IO ())] -> IO ()
prompt p cs = do
	putStr (p ++ " [" ++ intercalate "|" (map fst cs) ++ "] ")
	hFlush stdout
	r <- map toLower <$> getLine
	if null r
		then snd (head cs) -- default to first choice on return
		else case filter (\(s, _) -> map toLower s == r) cs of
			[(_, a)] -> a
			_ -> do
				putStrLn "Not a valid choice, try again.. (Or ctrl-c to quit)"
				prompt p cs

section :: IO ()
section = do
	putStrLn ""
	putStrLn "---------------------------------------------------------------------------------"
	putStrLn ""

setup :: IO ()
setup = do
	dotpropellor <- dotPropellor
	putStrLn "Propellor's configuration file is ~/.propellor/config.hs"
	putStrLn ""
	putStrLn "Lets get you started with a simple config that you can adapt"
	putStrLn "to your needs. You can start with:"
	putStrLn "   A: A clone of propellor's git repository    (most flexible)"
	putStrLn "   B: The bare minimum files to use propellor  (most simple)"
	prompt "Which would you prefer?"
		[ ("A", fullClone)
		, ("B", minimalConfig)
		]
	putStrLn "Ok, ~/.propellor/config.hs is set up!"
	changeWorkingDirectory dotpropellor

	section
	putStrLn "Let's try building the propellor configuration, to make sure it will work..."
	buildPropellor Nothing
	putStrLn "Great! Propellor is bootstrapped."
	
	section
	putStrLn "Propellor uses gpg to encrypt private data about the systems it manages,"
	putStrLn "and to sign git commits."
	gpg <- getGpgBin
	ifM (inPath gpg)
		( setupGpgKey
		, do
			putStrLn "You don't seem to have gpg installed, so skipping setting it up."
			explainManualSetupGpgKey
		)

	section
	putStrLn "Everything is set up ..."
	putStrLn "Your next step is to edit ~/.propellor/config.hs,"
	putStrLn "and run propellor again to try it out."
	putStrLn ""
	putStrLn "For docs, see https://propellor.branchable.com/"
	putStrLn "Enjoy propellor!"

explainManualSetupGpgKey :: IO ()
explainManualSetupGpgKey = do
	putStrLn "Propellor can still be used without gpg, but it won't be able to"
	putStrLn "manage private data. You can set this up later:"
	putStrLn " 1. gpg --gen-key"
	putStrLn " 2. propellor --add-key (pass it the key ID generated in step 1)"

setupGpgKey :: IO ()
setupGpgKey = do
	ks <- listSecretKeys
	putStrLn ""
	case ks of
		[] -> makeGpgKey
		[(k, _)] -> propellorAddKey k
		_ -> do
			let nks = zip ks (map show ([1..] :: [Integer]))
			putStrLn "I see you have several gpg keys:"
			forM_ nks $ \((k, d), n) ->
				putStrLn $ "   " ++ n ++ "   " ++ d ++ "  (keyid " ++ k ++ ")"
			prompt "Which of your gpg keys should propellor use?"
				(map (\((k, _), n) -> (n, propellorAddKey k)) nks)

makeGpgKey :: IO ()
makeGpgKey = do
	putStrLn "You seem to not have any gpg secret keys."
	prompt "Would you like to create one now?"
		[("Y", rungpg), ("N", nope)]
  where
	nope = do
		putStrLn "No problem."
		explainManualSetupGpgKey
	rungpg = do
		putStrLn "Running gpg --gen-key ..."
		gpg <- getGpgBin
		void $ boolSystem gpg [Param "--gen-key"]
		ks <- listSecretKeys
		case ks of
			[] -> do
				putStrLn "Hmm, gpg seemed to not set up a secret key."
				prompt "Want to try running gpg again?"
					[("Y", rungpg), ("N", nope)]
			((k, _):_) -> propellorAddKey k

propellorAddKey :: String -> IO ()
propellorAddKey keyid = do
	putStrLn ""
	putStrLn $ "Telling propellor to use your gpg key by running: propellor --add-key " ++ keyid
	d <- dotPropellor
	unlessM (boolSystem (d </> "propellor") [Param "--add-key", Param keyid]) $ do
		putStrLn "Oops, that didn't work! You can retry the same command later."
		putStrLn "Continuing onward ..."

minimalConfig :: IO ()
minimalConfig = do
	d <- dotPropellor
	createDirectoryIfMissing True d
	let cabalfile = d </> "config.cabal"
	let configfile = d </> "config.hs"
	writeFile cabalfile (unlines cabalcontent)
	writeFile configfile (unlines configcontent)
	changeWorkingDirectory d
	void $ boolSystem "git" [Param "init"]
	void $ boolSystem "git" [Param "add" , File cabalfile, File configfile]
  where
	cabalcontent =
		[ "-- This is a cabal file to use to build your propellor configuration."
		, ""
		, "Name: config"
		, "Cabal-Version: >= 1.6"
		, "Build-Type: Simple"
		, "Version: 0"
		, ""
		, "Executable propellor-config"
		, "  Main-Is: config.hs"
		, "  GHC-Options: -threaded -Wall -fno-warn-tabs -O0"
		, "  Extensions: TypeOperators"
		, "  Build-Depends: propellor >= 3.0, base >= 3"
		]
	configcontent = 
		[ "-- This is the main configuration file for Propellor, and is used to build"
		, "-- the propellor program."
		, ""
		, "import Propellor"
		, "import qualified Propellor.Property.File as File"
		, "import qualified Propellor.Property.Apt as Apt"
		, "import qualified Propellor.Property.Cron as Cron"
		, "import qualified Propellor.Property.User as User"
		, ""
		, "main :: IO ()"
		, "main = defaultMain hosts"
		, ""
		, "-- The hosts propellor knows about."
		, "hosts :: [Host]"
		, "hosts ="
		, "        [ mybox"
		, "        ]"
		, ""
		, "-- An example host."
		, "mybox :: Host"
		, "mybox = host \"mybox.example.com\" $ props"
		, "        & osDebian Unstable \"amd64\""
		, "        & Apt.stdSourcesList"
		, "        & Apt.unattendedUpgrades"
		, "        & Apt.installed [\"etckeeper\"]"
		, "        & Apt.installed [\"ssh\"]"
		, "        & User.hasSomePassword (User \"root\")"
		, "        & File.dirExists \"/var/www\""
		, "        & Cron.runPropellor (Cron.Times \"30 * * * *\")"
		, ""
		]

fullClone :: IO ()
fullClone = do
	d <- dotPropellor
	ifM (doesFileExist distrepo <||> doesDirectoryExist distrepo)
		( do			
			void $ boolSystem "git" [Param "clone", File distrepo, File d]
			fetchUpstreamBranch distrepo
			changeWorkingDirectory d
			void $ boolSystem "git" [Param "remote", Param "rm", Param "origin"]
		, do
			void $ boolSystem "git" [Param "clone", Param netrepo, File d]
			changeWorkingDirectory d
			-- Rename origin to upstream and avoid
			-- git push to that read-only repo.
			void $ boolSystem "git" [Param "remote", Param "rename", Param "origin", Param "upstream"]
			void $ boolSystem "git" [Param "config", Param "--unset", Param "branch.master.remote", Param "upstream"]
		)

fetchUpstreamBranch :: FilePath -> IO ()
fetchUpstreamBranch repo = do
	changeWorkingDirectory =<< dotPropellor
	void $ boolSystem "git"
		[ Param "fetch"
		, File repo
		, Param ("+refs/heads/master:refs/remotes/" ++ upstreambranch)
		, Param "--quiet"
		]

checkRepoUpToDate :: IO ()
checkRepoUpToDate = whenM (gitbundleavail <&&> dotpropellorpopulated) $ do
	headrev <- takeWhile (/= '\n') <$> readFile disthead
	changeWorkingDirectory =<< dotPropellor
	headknown <- catchMaybeIO $ 
		withQuietOutput createProcessSuccess $
			proc "git" ["log", headrev]
	if (headknown == Nothing)
		then setupUpstreamMaster headrev
		else do
			theirhead <- getCurrentGitSha1 =<< getCurrentBranchRef
			when (theirhead /= headrev) $ do
				merged <- not . null <$>
					readProcess "git" ["log", headrev ++ "..HEAD", "--ancestry-path"]
				unless merged $
					warnoutofdate True
  where
	gitbundleavail = doesFileExist disthead
	dotpropellorpopulated = do
		d <- dotPropellor
		doesFileExist (d </> "propellor.cabal")

-- Passed the user's dotpropellor repository, makes upstream/master
-- be a usefully mergeable branch.
--
-- We cannot just use origin/master, because in the case of a distrepo,
-- it only contains 1 commit. So, trying to merge with it will result
-- in lots of merge conflicts, since git cannot find a common parent
-- commit.
--
-- Instead, the upstream/master branch is created by taking the
-- upstream/master branch (which must be an old version of propellor,
-- as distributed), and diffing from it to the current origin/master,
-- and committing the result. This is done in a temporary clone of the
-- repository, giving it a new master branch. That new branch is fetched
-- into the user's repository, as if fetching from a upstream remote,
-- yielding a new upstream/master branch.
setupUpstreamMaster :: String -> IO ()
setupUpstreamMaster newref = do
	changeWorkingDirectory =<< dotPropellor
	go =<< catchMaybeIO getoldrev
  where
	go Nothing = warnoutofdate False
	go (Just oldref) = do
		let tmprepo = ".git/propellordisttmp"
		let cleantmprepo = void $ catchMaybeIO $ removeDirectoryRecursive tmprepo
		cleantmprepo
		git ["clone", "--quiet", ".", tmprepo]
	
		changeWorkingDirectory tmprepo
		git ["fetch", distrepo, "--quiet"]
		git ["reset", "--hard", oldref, "--quiet"]
		git ["merge", newref, "-s", "recursive", "-Xtheirs", "--quiet", "-m", "merging upstream version"]
	
		fetchUpstreamBranch tmprepo
		cleantmprepo
		warnoutofdate True

	getoldrev = takeWhile (/= '\n')
		<$> readProcess "git" ["show-ref", upstreambranch, "--hash"]
	
	git = run "git"
	run cmd ps = unlessM (boolSystem cmd (map Param ps)) $
		error $ "Failed to run " ++ cmd ++ " " ++ show ps

warnoutofdate :: Bool -> IO ()
warnoutofdate havebranch = do
	warningMessage ("** Your ~/.propellor/ is out of date..")
	let also s = hPutStrLn stderr ("   " ++ s)
	also ("A newer upstream version is available in " ++ distrepo)
	if havebranch
		then also ("To merge it, run: git merge " ++ upstreambranch)
		else also ("To merge it, find the most recent commit in your repository's history that corresponds to an upstream release of propellor, and set refs/remotes/" ++ upstreambranch ++ " to it. Then run propellor again.")
	also ""
