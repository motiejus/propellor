-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.DebianMirror
	( DebianPriority (..)
	, showPriority
	, mirror
	, RsyncExtra (..)
	, Method (..)
	, DebianMirror
	, debianMirrorHostName
	, debianMirrorSuites
	, debianMirrorArchitectures
	, debianMirrorSections
	, debianMirrorSourceBool
	, debianMirrorPriorities
	, debianMirrorMethod
	, debianMirrorKeyring
	, debianMirrorRsyncExtra
	, mkDebianMirror
	) where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.User as User

import Data.List


data DebianPriority = Essential | Required | Important | Standard | Optional | Extra
	deriving (Show, Eq)

showPriority :: DebianPriority -> String
showPriority Essential = "essential"
showPriority Required  = "required"
showPriority Important = "important"
showPriority Standard  = "standard"
showPriority Optional  = "optional"
showPriority Extra     = "extra"

mirror :: HostName -> FilePath -> [DebianSuite] -> [Architecture] -> [Apt.Section] -> Bool -> [DebianPriority] -> Cron.Times -> Property NoInfo
mirror hn dir suites archs sections source priorities crontimes = propertyList
	("Debian mirror " ++ dir)
	[ Apt.installed ["debmirror"]
	, User.accountFor (User "debmirror")
	, File.dirExists dir
	, File.ownerGroup dir (User "debmirror") (Group "debmirror")
	, check (not . and <$> mapM suitemirrored suites) $ cmdProperty "debmirror" args
		`describe` "debmirror setup"
	, Cron.niceJob ("debmirror_" ++ dir) crontimes (User "debmirror") "/" $
		unwords ("/usr/bin/debmirror" : args)
	]
  where
	dir = _debianMirrorDir mirror'
	suites = _debianMirrorSuites mirror'
	suitemirrored suite = doesDirectoryExist $ dir </> "dists" </> Apt.showSuite suite
	architecturearg = intercalate ","
	suitearg = intercalate "," $ map Apt.showSuite suites
	priorityRegex pp = "(" ++ intercalate "|" (map showPriority pp) ++ ")"
	rsyncextraarg [] = "none"
	rsyncextraarg res = intercalate "," $ map showRsyncExtra res
	args =
		[ "--dist" , suitearg
		, "--arch", architecturearg $ map architectureToDebianArchString (_debianMirrorArchitectures mirror')
		, "--section", intercalate "," $ _debianMirrorSections mirror'
		, "--limit-priority", "\"" ++ priorityRegex (_debianMirrorPriorities mirror') ++ "\""
		]
		++
		(if _debianMirrorSourceBool mirror' then [] else ["--nosource"])
		++
		[ "--host", hn
		, "--method", "http"
		, "--keyring", "/usr/share/keyrings/debian-archive-keyring.gpg"
		, dir
		]

mirrorCdn :: FilePath -> [DebianSuite] -> [Architecture] -> [Apt.Section] -> Bool -> [DebianPriority] -> Cron.Times -> Property NoInfo
mirrorCdn = mirror "httpredir.debian.org"
