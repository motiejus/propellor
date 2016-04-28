-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.Attic
	( installed
	, repoExists
	, init
	, restored
	, backup
	, KeepPolicy (..)
	) where

import Propellor.Base hiding (init)
import Prelude hiding (init)
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import Data.List (intercalate)

type AtticParam = String

type AtticRepo = FilePath

installed :: Property DebianLike
installed = Apt.installed ["attic"]

repoExists :: AtticRepo -> IO Bool
repoExists repo = boolSystem "attic" [Param "list", File repo]

init :: AtticRepo -> Property DebianLike
init backupdir = check (not <$> repoExists backupdir) (cmdProperty "attic" initargs)
	`requires` installed
  where
	initargs =
		[ "init"
		, backupdir
		]

-- TODO: use restored from Obnam
restored :: FilePath -> AtticRepo -> Property DebianLike
restored dir backupdir = cmdProperty "attic" restoreargs
	`assume` MadeChange
	`describe` ("attic restore from " ++ backupdir)
	`requires` installed
  where
	restoreargs =
		[ "extract"
		, backupdir
		, dir
		]

backup :: FilePath -> AtticRepo -> Cron.Times -> [AtticParam] -> [KeepPolicy] -> Property DebianLike
backup dir backupdir crontimes extraargs kp = backup' dir backupdir crontimes extraargs kp
	`requires` restored dir backupdir

backup' :: FilePath -> AtticRepo -> Cron.Times -> [AtticParam] -> [KeepPolicy] -> Property DebianLike
backup' dir backupdir crontimes extraargs kp = cronjob
	`describe` desc
	`requires` installed
  where
	desc = backupdir ++ " attic backup"
	cronjob = Cron.niceJob ("attic_backup" ++ dir) crontimes (User "root") "/" $
		"flock " ++ shellEscape lockfile ++ " sh -c " ++ backupcmd
	lockfile = "/var/lock/propellor-attic.lock"
	backupcmd = intercalate ";" $
		createCommand
		: if null kp then [] else [pruneCommand]
	createCommand = unwords $
		[ "attic"
		, "create"
		, "--stats"
		]
		++ map shellEscape extraargs ++
		[ shellEscape backupdir ++ "::" ++ "$(date --iso-8601=ns --utc)"
		, shellEscape dir
		]
	pruneCommand = unwords $
		[ "attic"
		, "prune"
		, shellEscape backupdir
		]
		++
		map keepParam kp

keepParam :: KeepPolicy -> AtticParam
keepParam (KeepHours n) = "--keep-hourly=" ++ show n
keepParam (KeepDays n) = "--keep-daily=" ++ show n
keepParam (KeepWeeks n) = "--keep-daily=" ++ show n
keepParam (KeepMonths n) = "--keep-monthly=" ++ show n
keepParam (KeepYears n) = "--keep-yearly=" ++ show n

data KeepPolicy
	= KeepHours Int
	| KeepDays Int
	| KeepWeeks Int
	| KeepMonths Int
	| KeepYears Int
