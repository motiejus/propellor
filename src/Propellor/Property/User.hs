module Propellor.Property.User where

import System.Posix

import Propellor.Base
import qualified Propellor.Property.File as File

data Eep = YesReallyDeleteHome

accountFor :: User -> Property DebianLike
accountFor user@(User u) = tightenTargets $ check nohomedir go
	`describe` ("account for " ++ u)
  where
	nohomedir = isNothing <$> catchMaybeIO (homedir user)
	go = cmdProperty "adduser"
		[ "--disabled-password"
		, "--gecos", ""
		, u
		]

systemAccountFor :: User -> Property DebianLike
systemAccountFor user@(User u) = systemAccountFor' user Nothing (Just (Group u))

systemAccountFor' :: User -> Maybe FilePath -> Maybe Group -> Property DebianLike
systemAccountFor' (User u) mhome mgroup = tightenTargets $ check nouser go
	`describe` ("system account for " ++ u)
  where
	nouser = isNothing <$> catchMaybeIO (getUserEntryForName u)
	go = cmdProperty "adduser" $
		[ "--system" ]
		++
		"--home" : maybe
			["/nonexistent", "--no-create-home"]
			( \h -> [ h ] )
			mhome
		++
		maybe [] ( \(Group g) -> ["--ingroup", g] ) mgroup
		++
		[ "--shell", "/usr/bin/nologin"
		, "--disabled-login"
		, "--disabled-password"
		, u
		]

-- | Removes user home directory!! Use with caution.
nuked :: User -> Eep -> Property Linux
nuked user@(User u) _ = tightenTargets $ check hashomedir go
	`describe` ("nuked user " ++ u)
  where
	hashomedir = isJust <$> catchMaybeIO (homedir user)
	go = cmdProperty "userdel"
		[ "-r"
		, u
		]

-- | Only ensures that the user has some password set. It may or may
-- not be a password from the PrivData.
hasSomePassword :: User -> Property (HasInfo + DebianLike)
hasSomePassword user = hasSomePassword' user hostContext

-- | While hasSomePassword uses the name of the host as context,
-- this allows specifying a different context. This is useful when
-- you want to use the same password on multiple hosts, for example.
hasSomePassword' :: IsContext c => User -> c -> Property (HasInfo + DebianLike)
hasSomePassword' user context = check ((/= HasPassword) <$> getPasswordStatus user) $
	hasPassword' user context

-- | Ensures that a user's password is set to a password from the PrivData.
-- (Will change any existing password.)
--
-- A user's password can be stored in the PrivData in either of two forms;
-- the full cleartext <Password> or a <CryptPassword> hash. The latter
-- is obviously more secure.
hasPassword :: User -> Property (HasInfo + DebianLike)
hasPassword user = hasPassword' user hostContext

hasPassword' :: IsContext c => User -> c -> Property (HasInfo + DebianLike)
hasPassword' (User u) context = go
	`requires` shadowConfig True
  where
	go :: Property (HasInfo + UnixLike)
	go = withSomePrivData srcs context $
		property (u ++ " has password") . setPassword
	srcs =
		[ PrivDataSource (CryptPassword u)
			"a crypt(3)ed password, which can be generated by, for example: perl -e 'print crypt(shift, q{$6$}.shift)' 'somepassword' 'somesalt'"
		, PrivDataSource (Password u) ("a password for " ++ u)
		]

setPassword :: (((PrivDataField, PrivData) -> Propellor Result) -> Propellor Result) -> Propellor Result
setPassword getpassword = getpassword $ go
  where
	go (Password user, password) = chpasswd (User user) (privDataVal password) []
	go (CryptPassword user, hash) = chpasswd (User user) (privDataVal hash) ["--encrypted"]
	go (f, _) = error $ "Unexpected type of privdata: " ++ show f

-- | Makes a user's password be the passed String. Highly insecure:
-- The password is right there in your config file for anyone to see!
hasInsecurePassword :: User -> String -> Property NoInfo
hasInsecurePassword u@(User n) p = property (n ++ " has insecure password") $
	chpasswd u p []

chpasswd :: User -> String -> [String] -> Propellor Result
chpasswd (User user) v ps = makeChange $ withHandle StdinHandle createProcessSuccess
	(proc "chpasswd" ps) $ \h -> do
		hPutStrLn h $ user ++ ":" ++ v
		hClose h

lockedPassword :: User -> Property NoInfo
lockedPassword user@(User u) = check (not <$> isLockedPassword user) $ cmdProperty "passwd"
	[ "--lock"
	, u
	]
	`describe` ("locked " ++ u ++ " password")

data PasswordStatus = NoPassword | LockedPassword | HasPassword
	deriving (Eq)

getPasswordStatus :: User -> IO PasswordStatus
getPasswordStatus (User u) = parse . words <$> readProcess "passwd" ["-S", u]
  where
	parse (_:"L":_) = LockedPassword
	parse (_:"NP":_) = NoPassword
	parse (_:"P":_) = HasPassword
	parse _ = NoPassword

isLockedPassword :: User -> IO Bool
isLockedPassword user = (== LockedPassword) <$> getPasswordStatus user

homedir :: User -> IO FilePath
homedir (User user) = homeDirectory <$> getUserEntryForName user

hasGroup :: User -> Group -> Property DebianLike
hasGroup (User user) (Group group') = tightenTargets $ check test go
	`describe` unwords ["user", user, "in group", group']
  where
	test = not . elem group' . words <$> readProcess "groups" [user]
	go = cmdProperty "adduser"
		[ user
		, group'
		]

-- | Gives a user access to the secondary groups, including audio and
-- video, that the OS installer normally gives a desktop user access to.
--
-- Note that some groups may only exit after installation of other
-- software. When a group does not exist yet, the user won't be added to it.
hasDesktopGroups :: User -> Property DebianLike
hasDesktopGroups user@(User u) = property' desc $ \o -> do
	existinggroups <- map (fst . break (== ':')) . lines
		<$> liftIO (readFile "/etc/group")
	let toadd = filter (`elem` existinggroups) desktopgroups
	ensureProperty o $ propertyList desc $ toProps $
		map (hasGroup user . Group) toadd
  where
	desc = "user " ++ u ++ " is in standard desktop groups"
	-- This list comes from user-setup's debconf
	-- template named "passwd/user-default-groups"
	desktopgroups =
		[ "audio"
		, "cdrom"
		, "dip"
		, "floppy"
		, "video"
		, "plugdev"
		, "netdev"
		, "scanner"
		, "bluetooth"
		, "debian-tor"
		, "lpadmin"
		]

-- | Controls whether shadow passwords are enabled or not.
shadowConfig :: Bool -> Property DebianLike
shadowConfig True = tightenTargets $ check (not <$> shadowExists)
	(cmdProperty "shadowconfig" ["on"])
		`describe` "shadow passwords enabled"
shadowConfig False = tightenTargets $ check shadowExists
	(cmdProperty "shadowconfig" ["off"])
		`describe` "shadow passwords disabled"

shadowExists :: IO Bool
shadowExists = doesFileExist "/etc/shadow"

-- | Ensures that a user has a specified login shell, and that the shell
-- is enabled in /etc/shells.
hasLoginShell :: User -> FilePath -> Property DebianLike
hasLoginShell user loginshell = shellSetTo user loginshell `requires` shellEnabled loginshell

shellSetTo :: User -> FilePath -> Property DebianLike
shellSetTo (User u) loginshell = tightenTargets $ check needchangeshell
	(cmdProperty "chsh" ["--shell", loginshell, u])
		`describe` (u ++ " has login shell " ++ loginshell)
  where
	needchangeshell = do
		currshell <- userShell <$> getUserEntryForName u
		return (currshell /= loginshell)

-- | Ensures that /etc/shells contains a shell.
shellEnabled :: FilePath -> Property DebianLike
shellEnabled loginshell = tightenTargets $
	"/etc/shells" `File.containsLine` loginshell
