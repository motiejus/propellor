{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

-- | Properties in this module ensure that things are currently mounted,
-- but without making the mount persistent. Use `Propellor.Property.Fstab`
-- to configure persistent mounts.

module Propellor.Property.Mount where

import Propellor.Base
import qualified Propellor.Property.File as File
import Utility.Path

import Data.Char
import Data.List
import Utility.Table

type FsType = String -- ^ type of filesystem to mount ("auto" to autodetect)

-- | A device or other thing to be mounted.
type Source = String

type MountPoint = FilePath

-- | Mounts a device.
mounted :: FsType -> Source -> MountPoint -> Property NoInfo
mounted fs src mnt = property (mnt ++ " mounted") $ 
	toResult <$> liftIO (mount fs src mnt)

-- | Bind mounts the first directory so its contents also appear
-- in the second directory.
bindMount :: FilePath -> FilePath -> Property NoInfo
bindMount src dest = cmdProperty "mount" ["--bind", src, dest]
	`describe` ("bind mounted " ++ src ++ " to " ++ dest)

mount :: FsType -> Source -> MountPoint -> IO Bool
mount fs src mnt = boolSystem "mount" [Param "-t", Param fs, Param src, Param mnt]

newtype SwapPartition = SwapPartition FilePath

-- | Replaces /etc/fstab with a file that should cause the currently
-- mounted partitions to be re-mounted the same way on boot.
--
-- For each specified MountPoint, the UUID of each partition
-- (or if there is no UUID, its label), its filesystem type,
-- and its mount options are all automatically probed.
--
-- The SwapPartitions are also included in the generated fstab.
fstabbed :: [MountPoint] -> [SwapPartition] -> Property NoInfo
fstabbed mnts swaps = property "fstabbed" $ do
	fstab <- liftIO $ genFstab mnts swaps id
	ensureProperty $ 
		"/etc/fstab" `File.hasContent` fstab

genFstab :: [MountPoint] -> [SwapPartition] -> (MountPoint -> MountPoint) -> IO [String]
genFstab mnts swaps mnttransform = do
	fstab <- liftIO $ mapM getcfg (sort mnts)
	swapfstab <- liftIO $ mapM getswapcfg swaps
	return $ header ++ formatTable (legend : fstab ++ swapfstab)
  where
	header =
		[ "# /etc/fstab: static file system information. See fstab(5)"
		, "# "
		]
	legend = ["# <file system>", "<mount point>", "<type>", "<options>", "<dump>", "<pass>"]
	getcfg mnt = sequence
		[ fromMaybe (error "unable to find mount source")
			<$> getM (\a -> a mnt)
				[ uuidprefix getMountUUID
				, sourceprefix getMountLabel
				, getMountSource
				]
		, pure (mnttransform mnt)
		, fromMaybe "auto" <$> getFsType mnt
		, fromMaybe "defaults" <$> getFsOptions mnt
		, pure "0"
		, pure (if mnt == "/" then "1" else "2")
		]
	getswapcfg (SwapPartition swap) = sequence
		[ fromMaybe swap <$> getM (\a -> a swap)
			[ uuidprefix getSourceUUID
			, sourceprefix getSourceLabel
			]
		, pure "none"
		, pure "swap"
		, pure "defaults"
		, pure "0"
		, pure "0"
		]
	prefix s getter m = fmap (s ++) <$> getter m
	uuidprefix = prefix "UUID="
	sourceprefix = prefix "LABEL="

-- | Checks if /etc/fstab is not configured. This is the case if it doesn't
-- exist, or consists entirely of blank lines or comments.
--
-- So, if you want to only replace the fstab once, and then never touch it
-- again, allowing local modifications:
--
-- > check noFstab (fstabbed mnts [])
noFstab :: IO Bool
noFstab = ifM (doesFileExist "/etc/fstab")
	( null . filter iscfg . lines <$> readFile "/etc/fstab"
	, return True
	)
  where
	iscfg l
		| null l = False
		| otherwise = not $ "#" `isPrefixOf` dropWhile isSpace l
	
-- | Lists all mount points of the system.
mountPoints :: IO [MountPoint]
mountPoints = lines <$> readProcess "findmnt" ["-rn", "--output", "target"]

-- | Finds all filesystems mounted inside the specified directory.
mountPointsBelow :: FilePath -> IO [MountPoint]
mountPointsBelow target = filter (\p -> simplifyPath p /= simplifyPath target)
	. filter (dirContains target)
	<$> mountPoints

-- | Filesystem type mounted at a given location.
getFsType :: MountPoint -> IO (Maybe FsType)
getFsType = findmntField "fstype"

-- | Mount options for the filesystem mounted at a given location.
getFsOptions :: MountPoint -> IO (Maybe String)
getFsOptions = findmntField "fs-options"

type UUID = String

-- | UUID of filesystem mounted at a given location.
getMountUUID :: MountPoint -> IO (Maybe UUID)
getMountUUID = findmntField "uuid"

-- | UUID of a device
getSourceUUID :: Source -> IO (Maybe UUID)
getSourceUUID = blkidTag "UUID"

type Label = String

-- | Label of filesystem mounted at a given location.
getMountLabel :: MountPoint -> IO (Maybe Label)
getMountLabel = findmntField "label"

-- | Label of a device
getSourceLabel :: Source -> IO (Maybe UUID)
getSourceLabel = blkidTag "LABEL"

-- | Device mounted at a given location.
getMountSource :: MountPoint -> IO (Maybe Source)
getMountSource = findmntField "source"

findmntField :: String -> FilePath -> IO (Maybe String)
findmntField field mnt = catchDefaultIO Nothing $
	headMaybe . filter (not . null) . lines
		<$> readProcess "findmnt" ["-n", mnt, "--output", field]

blkidTag :: String -> Source -> IO (Maybe String)
blkidTag tag dev = catchDefaultIO Nothing $
	headMaybe . filter (not . null) . lines
		<$> readProcess "blkid" [dev, "-s", tag]

-- | Unmounts a device or mountpoint,
-- lazily so any running processes don't block it.
umountLazy :: FilePath -> IO ()
umountLazy mnt =  
	unlessM (boolSystem "umount" [ Param "-l", Param mnt ]) $
		stopPropellorMessage $ "failed unmounting " ++ mnt

-- | Unmounts anything mounted inside the specified directory.
unmountBelow :: FilePath -> IO ()
unmountBelow d = do
	submnts <- mountPointsBelow d
	forM_ submnts umountLazy
