{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Types.OS (
	System(..),
	Distribution(..),
	TargetOS(..),
	DebianSuite(..),
	FreeBSDRelease(..),
	FBSDVersion(..),
	isStable,
	Release,
	Architecture(..),
	architectureToDebianArchString,
	HostName,
	UserName,
	User(..),
	Group(..),
	userGroup,
	Port(..),
	fromPort,
	systemToTargetOS,
) where

import Propellor.Types.ConfigurableValue

import Network.BSD (HostName)
import Data.Typeable
import Data.String

-- | High level description of a operating system.
data System = System Distribution Architecture
	deriving (Show, Eq, Typeable)

data Distribution
	= Debian DebianSuite
	| Buntish Release -- ^ A well-known Debian derivative founded by a space tourist. The actual name of this distribution is not used in Propellor per <http://joeyh.name/blog/entry/trademark_nonsense/>
	| FreeBSD FreeBSDRelease
	deriving (Show, Eq)

-- | Properties can target one or more OS's; the targets are part
-- of the type of the property, so need to be kept fairly simple.
data TargetOS
	= OSDebian
	| OSBuntish
	| OSFreeBSD
	deriving (Show, Eq, Ord)

systemToTargetOS :: System -> TargetOS
systemToTargetOS (System (Debian _) _) = OSDebian
systemToTargetOS (System (Buntish _) _) = OSBuntish
systemToTargetOS (System (FreeBSD _) _) = OSFreeBSD

-- | Debian has several rolling suites, and a number of stable releases,
-- such as Stable "jessie".
data DebianSuite = Experimental | Unstable | Testing | Stable Release
	deriving (Show, Eq)

-- | FreeBSD breaks their releases into "Production" and "Legacy".
data FreeBSDRelease = FBSDProduction FBSDVersion | FBSDLegacy FBSDVersion
	deriving (Show, Eq)

data FBSDVersion = FBSD101 | FBSD102 | FBSD093
	deriving (Eq)

instance IsString FBSDVersion where
	fromString "10.1-RELEASE" = FBSD101
	fromString "10.2-RELEASE" = FBSD102
	fromString "9.3-RELEASE" = FBSD093
	fromString _ = error "Invalid FreeBSD release"

instance Show FBSDVersion where
	show FBSD101 = "10.1-RELEASE"
	show FBSD102 = "10.2-RELEASE"
	show FBSD093 = "9.3-RELEASE"

isStable :: DebianSuite -> Bool
isStable (Stable _) = True
isStable _ = False

type Release = String

-- | Many of these architecture names are based on the names used by
-- Debian, with a few exceptions for clarity.
data Architecture
	= X86_64 -- ^ 64 bit Intel, called "amd64" in Debian
	| X86_32 -- ^ 32 bit Intel, called "i386" in Debian
	| ARMHF
	| ARMEL
	| PPC
	| PPC64
	| SPARC
	| SPARC64
	| MIPS
	| MIPSEL
	| MIPS64EL
	| SH4
	| IA64 -- ^ Itanium
	| S390
	| S390X
	| ALPHA
	| HPPA
	| M68K
	| ARM64
	| X32 -- ^ New Linux ABI for 64 bit CPUs using 32-bit integers. Not widely used.
	deriving (Show, Eq)

architectureToDebianArchString :: Architecture -> String
architectureToDebianArchString X86_64 = "amd64"
architectureToDebianArchString X86_32 = "i386"
architectureToDebianArchString ARMHF = "armhf"
architectureToDebianArchString ARMEL = "armel"
architectureToDebianArchString ANDROID = "android"
architectureToDebianArchString PPC = "powerpc"
architectureToDebianArchString PPC64 = "ppc64el"
architectureToDebianArchString SPARC = "sparc"
architectureToDebianArchString SPARC64 = "sparc64"
architectureToDebianArchString MIPS = "mips"
architectureToDebianArchString MIPSEL = "mipsel"
architectureToDebianArchString MIPS64EL = "mips64el"
architectureToDebianArchString SH4 = "sh"
architectureToDebianArchString IA64 = "ia64"
architectureToDebianArchString S390 = "s390"
architectureToDebianArchString S390X = "s390x"
architectureToDebianArchString ALPHA = "alpha"
architectureToDebianArchString HPPA = "hppa"
architectureToDebianArchString M68K = "m68k"
architectureToDebianArchString ARM64 = "arm64"
architectureToDebianArchString X32 = "x32"

type UserName = String

newtype User = User UserName
	deriving (Eq, Ord, Show)

newtype Group = Group String
	deriving (Eq, Ord, Show)

-- | Makes a Group with the same name as the User.
userGroup :: User -> Group
userGroup (User u) = Group u

newtype Port = Port Int
	deriving (Eq, Show)

fromPort :: Port -> String
fromPort (Port p) = show p
