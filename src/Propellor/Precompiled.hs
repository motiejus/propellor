module Propellor.Precompiled (
	precompiled,
	isPrecompilable
) where

import Propellor.Base
import Propellor.Types.Info

import System.Info (arch, os)

-- | Specifies that propellor should be precompiled before being sent and
-- executed on the remote host.
--
-- For precompilation to work, the controller that is building and running
-- propellor must have a compatable architecture as the host that the
-- precompiled propellor will run on. This is checked before sending the
-- precompiled binary, and spinning a precompiled host will abort if
-- it's not compatable.
--
-- Note that sending precompiled propellor binaries can use significantly
-- more bandwidth than using source code.
precompiled :: Property (HasInfo + UnixLike)
precompiled = pureInfoProperty "Set build state as precompiled" (InfoVal Precompiled)

type ControllerArchitecture = Architecture
type HostArchitecture = Architecture

getControllerArchitecture :: Either String Architecture
getControllerArchitecture = case arch of
	"x86_64" -> Right X86_64
	"amd64" -> Right X86_64
	"i386" -> Right X86_32
	"i686" -> Right X86_32
	"ppc" -> Right PPC
	"powerpc" -> Right PPC
	"powerpc64" -> Right PPC64
	"sparc" -> Right SPARC
	"sparc64" -> Right SPARC64
	"mips" -> Right MIPS
	"mipsel" -> Right MIPSEL
	"mips64el" -> Right MIPS64EL
	"sh" -> Right SH4
	"sh4" -> Right SH4
	"ia64" -> Right M68K
	"s390" -> Right S390
	"s390x" -> Right S390X
	"alpha" -> Right ALPHA
	"hppa" -> Right HPPA
	"m68k" -> Right M68K
	"arm64" -> Right ARM64
	"x32" -> Right X32
	a -> Left ("Unknown architecture: " ++ a)

compatibleArch :: ControllerArchitecture -> HostArchitecture -> Bool
compatibleArch x y | x == y = True
compatibleArch X86_32 X86_64 = True
compatibleArch _ _ = False

data PrecompiledOS = PLinux | PBSD | PHurd
	deriving (Eq, Show)

systemToPrecompiledOS :: System -> PrecompiledOS
systemToPrecompiledOS (System (Debian Linux _) _) = PLinux
systemToPrecompiledOS (System (Debian KFreeBSD _) _) = PBSD
systemToPrecompiledOS (System (Debian Hurd _) _) = PHurd
systemToPrecompiledOS (System (Buntish _) _) = PLinux
systemToPrecompiledOS (System (FreeBSD _) _) = PBSD

type ControllerOS = PrecompiledOS
type HostOS = PrecompiledOS

getControllerOS :: Either String PrecompiledOS
getControllerOS = case os of
	"linux" -> Right PLinux
	"freebsd" -> Right PBSD
	"kfreebsdgnu" -> Right PBSD
	"hurd" -> Right PHurd
	"gnu" -> Right PHurd
	o -> Left ("Unknown OS: " ++ o)

compatibleOS :: ControllerOS -> HostOS -> Bool
compatibleOS x y = x == y

isPrecompilable :: System -> Bool
isPrecompilable s@(System _ harch) = case (getControllerArchitecture, getControllerOS) of
	(Right carch, Right cos') -> compatibleArch carch harch && compatibleOS cos' hos
	_ -> False
  where
	hos = systemToPrecompiledOS s
