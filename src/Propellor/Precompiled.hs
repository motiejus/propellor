module Propellor.Precompiled (
	precompiled,
	isPrecompilable
) where

import Propellor.Base
import Propellor.Types.Info

import System.Info (arch, os)

-- | Specifies that propellor should be precompiled before being sent and
-- executed on the remote host
precompiled :: Property (HasInfo + UnixLike)
precompiled = pureInfoProperty ("Set build state as precompiled") (InfoVal Precompiled)

type ControllerArchitecture = Architecture
type HostArchitecture = Architecture

getControllerArchitecture :: Architecture
getControllerArchitecture = case arch of
	"x86_64" -> X86_64
	"amd64" -> X86_64
	"i386" -> X86_32
	"i686" -> X86_32
	a -> error $ "Unknown architecture: " ++ a
	-- TODO: add other architectures

compatibleArch :: ControllerArchitecture -> HostArchitecture -> Bool
compatibleArch x y | x == y = True
compatibleArch X86_32 X86_64 = True
compatibleArch _ _ = False

data PrecompiledOS = PLinux | PBSD
	deriving (Eq, Show)

targetOSToPrecompiledOS :: TargetOS -> PrecompiledOS
targetOSToPrecompiledOS OSDebian = PLinux
targetOSToPrecompiledOS OSBuntish = PLinux
targetOSToPrecompiledOS OSFreeBSD = PBSD

type ControllerOS = PrecompiledOS
type HostOS = PrecompiledOS

getControllerOS :: PrecompiledOS
getControllerOS = case os of
	"linux" -> PLinux
	h -> error $ "Unknown OS: " ++ h
	-- TODO: add other oses

compatibleOS :: ControllerOS -> HostOS -> Bool
compatibleOS x y = x == y

isPrecompilable :: System -> Bool
isPrecompilable s@(System _ harch) = compatibleArch carch harch && compatibleOS cos' hos
  where
	carch = getControllerArchitecture
	cos' = getControllerOS
	hos = targetOSToPrecompiledOS (systemToTargetOS s)
