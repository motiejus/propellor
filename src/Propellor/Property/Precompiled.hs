module Propellor.Property.Precompiled (
	precompiled
) where

import Propellor.Base
import Propellor.Types
import Propellor.Types.Info
import Propellor.Info

import System.Info (arch, os)

-- | Specifies that propellor should be precompiled before being sent and
-- executed on the remote host
precompiled :: Property (HasInfo + UnixLike)
precompiled = withOS "Set build state as precompiled" $ \w o ->
	case o of
		(Just (System (Debian _) arch')) ->
			if compatibleArch getControllerArchitecture arch' then ensureProperty w precompiled' else unsupportedOS'
		_ -> unsupportedOS'

precompiled' :: Property (HasInfo + UnixLike)
precompiled' = pureInfoProperty ("Set build state as precompiled") (InfoVal Precompiled)

type ControllerArchitecture = Architecture
type HostArchitecture = Architecture

getControllerArchitecture :: Architecture
getControllerArchitecture = case arch of
	"x86_64" -> X86_64
	"amd64" -> X86_64
	"i386" -> X86_32
	"i686" -> X86_32
	a -> error $ "Unknown architecture: " ++ a

compatibleArch :: ControllerArchitecture -> HostArchitecture -> Bool
compatibleArch x y | x == y = True
compatibleArch X86_32 X86_64 = True
compatibleArch _ _ = False

type ControllerOS = TargetOS
type HostOS = TargetOS

-- getOS :: TargetOS
-- getOS = case os of
-- 	"linux"

compatibleOS :: ControllerOS -> HostOS -> Bool
compatibleOS x y | x == y = True
compatibleOS _ _ = False
