module Propellor.Types.Build where

import Data.Typeable

-- | Specifies the build state of a host
-- Precompiled means that propellor should be precompiled before being sent
-- and executed on the remote host
data Build = Precompiled
	deriving (Show, Typeable)
