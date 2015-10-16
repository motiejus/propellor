{-# LANGUAGE FlexibleInstances #-}

module Propellor.Property.Spin (
	Spinnable(..),
	controllerFor,
	controllerKeys,
	controlledBy,
) where

import Propellor.Base
import Propellor.Spin (spin)
import Propellor.Types.CmdLine (ControllerChain(..))
import Propellor.Types.Info
import qualified Propellor.Property.Ssh as Ssh

-- | A class of things that can be spinned.
class Spinnable t where
	toSpin :: t -> Property NoInfo

instance Spinnable Host where
	toSpin h = go `requires` Ssh.knownHost [h] (hostName h) (User "root")
	  where
	  	go = property (cdesc (hostName h)) $ do
			ControllerChain cc <- getControllerChain
			if hostName h `elem` cc
				then noChange -- avoid loop
				else do
					liftIO $ spin (hostName h) Nothing (ControllerChain cc) h
					-- Don't know if the spin made a change to the
					-- remote host or not, but in any case, the
					-- local host was not changed.
					noChange

-- | Each Host in the list is spinned in turn. Does not stop on spin
-- failure; does propigate overall success/failure.
instance Spinnable [Host] where
	toSpin l = propertyList (cdesc $ unwords $ map hostName l) (map toSpin l)

-- | The Host that has this Property is in control of running propellor on
-- some other Hosts.
--
-- Making a host a controller eliminates the need to manually run
-- propellor --spin to update the controlled hosts. Each time
-- propellor is run on the controller host, it will in turn run
-- propellor on the controlled Hosts.
--
-- The controller needs to be able to ssh to the hosts it controls,
-- and run propellor, as root. The controller is automatically configured
-- with `Propellor.Property.Ssh.knownHost` to know the host keys of the 
-- hosts that it will ssh to. It's up to you to use `controllerKey`
-- and `controlledBy` to set up the ssh keys that will let the controller
-- log into the hosts it controls.
--
-- For example, if you have some webservers and a dnsserver,
-- and want a master that runs propellor on all of them:
--
-- > import Propellor
-- > import qualified Propellor.Property.Spin as Spin
-- > import qualified Propellor.Property.Ssh as Ssh
-- > import qualified Propellor.Property.Cron as Cron
-- > 
-- > main = defaultMain hosts
-- >
-- > hosts =
-- > 	[ master
-- >	, dnsserver
-- >	] ++ webservers
-- > 
-- > dnsserver = host "dns.example.com"
-- >	& Ssh.hostKeys hostContext [(SshEcdsa, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB3BJ2GqZiTR2LEoDXyYFgh/BduWefjdKXAsAtzS9zeI")]
-- >    & Spin.controlledBy master
-- >	& ...
-- > 
-- > webservers =
-- >    [ host "www1.example.com"
-- >		& Ssh.hostKeys hostContext [(SshEcdsa, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICfFntnesZcYz2B2T41ay45igfckXRSh5uVffkuCQkLv")]
-- > 		& Spin.controlledBy master
-- >		& ...
-- >	, ...
-- >	]
-- >
-- > master = host "master.example.com"
-- >	& Spin.controllerKeys [(SshEcdsa, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFWD0Hau5FDLeNrDHKilNMKm9c68R3WD+NJOp2jPWvJV")]
-- > 	-- Only update dnsserver once all webservers are successfully updated.
-- >	& Spin.controllerFor dnsserver
-- >		`requires` Spin.controllerFor webservers
-- >	& Cron.runPropellor
--
-- Multiple controllers can control the same hosts. However, when
-- propellor is already running on a host, a controller will fail
-- to run it. So, if two controllers both try to control the same
-- host at the same time, one will fail.
--
-- Chains of controllers are supported; host A can control host B which
-- controls host C. Loops of controllers are automatically prevented.
controllerFor :: Spinnable h => h -> Property NoInfo
controllerFor h = toSpin h
	`requires` Ssh.installed

-- | Uses `Propellor.Property.Ssh.keysImported` to set up the ssh keys
-- for a controller; so the corresponding private keys come from the privdata.
controllerKeys :: [(SshKeyType, Ssh.PubKeyText)] -> Property HasInfo
controllerKeys ks = Ssh.userKeys (User "root") hostContext ks
	`requires` Ssh.installed

-- | Use this property to let the specified controller Host ssh in
-- and run propellor.
controlledBy :: Host -> Property NoInfo
controlledBy h = User "root" `Ssh.authorizedKeysFrom` (User "root", h)
	`requires` Ssh.installed

cdesc :: String -> Desc
cdesc n = "controller for " ++ n

-- | The current host is included on the chain, as well as any hosts that
-- acted as controllers to get the current propellor process to run.
getControllerChain :: Propellor ControllerChain
getControllerChain = do
	hn <- hostName <$> ask
	ControllerChain cc <- fromMaybe (ControllerChain []) . fromInfoVal <$> askInfo
	return (ControllerChain (hn:cc))
