-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import Data.String (fromString)

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.User as User
--import qualified Propellor.Property.Hostname as Hostname
--import qualified Propellor.Property.Tor as Tor
import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.FreeBSD.Pkg as Pkg
import qualified Propellor.Property.ZFS as ZFS
import qualified Propellor.Property.FreeBSD.Poudriere as Poudriere

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts =
	[ mybox
	  , freebsd
	]

-- An example host.
mybox :: Host
mybox = host "mybox.example.com"
	& os (System (Debian Unstable) "amd64")
	& Apt.stdSourcesList
	& Apt.unattendedUpgrades
	& Apt.installed ["etckeeper"]
	& Apt.installed ["ssh"]
	& User.hasSomePassword (User "root")
	& Network.ipv6to4
	& File.dirExists "/var/www"
	& Docker.docked webserverContainer
	& Docker.garbageCollected `period` Daily
	& Cron.runPropellor (Cron.Times "30 * * * *")

-- An example host.
mybox :: Host
mybox = host "mybox.example.com" $ props
	& osDebian Unstable X86_64
	& Apt.stdSourcesList
	& Docker.publish "80:80"
	& Docker.volume "/var/www:/var/www"
	& Apt.serviceInstalledRunning "apache2"

poudriereZFS :: Poudriere.Poudriere
poudriereZFS = Poudriere.defaultConfig {
	Poudriere._zfs = Just $ Poudriere.PoudriereZFS
		(ZFS.ZFS (fromString "zroot") (fromString "poudriere"))
		(ZFS.fromList [ZFS.Mountpoint (fromString "/poudriere"), ZFS.ACLInherit ZFS.AIPassthrough])
	}

-- An example host.
freebsd :: Host
freebsd = host "192.168.56.10"
	& os (System (FreeBSD (FBSDProduction FBSD102)) "amd64")
	& Pkg.update
	& Pkg.upgrade
	& Poudriere.poudriere poudriereZFS
	& Poudriere.jail (Poudriere.Jail "formail" (fromString "10.2-RELEASE") (fromString "amd64"))
