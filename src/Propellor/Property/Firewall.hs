-- | Maintainer: Arnaud Bailly <arnaud.oqube@gmail.com>
--
-- Properties for configuring firewall (iptables) rules

module Propellor.Property.Firewall (
	rule,
	installed,
	Chain(..),
	Table(..),
	Target(..),
	Proto(..),
	Rules(..),
	ConnectionState(..),
	ICMPTypeMatch(..),
	TCPFlag(..),
	Frequency(..),
	IPWithMask(..),
) where

import Data.Monoid
import Data.Char
import Data.List

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Network as Network

installed :: Property DebianLike
installed = Apt.installed ["iptables"]

rule :: Chain -> Table -> Target -> Rules -> Property Linux
rule c tb tg rs = property ("firewall rule: " <> show r) addIpTable
  where
	r = Rule c tb tg rs
	addIpTable = liftIO $ do
		let args = toIpTable r
		exist <- boolSystem "iptables" (chk args)
		if exist
			then return NoChange
			else toResult <$> boolSystem "iptables" (add args)
	add params = Param "-A" : params
	chk params = Param "-C" : params

toIpTable :: Rule -> [CommandParam]
toIpTable r =  map Param $
	show (ruleChain r) :
	toIpTableArg (ruleRules r) ++ [ "-j" , show $ ruleTarget r ]

toIpTableArg :: Rules -> [String]
toIpTableArg Everything = []
toIpTableArg (Proto proto) = ["-p", map toLower $ show proto]
toIpTableArg (DPort (Port port)) = ["--dport", show port]
toIpTableArg (DPortRange (Port f, Port t)) =
	["--dport", show f ++ ":" ++ show t]
toIpTableArg (InIFace iface) = ["-i", iface]
toIpTableArg (OutIFace iface) = ["-o", iface]
toIpTableArg (Ctstate states) =
	[ "-m"
	, "conntrack"
	, "--ctstate", intercalate "," (map show states)
	]
toIpTableArg (Source ipwm) =
	[ "-s"
	, intercalate "," (map fromIPWithMask ipwm)
	]
toIpTableArg (Destination ipwm) =
	[ "-d"
	, intercalate "," (map fromIPWithMask ipwm)
	]
toIpTableArg (r :- r') = toIpTableArg r <> toIpTableArg r'

data IPWithMask = IPWithNoMask IPAddr | IPWithIPMask IPAddr IPAddr | IPWithNumMask IPAddr Int
	deriving (Eq, Show)

fromIPWithMask :: IPWithMask -> String
fromIPWithMask (IPWithNoMask ip) = fromIPAddr ip
fromIPWithMask (IPWithIPMask ip ipm) = fromIPAddr ip ++ "/" ++ fromIPAddr ipm
fromIPWithMask (IPWithNumMask ip m) = fromIPAddr ip ++ "/" ++ show m

data Rule = Rule
	{ ruleChain  :: Chain
	, ruleTable  :: Table
	, ruleTarget :: Target
	, ruleRules  :: Rules
	} deriving (Eq, Show)

data Table = Filter | Nat | Mangle | Raw | Security
	deriving (Eq, Show)

instance ConfigurableValue Table where
	val Filter = "filter"
	val Nat = "nat"
	val Mangle = "mangle"
	val Raw = "raw"
	val Security = "security"

data Target = ACCEPT | REJECT | DROP | LOG | TargetCustom String
	deriving (Eq, Show)

instance ConfigurableValue Target where
	val ACCEPT = "ACCEPT"
	val REJECT = "REJECT"
	val DROP = "DROP"
	val LOG = "LOG"
	val (TargetCustom t) = t

data Chain = INPUT | OUTPUT | FORWARD | PREROUTING | POSTROUTING | ChainCustom String
	deriving (Eq, Show)

instance ConfigurableValue Chain where
	val INPUT = "INPUT"
	val OUTPUT = "OUTPUT"
	val FORWARD = "FORWARD"
	val PREROUTING = "PREROUTING"
	val POSTROUTING = "POSTROUTING"
	val (ChainCustom c) = c

data Proto = TCP | UDP | ICMP
	deriving (Eq, Show)

data ConnectionState = ESTABLISHED | RELATED | NEW | INVALID
	deriving (Eq, Show)

data ICMPTypeMatch = ICMPTypeName String | ICMPTypeCode Int
	deriving (Eq, Show)

instance ConfigurableValue ICMPTypeMatch where
	val (ICMPTypeName t) = t
	val (ICMPTypeCode c) = val c

data Frequency = NumBySecond Int
	deriving (Eq, Show)

instance ConfigurableValue Frequency where
	val (NumBySecond n) = val n ++ "/second"

type TCPFlagMask = [TCPFlag]

type TCPFlagComp = [TCPFlag]

data TCPFlag = SYN | ACK | FIN | RST | URG | PSH | ALL | NONE
	deriving (Eq, Show)

data Rules
	= Everything
	| Proto Proto
	-- ^There is actually some order dependency between proto and port so this should be a specific
	-- data type with proto + ports
	| DPort Port
	| DPortRange (Port,Port)
	| InIFace Network.Interface
	| OutIFace Network.Interface
	| Ctstate [ ConnectionState ]
	| Source [ IPWithMask ]
	| Destination [ IPWithMask ]
	| Rules :- Rules   -- ^Combine two rules
	deriving (Eq, Show)

infixl 0 :-

instance Monoid Rules where
	mempty  = Everything
	mappend = (:-)
