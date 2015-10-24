{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Types (
	-- * Core data types
	  Host(..)
	, Property(..)
	, property
	, property''
	, Desc
	, RevertableProperty(..)
	, (<!>)
	, Propellor(..)
	, LiftPropellor(..)
	, Info
	-- * Types of properties
	, UnixLike
	, Linux
	, DebianLike
	, Debian
	, Buntish
	, ArchLinux
	, FreeBSD
	, HasInfo
	, type (+)
	, TightenTargets(..)
	-- * Combining and modifying properties
	, Combines(..)
	, CombinedType
	, Propellor(..)
	, LiftPropellor(..)
	, EndAction(..)
	, module Propellor.Types.OS
	, module Propellor.Types.ConfigurableValue
	, module Propellor.Types.Dns
	, module Propellor.Types.Result
	, module Propellor.Types.ZFS
	) where

import Data.Monoid
import Control.Applicative
import Prelude

import Propellor.Types.Core
import Propellor.Types.Info
import Propellor.Types.OS
import Propellor.Types.ConfigurableValue
import Propellor.Types.Dns
import Propellor.Types.Result

-- | Everything Propellor knows about a system: Its hostname,
-- properties and their collected info.
data Host = Host
	{ hostName :: HostName
	, hostProperties :: [Property HasInfo]
	, hostInfo :: Info
	}
	deriving (Show, Typeable)

-- | Propellor's monad provides read-only access to info about the host
-- it's running on, and a writer to accumulate EndActions.
newtype Propellor p = Propellor { runWithHost :: RWST Host [EndAction] () IO p }
	deriving
		( Monad
		, Functor
		, Applicative
		, MonadReader Host
		, MonadWriter [EndAction]
		, MonadIO
		, MonadCatch
		, MonadThrow
		, MonadMask
		)

class LiftPropellor m where
	liftPropellor :: m a -> Propellor a

instance LiftPropellor Propellor where
	liftPropellor = id

instance LiftPropellor IO where
	liftPropellor = liftIO

instance Monoid (Propellor Result) where
	mempty = return NoChange
	-- | The second action is only run if the first action does not fail.
	mappend x y = do
		rx <- x
		case rx of
			FailedChange -> return FailedChange
			_ -> do
				ry <- y
				return (rx <> ry)

-- | An action that Propellor runs at the end, after trying to satisfy all
-- properties. It's passed the combined Result of the entire Propellor run.
data EndAction = EndAction Desc (Result -> Propellor Result)

type Desc = String

-- | The core data type of Propellor, this represents a property
-- that the system should have, with a descrition, and an action to ensure
-- it has the property.
--
-- There are different types of properties that target different OS's,
-- and so have different metatypes. 
-- For example: "Property DebianLike" and "Property FreeBSD".
--
-- Also, some properties have associated `Info`, which is indicated in
-- their type: "Property (HasInfo + DebianLike)"
--
-- There are many associated type families, which are mostly used
-- internally, so you needn't worry about them.
data Property i where
	IProperty :: Desc -> Propellor Result -> Info -> [Property HasInfo] -> Property HasInfo
	SProperty :: Desc -> Propellor Result -> [Property NoInfo] -> Property NoInfo

-- | Indicates that a Property has associated Info.
data HasInfo
-- | Indicates that a Property does not have Info.
data NoInfo

-- | Type level calculation of the combination of HasInfo and/or NoInfo
type family CInfo x y
type instance CInfo HasInfo HasInfo = HasInfo
type instance CInfo HasInfo NoInfo = HasInfo
type instance CInfo NoInfo HasInfo = HasInfo
type instance CInfo NoInfo NoInfo = NoInfo

-- | Constructs a Property with associated Info.
infoProperty 
	:: Desc -- ^ description of the property
	-> Propellor Result -- ^ action to run to satisfy the property (must be idempotent; may run repeatedly)
	-> Info -- ^ info associated with the property
	-> [Property i] -- ^ child properties
	-> Property HasInfo
infoProperty d a i cs = IProperty d a i (map toIProperty cs)

-- | Constructs a Property with no Info.
simpleProperty :: Desc -> Propellor Result -> [Property NoInfo] -> Property NoInfo
simpleProperty = SProperty

toIProperty :: Property i -> Property HasInfo
toIProperty p@(IProperty {}) = p
toIProperty (SProperty d s cs) = IProperty d s mempty (map toIProperty cs)

toSProperty :: Property i -> Property NoInfo
toSProperty (IProperty d s _ cs) = SProperty d s (map toSProperty cs)
toSProperty p@(SProperty {}) = p

-- | Makes a version of a Proprty without its Info.
-- Use with caution!
ignoreInfo :: Property i -> Property NoInfo
ignoreInfo = toSProperty

-- | Gets the action that can be run to satisfy a Property.
-- You should never run this action directly. Use
-- 'Propellor.Engine.ensureProperty` instead.
propertySatisfy :: Property i -> Propellor Result
propertySatisfy (IProperty _ a _ _) = a
propertySatisfy (SProperty _ a _) = a

instance Show (Property i) where
        show p = "property " ++ show (propertyDesc p)

instance Show RevertableProperty where
        show (RevertableProperty p _) = "property " ++ show (propertyDesc p)

-- | Changes the action that is performed to satisfy a property. 
adjustPropertySatisfy :: Property i -> (Propellor Result -> Propellor Result) -> Property i
adjustPropertySatisfy (IProperty d s i cs) f = IProperty d (f s) i cs
adjustPropertySatisfy (SProperty d s cs) f = SProperty d (f s) cs

instance Show (Property metatypes) where
	show p = "property " ++ show (getDesc p)

-- | Constructs a Property, from a description and an action to run to
-- ensure the Property is met.
--
-- Due to the polymorphic return type of this function, most uses will need
-- to specify a type signature. This lets you specify what OS the property
-- targets, etc.
--
-- For example:
--
-- > foo :: Property Debian
-- > foo = property "foo" $ do
-- >	...
-- > 	return MadeChange
property
	:: SingI metatypes
	=> Desc
	-> Propellor Result
	-> Property (MetaTypes metatypes)
property d a = Property sing d (Just a) mempty mempty

property''
	:: SingI metatypes
	=> Desc
	-> Maybe (Propellor Result)
	-> Property (MetaTypes metatypes)
property'' d a = Property sing d a mempty mempty

-- | Changes the action that is performed to satisfy a property.
adjustPropertySatisfy :: Property metatypes -> (Propellor Result -> Propellor Result) -> Property metatypes
adjustPropertySatisfy (Property t d s i c) f = Property t d (f <$> s) i c

-- | A property that can be reverted. The first Property is run
-- normally and the second is run when it's reverted.
data RevertableProperty setupmetatypes undometatypes = RevertableProperty
	{ setupRevertableProperty :: Property setupmetatypes
	, undoRevertableProperty :: Property undometatypes
	}

instance Show (RevertableProperty setupmetatypes undometatypes) where
	show (RevertableProperty p _) = show p

-- | Shorthand to construct a revertable property from any two Properties.
(<!>)
	:: Property setupmetatypes
	-> Property undometatypes
	-> RevertableProperty setupmetatypes undometatypes
setup <!> undo = RevertableProperty setup undo

instance IsProp (Property metatypes) where
	setDesc (Property t _ a i c) d = Property t d a i c
	getDesc (Property _ d _ _ _) = d
	getChildren (Property _ _ _ _ c) = c
	addChildren (Property t d a i c) c' = Property t d a i (c ++ c')
	getInfoRecursive (Property _ _ _ i c) =
		i <> mconcat (map getInfoRecursive c)
	getInfo (Property _ _ _ i _) = i
	toChildProperty (Property _ d a i c) = ChildProperty d a i c
	getSatisfy (Property _ _ a _ _) = a

instance IsProp (RevertableProperty setupmetatypes undometatypes) where
	-- | Sets the description of both sides.
	setDesc (RevertableProperty p1 p2) d =
		RevertableProperty (setDesc p1 d) (setDesc p2 ("not " ++ d))
	getDesc (RevertableProperty p1 _) = getDesc p1
	getChildren (RevertableProperty p1 _) = getChildren p1
	-- | Only add children to the active side.
	addChildren (RevertableProperty p1 p2) c = RevertableProperty (addChildren p1 c) p2
	-- | Return the Info of the currently active side.
	getInfoRecursive (RevertableProperty p1 _p2) = getInfoRecursive p1
	getInfo (RevertableProperty p1 _p2) = getInfo p1
	toChildProperty (RevertableProperty p1 _p2) = toChildProperty p1
	getSatisfy (RevertableProperty p1 _) = getSatisfy p1

-- | Type level calculation of the type that results from combining two
-- types of properties.
type family CombinedType x y
type instance CombinedType (Property x) (Property y) = Property (CInfo x y)
type instance CombinedType RevertableProperty RevertableProperty = RevertableProperty
-- When only one of the properties is revertable, the combined property is
-- not fully revertable, so is not a RevertableProperty.
type instance CombinedType RevertableProperty (Property NoInfo) = Property HasInfo
type instance CombinedType RevertableProperty (Property HasInfo) = Property HasInfo
type instance CombinedType (Property NoInfo) RevertableProperty = Property HasInfo
type instance CombinedType (Property HasInfo) RevertableProperty = Property HasInfo

class Combines x y where
	-- | Combines together two properties, yielding a property that
	-- has the description and info of the first, and that has the second
	-- property as a child. 
	combineWith 
		:: (Propellor Result -> Propellor Result -> Propellor Result)
		-- ^ How to combine the actions to satisfy the properties.
		-> (Propellor Result -> Propellor Result -> Propellor Result)
		-- ^ Used when combining revertable properties, to combine
		-- their reversion actions.
		-> x
		-> y
		-> CombinedType x y

instance Combines (Property HasInfo) (Property HasInfo) where
	combineWith f _ (IProperty d1 a1 i1 cs1) y@(IProperty _d2 a2 _i2 _cs2) =
		IProperty d1 (f a1 a2) i1 (y : cs1)

instance Combines (Property HasInfo) (Property NoInfo) where
	combineWith f _ (IProperty d1 a1 i1 cs1) y@(SProperty _d2 a2 _cs2) =
		IProperty d1 (f a1 a2) i1 (toIProperty y : cs1)

instance Combines (Property NoInfo) (Property HasInfo) where
	combineWith f _ (SProperty d1 a1 cs1) y@(IProperty _d2 a2 _i2 _cs2) =
		IProperty d1 (f a1 a2) mempty (y : map toIProperty cs1)

instance Combines (Property NoInfo) (Property NoInfo) where
	combineWith f _ (SProperty d1 a1  cs1) y@(SProperty _d2 a2 _cs2) =
		SProperty d1 (f a1 a2) (y : cs1)

instance Combines RevertableProperty RevertableProperty where
	combineWith sf tf (RevertableProperty s1 t1) (RevertableProperty s2 t2) =
		RevertableProperty
			(combineWith sf tf s1 s2)
			(combineWith tf sf t1 t2)

instance Combines RevertableProperty (Property HasInfo) where
	combineWith sf tf (RevertableProperty x _) y = combineWith sf tf x y

instance Combines RevertableProperty (Property NoInfo) where
	combineWith sf tf (RevertableProperty x _) y = combineWith sf tf x y

instance Combines (Property HasInfo) RevertableProperty where
	combineWith sf tf x (RevertableProperty y _) = combineWith sf tf x y

instance Combines (Property NoInfo) RevertableProperty where
	combineWith sf tf x (RevertableProperty y _) = combineWith sf tf x y
