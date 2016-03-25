{-# LANGUAGE PackageImports, FlexibleContexts #-}

module Propellor.PropAccum
	( host
	, Props(..)
	, props
	, (&)
	, (&^)
	, (!)
	--, propagateContainer
	) where

import Propellor.Types
import Propellor.Types.MetaTypes
import Propellor.Types.Core
import Propellor.Property

import Data.Monoid
import Prelude

-- | Defines a host and its properties.
--
-- > host "example.com" $ props
-- > 	& someproperty
-- > 	! oldproperty
-- > 	& otherproperty
host :: HostName -> Props metatypes -> Host
host hn (Props ps) = Host hn ps (mconcat (map getInfoRecursive ps))

-- | Start accumulating a list of properties.
--
-- Properties can be added to it using `(&)` etc.
props :: Props UnixLike
props = Props []

infixl 1 &
infixl 1 &^
infixl 1 !

	getProperties :: h -> [ChildProperty]

-- | Adds a property to a Props.
--
-- Can add Properties and RevertableProperties
(&)
	::
		( IsProp p
		-- -Wredundant-constraints is turned off because
		-- this constraint appears redundant, but is actually
		-- crucial.
		, MetaTypes y ~ GetMetaTypes p
		, CheckCombinable x y ~ 'CanCombine
		)
	=> Props (MetaTypes x)
	-> p
	-> Props (MetaTypes (Combine x y))
Props c & p = Props (c ++ [toChildProperty p])

-- | Adds a property before any other properties.
(&^)
	::
		( IsProp p
		-- -Wredundant-constraints is turned off because
		-- this constraint appears redundant, but is actually
		-- crucial.
		, MetaTypes y ~ GetMetaTypes p
		, CheckCombinable x y ~ 'CanCombine
		)
	=> Props (MetaTypes x)
	-> p
	-> Props (MetaTypes (Combine x y))
Props c &^ p = Props (toChildProperty p : c)

-- | Adds a property in reverted form.
(!) :: IsProp (RevertableProperty undometatypes setupmetatypes) => PropAccum h => h -> RevertableProperty setupmetatypes undometatypes -> h
h ! p = h & revert p

infixl 1 &
infixl 1 &^
infixl 1 !

instance PropAccum Host where
	(Host hn ps is) `addProp`  p = Host hn (ps ++ [toProp p])
		(is <> getInfoRecursive p)
	(Host hn ps is) `addPropFront` p = Host hn (toProp p : ps)
		(getInfoRecursive p <> is)
	getProperties = hostProperties

{-

-- | Adjust the provided Property, adding to its
-- propertyChidren the properties of the provided container.
-- 
-- The Info of the propertyChildren is adjusted to only include 
-- info that should be propagated out to the Property.
--
-- Any PrivInfo that uses HostContext is adjusted to use the name
-- of the container as its context.
propagateContainer
	:: (PropAccum container)
	=> String
	-> container
	-> Property metatypes
	-> Property metatypes
propagateContainer containername c prop = Property
	undefined
	(propertyDesc prop)
	(propertySatisfy prop)
	(propertyInfo prop)
	(propertyChildren prop ++ hostprops)
  where
	hostprops = map go $ getProperties c
	go p = 
		let i = mapInfo (forceHostContext containername)
			(propagatableInfo (propertyInfo p))
		    cs = map go (propertyChildren p)
		in infoProperty (propertyDesc p) (propertySatisfy p) i cs

-}
