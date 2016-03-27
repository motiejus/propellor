{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

module Propellor.PropAccum
	( host
	, Props(..)
	, props
	, (&)
	, (&^)
	, (!)
	, hostProps
	, modifyHostProps
	) where

import Propellor.Types
import Propellor.Types.MetaTypes
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

-- | Note that the metatype of a Host's properties is not retained,
-- so this defaults to UnixLike. So, using this with modifyHostProps can
-- add properties to a Host that conflict with properties already in it.
-- Use caution when using this.
hostProps :: Host -> Props UnixLike
hostProps = Props . hostProperties

modifyHostProps :: Host -> Props metatypes -> Host
modifyHostProps h ps = host (hostName h) ps

-- | Props is a combination of a list of properties, with their combined 
-- metatypes.
data Props metatypes = Props [ChildProperty]

-- | Start accumulating a list of properties.
--
-- Properties can be added to it using `(&)` etc.
props :: Props UnixLike
props = Props []

infixl 1 &
infixl 1 &^
infixl 1 !

type family GetMetaTypes x
type instance GetMetaTypes (Property (MetaTypes t)) = MetaTypes t
type instance GetMetaTypes (RevertableProperty (MetaTypes t) undo) = MetaTypes t

-- | Adds a property to a Props.
--
-- Can add Properties and RevertableProperties
(&)
	::
		( IsProp p
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
		, MetaTypes y ~ GetMetaTypes p
		, CheckCombinable x y ~ 'CanCombine
		)
	=> Props (MetaTypes x)
	-> p
	-> Props (MetaTypes (Combine x y))
Props c &^ p = Props (toChildProperty p : c)

-- | Adds a property in reverted form.
(!)
	:: (CheckCombinable x z ~ 'CanCombine)
	=> Props (MetaTypes x)
	-> RevertableProperty (MetaTypes y) (MetaTypes z)
	-> Props (MetaTypes (Combine x z))
Props c ! p = Props (c ++ [toChildProperty (revert p)])

-- addPropsHost :: Host -> [Prop] -> Host
-- addPropsHost (Host hn ps i) p = Host hn ps' i'
--   where
-- 	ps' = ps ++ [toChildProperty p]
-- 	i' = i <> getInfoRecursive p
