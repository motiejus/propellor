{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Propellor.Property.List (
	props,
	PropertyList(..),
	PropertyListType,
	PropList(..),
) where

import Propellor.Types
import Propellor.Types.Core
import Propellor.Types.MetaTypes
import Propellor.PropAccum
import Propellor.Engine
import Propellor.Exception

import Data.Monoid

toProps :: [Property (MetaTypes metatypes)] -> Props (MetaTypes metatypes)
toProps ps = Props (map toChildProperty ps)

-- | Combines a list of properties, resulting in a single property
-- that when run will run each property in the list in turn,
-- and print out the description of each as it's run. Does not stop
-- on failure; does propagate overall success/failure.
--
-- For example:
--
-- > propertyList "foo" $ props
-- > 	& someproperty
-- > 	! oldproperty
-- > 	& otherproperty
props :: PropList
props = PropList []

data PropList = PropList [Property HasInfo]

instance PropAccum PropList where
	PropList l `addProp` p = PropList (toProp p : l)
	PropList l `addPropFront` p = PropList (l ++ [toProp p])
	getProperties (PropList l) = reverse l

class PropertyList l where
	-- | Combines a list of properties, resulting in a single property
	-- that when run will run each property in the list in turn,
	-- and print out the description of each as it's run. Does not stop
	-- on failure; does propagate overall success/failure.
	--
	-- Note that Property HasInfo and Property NoInfo are not the same
	-- type, and so cannot be mixed in a list. To make a list of
	-- mixed types, which can also include RevertableProperty,
	-- use `props`
	propertyList :: Desc -> l -> Property (PropertyListType l)

-- | Combines a list of properties, resulting in one property that
-- ensures each in turn. Stops if a property fails.
--
-- > combineProperties "foo" $ props
-- > 	& bar
-- > 	& baz
--
-- This is similar to using `mconcat` with a list of properties,
-- except it can combine together different types of properties.
combineProperties :: SingI metatypes => Desc -> Props (MetaTypes metatypes) -> Property (MetaTypes metatypes)
combineProperties desc (Props ps) = 
	property desc (combineSatisfy cs NoChange)
		`addChildren` cs
  where
	cs = map toChildProperty ps

combineSatisfy :: [ChildProperty] -> Result -> Propellor Result
combineSatisfy [] rs = return rs
combineSatisfy (p:ps) rs = do
	r <- maybe (return NoChange) catchPropellor (getSatisfy p)
	case r of
		FailedChange -> return FailedChange
		_ -> combineSatisfy ps (r <> rs)
