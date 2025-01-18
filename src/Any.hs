{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Any where

import Data.Typeable (Typeable)
import Text.Printf (printf)

import Serialize (Serializable, serialize)
import Type (Type(..))

class (Show a, Typeable a) => Anyable a
instance (Show a, Typeable a) => Anyable a

data Any = forall a. Anyable a => Any (Type, a)

instance Serializable Any where
    serialize (Any (_, a)) = serialize a

instance Show Any where
    show :: Any -> String
    show (Any (type', val)) = printf "Any (%s, %s)" (show type') (show val)
