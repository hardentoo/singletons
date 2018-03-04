{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Identity
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports the promoted and singled versions of the 'Identity' data type.
--
-----------------------------------------------------------------------------

module Data.Singletons.Prelude.Identity (
  -- * The 'Identity' singleton
  Sing(SIdentity, sRunIdentity),
  SIdentity, RunIdentity,

  -- * Defunctionalization symbols
  IdentitySym0, IdentitySym1,
  RunIdentitySym0, RunIdentitySym1
  ) where

import Data.Functor.Identity
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Enum
import Data.Singletons.Prelude.Instances
import Data.Singletons.Prelude.Monoid
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Semigroup
import Data.Singletons.Prelude.Show
import Data.Singletons.Single

$(singletonsOnly [d|
  -- deriving instance Enum a => Enum (Identity a)
  instance Enum a => Enum (Identity a) where
    succ (Identity x)     = Identity (succ x)
    pred (Identity x)     = Identity (pred x)
    toEnum i              = Identity (toEnum i)
    fromEnum (Identity x) = fromEnum x
    enumFromTo (Identity x) (Identity y) = map Identity (enumFromTo   x y)
    enumFromThenTo (Identity x) (Identity y) (Identity z) =
        map Identity (enumFromThenTo x y z)

  -- deriving instance Monoid a => Monoid (Identity a)
  instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

  -- deriving instance Num a => Num (Identity a)
  instance Num a => Num (Identity a) where
    Identity x + Identity y = Identity (x + y)
    Identity x - Identity y = Identity (x - y)
    Identity x * Identity y = Identity (x * y)
    negate (Identity x)     = Identity (negate x)
    abs    (Identity x)     = Identity (abs    x)
    signum (Identity x)     = Identity (signum x)
    fromInteger n           = Identity (fromInteger n)

  -- deriving instance Semigroup a => Semigroup (Identity a)
  instance Semigroup a => Semigroup (Identity a) where
    Identity x <> Identity y = Identity (x <> y)

  -- -| This instance would be equivalent to the derived instances of the
  -- 'Identity' newtype if the 'runIdentity' field were removed
  instance Show a => Show (Identity a) where
    showsPrec d (Identity x) = showParen (d > 10) $
      showString "Identity " . showsPrec 11 x
  |])
