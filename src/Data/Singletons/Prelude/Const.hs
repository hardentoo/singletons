{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Const
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports the promoted and singled versions of the 'Const' data type.
--
-----------------------------------------------------------------------------

module Data.Singletons.Prelude.Const (
  -- * The 'Const' singleton
  Sing(SConst, sGetConst),
  SConst, GetConst,

  -- * Defunctionalization symbols
  ConstSym0, ConstSym1,
  GetConstSym0, GetConstSym1
  ) where

import Data.Functor.Const
import Data.Kind (Type)
import Data.Singletons.Internal
import Data.Singletons.Prelude.Base hiding (Const, ConstSym0, ConstSym1)
import Data.Singletons.Prelude.Enum
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Instances
import Data.Singletons.Prelude.Monoid
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Semigroup
import Data.Singletons.Prelude.Show
import Data.Singletons.Single

-- TODO RGS: Explain why we have to write all of these by hand.
data instance Sing :: forall (k :: Type) (a :: Type) (b :: k). Const a b -> Type where
  SConst :: { sGetConst :: Sing a } -> Sing ('Const a)
type SConst = (Sing :: Const a (b :: k) -> Type)
instance SingKind a => SingKind (Const a b) where
  type Demote (Const a b) = Const (Demote a) b
  fromSing (SConst sa) = Const (fromSing sa)
  toSing (Const a) = withSomeSing a $ SomeSing . SConst
instance SingI a => SingI ('Const a) where
  sing = SConst sing
data ConstSym0 :: forall (k :: Type) (a :: Type) (b :: k). a ~> Const a b
type instance Apply ConstSym0 x = 'Const x
type ConstSym1 (a :: Type) = 'Const a

$(singletons [d|
  type family GetConst (x :: Const a (b :: k)) :: a where
    GetConst ('Const x) = x
  |])

$(singletonsOnly [d|
  deriving instance Bounded a => Bounded (Const a (b :: k))
  deriving instance Eq      a => Eq      (Const a (b :: k))
  deriving instance Ord     a => Ord     (Const a (b :: k))

  -- deriving instance Enum a => Enum (Const a (b :: k))
  instance Enum a => Enum (Const a (b :: k)) where
    succ (Const x)     = Const (succ x)
    pred (Const x)     = Const (pred x)
    toEnum i           = Const (toEnum i)
    fromEnum (Const x) = fromEnum x
    enumFromTo (Const x) (Const y) = map Const (enumFromTo   x y)
    enumFromThenTo (Const x) (Const y) (Const z) =
        map Const (enumFromThenTo x y z)

  -- deriving instance Monoid a => Monoid (Const a (b :: k))
  instance Monoid a => Monoid (Const a (b :: k)) where
    mempty = Const mempty

  -- deriving instance Num a => Num (Const a (b :: k))
  instance Num a => Num (Const a (b :: k)) where
    Const x + Const y = Const (x + y)
    Const x - Const y = Const (x - y)
    Const x * Const y = Const (x * y)
    negate (Const x)  = Const (negate x)
    abs    (Const x)  = Const (abs    x)
    signum (Const x)  = Const (signum x)
    fromInteger n     = Const (fromInteger n)

  -- deriving instance Semigroup a => Semigroup (Const a (b :: k))
  instance Semigroup a => Semigroup (Const a (b :: k)) where
    Const x <> Const y = Const (x <> y)

  -- -| This instance would be equivalent to the derived instances of the
  -- 'Const' newtype if the 'runConst' field were removed
  instance Show a => Show (Const a (b :: k)) where
      showsPrec d (Const x) = showParen (d > 10) $
                              showString "Const " . showsPrec 11 x
  |])
