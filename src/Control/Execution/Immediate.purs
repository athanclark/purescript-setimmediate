module Control.Execution.Immediate
  ( IMMEDIATE, ImmediateID
  , run0, run1, run2, run3, run4, run5, cancel
  , ShimRegistered, SET_IMMEDIATE_SHIM, registerShim
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5, Fn6, runFn2, runFn3, runFn4, runFn5, runFn6)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, EffFn4, EffFn5, mkEffFn1, mkEffFn2, mkEffFn3, mkEffFn4, mkEffFn5)


foreign import data ShimRegistered :: Type

foreign import data SET_IMMEDIATE_SHIM :: Effect

foreign import registerShim :: forall eff. Eff (set_immediate_shim :: SET_IMMEDIATE_SHIM | eff) ShimRegistered




foreign import data ImmediateID :: Type


foreign import run0Impl :: forall eff. Eff eff Unit -> ImmediateID
foreign import run1Impl :: forall eff a. Fn2 (EffFn1 eff a Unit) a ImmediateID
foreign import run2Impl :: forall eff a b. Fn3 (EffFn2 eff a b Unit) a b ImmediateID
foreign import run3Impl :: forall eff a b c. Fn4 (EffFn3 eff a b c Unit) a b c ImmediateID
foreign import run4Impl :: forall eff a b c d. Fn5 (EffFn4 eff a b c d Unit) a b c d ImmediateID
foreign import run5Impl :: forall eff a b c d e. Fn6 (EffFn5 eff a b c d e Unit) a b c d e ImmediateID


foreign import data IMMEDIATE :: Effect


run0 :: forall eff. Eff (immediate :: IMMEDIATE | eff) Unit -> Eff (immediate :: IMMEDIATE | eff) ImmediateID
run0 = pure <<< run0Impl

run1 :: forall eff a
      . (a -> Eff (immediate :: IMMEDIATE | eff) Unit)
     -> a -> Eff (immediate :: IMMEDIATE | eff) ImmediateID
run1 f a = pure (runFn2 run1Impl (mkEffFn1 f) a)

run2 :: forall eff a b
      . (a -> b -> Eff (immediate :: IMMEDIATE | eff) Unit)
     -> a -> b -> Eff (immediate :: IMMEDIATE | eff) ImmediateID
run2 f a b = pure (runFn3 run2Impl (mkEffFn2 f) a b)

run3 :: forall eff a b c
      . (a -> b -> c -> Eff (immediate :: IMMEDIATE | eff) Unit)
     -> a -> b -> c -> Eff (immediate :: IMMEDIATE | eff) ImmediateID
run3 f a b c = pure (runFn4 run3Impl (mkEffFn3 f) a b c)

run4 :: forall eff a b c d
      . (a -> b -> c -> d -> Eff (immediate :: IMMEDIATE | eff) Unit)
     -> a -> b -> c -> d -> Eff (immediate :: IMMEDIATE | eff) ImmediateID
run4 f a b c d = pure (runFn5 run4Impl (mkEffFn4 f) a b c d)

run5 :: forall eff a b c d e
      . (a -> b -> c -> d -> e -> Eff (immediate :: IMMEDIATE | eff) Unit)
     -> a -> b -> c -> d -> e -> Eff (immediate :: IMMEDIATE | eff) ImmediateID
run5 f a b c d e = pure (runFn6 run5Impl (mkEffFn5 f) a b c d e)


foreign import cancelImpl :: ImmediateID -> Unit

cancel :: forall eff. ImmediateID -> Eff (immediate :: IMMEDIATE | eff) Unit
cancel = pure <<< cancelImpl
