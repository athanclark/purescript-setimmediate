module Control.Execution.Immediate
  ( ImmediateID
  , run0, run1, run2, run3, run4, run5, cancel
  , registerShim
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5, Fn6, runFn2, runFn3, runFn4, runFn5, runFn6)
-- import Control.Monad.Effect (Eff, kind Effect)
-- import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, EffFn4, EffFn5, mkEffFn1, mkEffFn2, mkEffFn3, mkEffFn4, mkEffFn5)
import Effect (Effect)
import Effect.Uncurried
  ( EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5
  , mkEffectFn1, mkEffectFn2, mkEffectFn3, mkEffectFn4, mkEffectFn5)



foreign import registerShim :: Effect Unit

foreign import data ImmediateID :: Type


foreign import run0Impl :: Effect Unit -> ImmediateID
foreign import run1Impl :: forall a. Fn2 (EffectFn1 a Unit) a ImmediateID
foreign import run2Impl :: forall a b. Fn3 (EffectFn2 a b Unit) a b ImmediateID
foreign import run3Impl :: forall a b c. Fn4 (EffectFn3 a b c Unit) a b c ImmediateID
foreign import run4Impl :: forall a b c d. Fn5 (EffectFn4 a b c d Unit) a b c d ImmediateID
foreign import run5Impl :: forall a b c d e. Fn6 (EffectFn5 a b c d e Unit) a b c d e ImmediateID



run0 :: Effect Unit -> Effect ImmediateID
run0 = pure <<< run0Impl

run1 :: forall a
      . (a -> Effect Unit)
     -> a -> Effect ImmediateID
run1 f a = pure (runFn2 run1Impl (mkEffectFn1 f) a)

run2 :: forall a b
      . (a -> b -> Effect Unit)
     -> a -> b -> Effect ImmediateID
run2 f a b = pure (runFn3 run2Impl (mkEffectFn2 f) a b)

run3 :: forall a b c
      . (a -> b -> c -> Effect Unit)
     -> a -> b -> c -> Effect ImmediateID
run3 f a b c = pure (runFn4 run3Impl (mkEffectFn3 f) a b c)

run4 :: forall a b c d
      . (a -> b -> c -> d -> Effect Unit)
     -> a -> b -> c -> d -> Effect ImmediateID
run4 f a b c d = pure (runFn5 run4Impl (mkEffectFn4 f) a b c d)

run5 :: forall a b c d e
      . (a -> b -> c -> d -> e -> Effect Unit)
     -> a -> b -> c -> d -> e -> Effect ImmediateID
run5 f a b c d e = pure (runFn6 run5Impl (mkEffectFn5 f) a b c d e)


foreign import cancelImpl :: ImmediateID -> Unit

cancel :: ImmediateID -> Effect Unit
cancel = pure <<< cancelImpl
