-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

--
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

module Lib
  ( someFunc,
  )
where
import Control.Monad
import Data.Functor

data VE w = Val w | E (Int -> VE w)

newtype Eff a = Eff
  { runEff :: forall w. (a -> VE w) -> VE w
  }

instance Functor Eff where
  fmap f (Eff c) = Eff $ \k -> c (\a -> k (f a))

{-
instance Monad Eff where
  return x = Eff $ \k -> k x
  m >>= f = Eff $ \k -> runEff m (\v -> runEff (f v) k)
-}

ask :: Eff Int
ask = Eff (\k -> E k)

admin :: Eff w -> VE w
admin (Eff m) = m Val

runReader :: Eff w -> Int -> w
runReader m e = loop (admin m)
  where
    loop :: VE w -> w
    loop (Val x) = x
    loop (E k) = loop (k e)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
