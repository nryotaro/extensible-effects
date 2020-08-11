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

import Control.Applicative
import Control.Monad
import Data.Functor

data VE w = Val w | E (Int -> VE w)

newtype Eff a = Eff
  { runEff :: forall w. (a -> VE w) -> VE w
  }

instance Functor Eff where
  -- (a -> b) -> f a -> f b
  fmap f (Eff c) = Eff $ \k -> c (\a -> k (f a))

instance Applicative Eff where
  pure x = Eff $ \k -> k x
  (Eff a) <*> (Eff b) = Eff $ \k -> (a (\ab -> (b (\c -> k (ab c)))))

instance Monad Eff where
  return = pure
  m >>= f = Eff $ \k -> runEff m (\v -> runEff (f v) k)

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

local :: (Int -> Int) -> Eff w -> Eff w
local f m = do
  e0 <- ask
  let e = f e0
  let loop (Val x) = return x
      loop (E k) = loop (k e)
  loop (admin m)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
