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

module Lib () where

import Control.Applicative
import Control.Monad
import Data.Functor

data VE w r = Val w | E (r (VE w r))

newtype Reader e v = Reader (e -> v)

newtype Eff r a = Eff {runEff :: forall w. (a -> VE w r) -> VE w r}

instance Functor (Eff r) where
  -- (a -> b) -> f a -> f b
  fmap f (Eff c) = Eff $ \k -> c (\a -> k (f a))

instance Applicative (Eff r) where
  pure x = Eff $ \k -> k x
  (Eff a) <*> (Eff b) = Eff $ \k -> (a (\ab -> (b (\c -> k (ab c)))))

instance Monad (Eff r) where
  return = pure
  m >>= f = Eff $ \k -> runEff m (\v -> runEff (f v) k)


send :: (forall w. (a -> VE w r) -> r (VE w r)) -> Eff r a
send f = Eff $ \k -> E (f k)

admin :: Eff r w -> VE w r
admin (Eff m) = m Val