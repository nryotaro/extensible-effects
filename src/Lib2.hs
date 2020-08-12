-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ExistentialQuantification #-}

{-# LANGUAGE ScopedTypeVariables #-}

-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

module Lib2() where

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

data Void v -- no constructors

run :: Eff Void w -> w
run m = case admin m of Val x -> x

ask :: Eff (Reader e) e
ask = send Reader


runReader :: forall e w. Eff (Reader e) w -> e -> Eff Void w
runReader m e = loop (admin m) where
  loop :: VE w (Reader e) -> Eff Void w
  loop (Val x) = return x
  -- requires ScopedTypeVariables
  loop (E (Reader k)) = loop (k e) 


add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)

t1 :: Eff (Reader Int) Int
t1 = ask `add` return (1:: Int)

t1r :: Eff Void Int
t1r = runReader t1 10
