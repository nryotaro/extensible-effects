{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

module Lib2 () where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Typeable

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
runReader m e = loop (admin m)
  where
    loop :: VE w (Reader e) -> Eff Void w
    loop (Val x) = return x
    -- requires ScopedTypeVariables
    loop (E (Reader k)) = loop (k e)

add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)

t1 :: Eff (Reader Int) Int
t1 = ask `add` return (1 :: Int)

t1r :: Eff Void Int
t1r = runReader t1 10

infixr 1 |>
data (a :: * -> *) |> b


data Union r v where
  Union :: (Functor t, Typeable t) => Id (t v) -> Union r v

newtype Id x = Id x

instance Functor (Union r) where
  -- (a -> b) -> f a -> f b
  fmap ab (Union (Id x)) = Union (Id (fmap ab x))

class Member (t :: * -> *) r

instance Member t (t |> r)

instance Member t r => Member t (t' |> r)

inj :: (Functor t, Typeable t, Member t r) => t v -> Union r v
inj x = Union (Id x)

prj :: (Functor t, Typeable t, Member t r) => Union r v -> Maybe (t v)
prj (Union v) | Just (Id x) <- gcast1 v = Just x
prj _ = Nothing

decomp :: Typeable t => Union (t |> r) v -> Either (Union r v) (t v)
decomp (Union v) | Just (Id x) <- gcast1 v = Right x
decomp (Union v) = Left (Union v)
