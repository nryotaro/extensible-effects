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

data VE w = Val w | E (Int -> VE w)

newtype Eff a = Eff
  { runEff :: forall w. (a -> VE w) -> VE w
  }

ask :: Eff Int
ask = Eff (\k -> E k)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
