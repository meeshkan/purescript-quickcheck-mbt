module Queue (epush, epop, elng, esetup, eteardown, einit) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.QuickCheck.MBT (Env(..))

foreign import push :: Int -> Effect Unit

foreign import pop :: Effect (Array Int)

foreign import lng :: Effect Int

foreign import setup :: Int -> Effect Unit

foreign import teardown :: Int -> Effect Unit

foreign import init :: Array Int -> Effect Unit

epush :: Int -> Env Unit
epush = Env <<< push

epop :: Env (Maybe Int)
epop = Env do
  l <- pop
  pure (if (Array.length l == 0) then Nothing else (Array.(!!) l 0))

elng :: Env Int
elng = Env lng

esetup :: Int -> Env Unit
esetup = Env <<< setup

eteardown :: Int -> Env Unit
eteardown = Env <<< teardown

einit :: Array Int -> Env Unit
einit = Env <<< init
