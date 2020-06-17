module Test.QuickCheck.MBT (Env(..), checkStateMachine, Result(..)) where

import Prelude

import Data.List (zip, List(..), (:), foldl, filter, head)
import Data.Maybe (maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Random.LCG (mkSeed)
import Test.QuickCheck (class Testable, arbitrary, test)
import Test.QuickCheck.Gen (Gen, evalGen)

newtype Env a = Env (Effect a)

derive newtype instance functorEnv ∷ Functor Env
derive newtype instance applyEnv ∷ Apply Env
derive newtype instance applicativeEnv ∷ Applicative Env
derive newtype instance bindEnv ∷ Bind Env
derive newtype instance monadEnv ∷ Monad Env

instance testableEnv ∷ Testable prop ⇒ Testable (Env prop) where
  test (Env prop) = test (unsafePerformEffect prop)

mockMap ∷
  ∀ model command result.
  (model → command → (Tuple model result)) → -- mock
  model → -- model
  List command → -- commands
  List result -- results
mockMap mock model Nil = Nil

mockMap mock model (x : xs) = let (Tuple newModel res) = mock model x in res : (mockMap mock newModel xs)

sutMap ∷ ∀ command result. (command → Env result) → List command → Env (List result)
sutMap c Nil = pure Nil

sutMap c (x : xs) = do
  r ← c x
  rr ← sutMap c xs
  pure (r : rr)

type Result model command result = {
  success :: Boolean, initialValue :: Int, model :: model, commands :: List command, mockResults :: List result, realResults :: List result }

env2Effect ∷ ∀ a. Env a → Effect a
env2Effect (Env a) = a

runStateMachineOnce ∷
  ∀ model command result.
  (Int → Env Unit) → -- setup
  (Int → Env Unit) → -- teardown
  (model → Env Unit) → -- initializer
  (model → command → (Tuple model result)) → -- mock
  (command → Env result) → -- system under test
  (command → result → result → Boolean) → -- postcondition
  Int → -- initializer
  model → -- initial model
  List command → -- command list
  Env (Result model command result)
runStateMachineOnce setup teardown initializer mock sut postcondition initialValue model commands = do
    setup initialValue
    initializer model
    realResults ← sutMap sut commands
    let
      mockResults = mockMap mock model commands
    let
      success =
        foldl
          (\a b → a && b)
          true
          $ map (\(Tuple rres (Tuple mres cmd)) → postcondition cmd mres rres) (zip realResults (zip mockResults commands))
    teardown initialValue
    pure $ {success, initialValue, model, commands, mockResults, realResults}


shrink ∷
  ∀ model command result.
  (Int → Env Unit) → -- setup
  (Int → Env Unit) → -- teardown
  (model → Env Unit) → -- initializer
  ((List command) → (List (List command))) → -- shrinker
  (model → command → (Tuple model result)) → -- mock
  (command → Env result) → -- system under test
  (command → result → result → Boolean) → -- postcondition
  (Result model command result) → -- inc
  Effect (Result model command result)
shrink setup teardown initializer shrinker mock sut postcondition incoming = do
  res ← env2Effect $ sequence (map (\c → runStateMachineOnce setup teardown initializer mock sut postcondition incoming.initialValue incoming.model c) (shrinker incoming.commands))
  maybe (pure incoming) (shrink setup teardown initializer shrinker mock sut postcondition) (head (filter (\v → not v.success) res))

checkStateMachine ∷
  ∀ model command result.
  Int → -- seed
  Int → -- n res
  (Int → Env Unit) → -- setup
  (Int → Env Unit) → -- teardown
  (model → Env Unit) → -- initializer
  (Gen model) → -- gen model
  (Gen (List command)) → -- gen commands
  ((List command) → (List (List command))) → -- shrinker
  (model → command → (Tuple model result)) → -- mock
  (command → Env result) → -- system under test
  (command → result → result → Boolean) → -- postcondition
  Effect (List (Result model command result))
checkStateMachine seed nres setup teardown initializer genModel genCommands shrinker mock sut postcondition = do
  res ← env2Effect $ sequence (evalGen (replicateA nres g) { newSeed: (mkSeed seed), size: 10 })
  sequence $ map (\r → if (r.success) then pure r else shrink setup teardown initializer shrinker mock sut postcondition r) res
  where
  g ∷ Gen (Env (Result model command result))
  g = do
    i ← arbitrary
    model ← genModel
    commands ← genCommands
    pure $ (runStateMachineOnce setup teardown initializer mock sut postcondition i model commands)
