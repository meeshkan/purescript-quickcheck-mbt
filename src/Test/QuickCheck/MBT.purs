module Test.QuickCheck.MBT (Env(..), testModel, Result(..), TestModelOptions) where

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

-- | A typeclass representing the result of an effectful computation done by the system under test.
-- |
-- | This is a wrapper arround `Effect`, so if you want to promote the result of an effectul computation
-- | to env, just use `(Env res)`.
newtype Env a
  = Env (Effect a)

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

-- | A custom `Result` type replacing `Result` from QuickCheck.
-- |
-- | Often when doing model-based testing, it is useful to have rich information of both
-- | success and failure outcomes. QuickCheck's result is pretty limited in its reporting
-- | facilities and is best suited to simple unit tests.
type Result model command result
  = { success :: Boolean
    , initialValue :: Int
    , model :: model
    , commands :: List command
    , mockResults :: List result
    , realResults :: List result
    }

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
        $ map
            ( \( Tuple
                  rres
                  (Tuple mres cmd)
              ) →
                postcondition cmd mres rres
            )
            (zip realResults (zip mockResults commands))
  teardown initialValue
  pure
    $ { success
      , initialValue
      , model
      , commands
      , mockResults
      , realResults
      }

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
  res ←
    env2Effect
      $ sequence
          ( map
              ( \c →
                  runStateMachineOnce
                    setup
                    teardown
                    initializer
                    mock
                    sut
                    postcondition
                    incoming.initialValue
                    incoming.model
                    c
              )
              (shrinker incoming.commands)
          )
  maybe (pure incoming)
    ( shrink setup
        teardown
        initializer
        shrinker
        mock
        sut
        postcondition
    )
    (head (filter (\v → not v.success) res))

-- | Options passed to `testModel`
-- |
-- | There are a lot, and it's pretty ugly. Sorry.
-- | - seed ∷ seed used to seed the quickCheck generator
-- | - nres ∷ the number of results to generate
-- | - setup ∷ used to set up the SUT before testing
-- | - teardown ∷ used to tear down the SUT after testing
-- | - sutInitializer ∷ used to initialize the SUT before a test
-- | - initialModelGenerator ∷ QuickCheck generator used to generate the initial model that kicks off the test and is consumed by the `sutInitializer`
-- | - commandListGenerator ∷ generator for the list of commands going to the mock and SUT
-- | - commandShrinker ∷ a shrinker a la QuickCheck of the command list
-- | - mock ∷ a mock of the model
-- | - sut ∷ the sut
-- | - postcondition ∷ the postcondition to validate after each command is run, accepting the command, the mock result, and the real result
type TestModelOptions model command result
  = { seed ∷ Int
    , nres ∷ Int
    , setup ∷ (Int → Env Unit)
    , teardown ∷ (Int → Env Unit)
    , sutInitializer ∷ (model → Env Unit)
    , initialModelGenerator ∷ (Gen model)
    , commandListGenerator ∷ (Gen (List command))
    , commandShrinker ∷ ((List command) → (List (List command)))
    , mock ∷ (model → command → (Tuple model result))
    , sut ∷ (command → Env result)
    , postcondition ∷ (command → result → result → Boolean)
    }

-- | Test a model
testModel ∷
  ∀ model command result.
  TestModelOptions model command result →
  Effect (List (Result model command result))
testModel opt = do
  res ← env2Effect $ sequence (evalGen (replicateA opt.nres g) { newSeed: (mkSeed opt.seed), size: 10 })
  sequence
    $ map
        ( \r →
            if (r.success) then
              pure r
            else
              shrink
                opt.setup
                opt.teardown
                opt.sutInitializer
                opt.commandShrinker
                opt.mock
                opt.sut
                opt.postcondition
                r
        )
        res
  where
  g ∷ Gen (Env (Result model command result))
  g = do
    i ← arbitrary
    model ← opt.initialModelGenerator
    commands ← opt.commandListGenerator
    pure
      $ ( runStateMachineOnce
            opt.setup
            opt.teardown
            opt.sutInitializer
            opt.mock
            opt.sut
            opt.postcondition
            i
            model
            commands
        )
