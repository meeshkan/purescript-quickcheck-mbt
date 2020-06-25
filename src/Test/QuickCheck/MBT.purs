module Test.QuickCheck.MBT (testModel, Result(..), TestModelOptions, Outcome(..)) where

import Prelude
import Effect.Aff (Aff)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.List (zip, List(..), (:), filter, head)
import Data.Maybe (maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Random.LCG (mkSeed)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, evalGen)

newtype Outcome a b
  = Outcome (Either a b)

derive newtype instance functorOutcome ∷ Functor (Outcome a)

derive newtype instance applyOutcome ∷ Apply (Outcome a)

derive newtype instance applicativeOutcome ∷ Applicative (Outcome a)

instance semigroupOutcome ∷ Semigroup b => Semigroup (Outcome a b) where
  append (Outcome a) (Outcome b) = Outcome (a <> b)

instance monoidOutcome ∷ Monoid b => Monoid (Outcome a b) where
  mempty = Outcome (Right mempty)

mockMap ∷
  ∀ model command result.
  (model → command → (Tuple model result)) → -- mock
  model → -- model
  List command → -- commands
  List result -- results
mockMap mock model Nil = Nil

mockMap mock model (x : xs) = let (Tuple newModel res) = mock model x in res : (mockMap mock newModel xs)

sutMap ∷ ∀ command result. (command → Aff result) → List command → Aff (List result)
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
type Result model command result failure success
  = { outcome :: Outcome failure success
    , initialValue :: Int
    , model :: model
    , commands :: List command
    , mockResults :: List result
    , realResults :: List result
    }

runStateMachineOnce ∷
  ∀ model command result failure success.
  Monoid success =>
  (Int → Aff Unit) → -- setup
  (Int → Aff Unit) → -- teardown
  (model → Aff Unit) → -- initializer
  (model → command → (Tuple model result)) → -- mock
  (command → Aff result) → -- system under test
  (command → result → result → Outcome failure success) → -- postcondition
  Int → -- initializer
  model → -- initial model
  List command → -- command list
  Aff (Result model command result failure success)
runStateMachineOnce setup teardown initializer mock sut postcondition initialValue model commands = do
  setup initialValue
  initializer model
  realResults ← sutMap sut commands
  let
    mockResults = mockMap mock model commands
  let
    outcome =
      fold
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
    $ { outcome
      , initialValue
      , model
      , commands
      , mockResults
      , realResults
      }

isFailure ∷ ∀ a b. Outcome a b → Boolean
isFailure (Outcome (Left _)) = true

isFailure (Outcome (Right _)) = false

shrink ∷
  ∀ model command result failure success.
  Monoid success =>
  (Int → Aff Unit) → -- setup
  (Int → Aff Unit) → -- teardown
  (model → Aff Unit) → -- initializer
  ((List command) → (List (List command))) → -- shrinker
  (model → command → (Tuple model result)) → -- mock
  (command → Aff result) → -- system under test
  (command → result → result → Outcome failure success) → -- postcondition
  (Result model command result failure success) → -- inc
  Aff (Result model command result failure success)
shrink setup teardown initializer shrinker mock sut postcondition incoming = do
  res ←
    sequence
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
    (head (filter (\v → isFailure v.outcome) res))

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
type TestModelOptions model command result failure success
  = { seed ∷ Int
    , nres ∷ Int
    , setup ∷ (Int → Aff Unit)
    , teardown ∷ (Int → Aff Unit)
    , sutInitializer ∷ (model → Aff Unit)
    , initialModelGenerator ∷ (Gen model)
    , commandListGenerator ∷ (Gen (List command))
    , commandShrinker ∷ ((List command) → (List (List command)))
    , mock ∷ (model → command → (Tuple model result))
    , sut ∷ (command → Aff result)
    , postcondition ∷ (command → result → result → Outcome failure success)
    }

-- | Test a model
testModel ∷
  ∀ model command result failure success.
  Monoid success =>
  TestModelOptions model command result failure success →
  Aff (List (Result model command result failure success))
testModel opt = do
  res ← sequence (evalGen (replicateA opt.nres g) { newSeed: (mkSeed opt.seed), size: 10 })
  sequence
    $ map
        ( \r →
            if (not (isFailure r.outcome)) then
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
  g ∷ Gen (Aff (Result model command result failure success))
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
