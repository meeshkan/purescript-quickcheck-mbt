module Main where

import Prelude

import Data.Array (fromFoldable)
import Data.List (List(..), (:), unsnoc, length, toUnfoldable, mapWithIndex, deleteAt, filter)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Queue (epush, epop, elng, esetup, eteardown, einit)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)
import Test.QuickCheck.MBT (Env, testModel)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- First, we create a model that represents the system under test
newtype Model
  = Model (List Int)

-- We need to be able to initialize the system with arbitrary models
-- In Purescript, because we already have instances of `Arbitrary Int`
-- and `Arbitrary a ⇒ Arbitrary (List a)`, we can use the derive
-- newtype strategy to create arbitrary models that contain integer lists
-- of arbitrary sizes.
derive newtype instance arbitraryModel ∷ Arbitrary Model

-- This represents a command in stateful testing
-- Here, because we are simulating a FIFO queue
-- we use the three commands push, pop, and length
data Command
  = Push Int
  | Pop
  | GetLength

-- This represents the outcome of a command
-- Note that while outcomes map 1-to-1 to commands, this is
-- not necessary for the general algorithm to work.
-- However, IMO, a 1-to-1 mapping makes things more readable.
data Outcome
  = Pushed
  | Popped (Maybe Int)
  | GotLength Int

-- Because we will be comparing outcomes, we need some way to
-- test for equality. In this case, we'll do a simple equality test.
derive instance eqOutcome ∷ Eq Outcome

-- This is our generator of commands
-- In this case, we generate commands with an even probability
-- If we wanted to skew the probability towards push, we could
-- just add push many times to the array.
instance arbitraryCommand ∷ Arbitrary Command where
  arbitrary = do
    i ← arbitrary
    oneOf $ NonEmpty (pure $ Push i) [ pure Pop, pure GetLength ]


-- This defines the behavior for the Push, Pop and GetLength commands
mock ∷ Model → Command → (Tuple Model Outcome)
mock (Model m) (Push i) = (Tuple (Model (i : m)) Pushed)

mock (Model m) Pop =
  let
    s = unsnoc m
  in
    maybe (Tuple (Model Nil) (Popped Nothing))
      (\x → Tuple (Model x.init) (Popped $ Just x.last))
      s

mock mm@(Model m) GetLength = Tuple mm (GotLength $ length m)

-- The postcondition here is simple - we just verify that the mocked outcome
-- is equal to the real outcome. This won't always work, ie if a server generates
-- UUIDs. In that case, we'll need some other comparison, but for here, simple
-- equality comparison works
postcondition ∷ Command → Outcome → Outcome → Boolean
postcondition _ r0 r1 = r0 == r1

-- This initializes the system under test with a given model
-- This is done ie if we want to start the DB in a given state
-- Here, it will initialize the queue to a pre-existing queue of arbitrary length
initializer ∷ Model → Env Unit
initializer (Model l) = do
  let
    arr = toUnfoldable l ∷ Array Int
  einit (fromFoldable l)

-- This is the system under test
-- It uses the Queue.purs implementation, which uses Queue.py under the hood
sut ∷ Command → Env Outcome
sut (Push i) = do
  epush i
  pure Pushed

sut Pop = Popped <$> epop

sut GetLength = GotLength <$> elng

-- we use a naive shrinker
shrinker ∷ ∀ a. List a → List (List a)
shrinker c = mapWithIndex (\i _ → fromMaybe Nil (deleteAt i c)) c

-- This is the main function that does the PBT and shows the results
-- It should show { failures: Nil, successes: 100, total: 100 }
-- To run more than 100 tests, change the number 100 below
main ∷ Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "checkStateMachine" do
    it "works" do
      res ← liftEffect $ testModel {
        seed: 0,
        nres: 100,
        setup: esetup,
        teardown: eteardown,
        modelInitializer: initializer,
        initialModelGenerator: arbitrary,
        commandListGenerator: arbitrary,
        commandShrinker: shrinker,
        mock, sut, postcondition }
      100 `shouldEqual` (length $ filter (\r → r.success) res)
