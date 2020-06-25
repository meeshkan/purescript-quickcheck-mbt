# QuickCheck MBT

A model-based testing framework inspired by [quick-check-state-machine](https://github.com/advancedtelematic/quickcheck-state-machine).

## How to use

First, we create a (hopefully simple) model capable of representing the persistance layer of the system under test.

```purescript
newtype Model
  = Model (List Int)
```

We need to be able to initialize the system with arbitrary models. Because we already have instances of `Arbitrary Int` and `Arbitrary a ⇒ Arbitrary (List a)`, we can use the derive newtype strategy to create arbitrary models that contain integer lists of arbitrary sizes.

```purescript
derive newtype instance arbitraryModel ∷ Arbitrary Model
```

Now that the model is defined, we need to represent the form of commands that can be generated. Here, because we are simulating a FIFO queue we use the three commands push, pop, and length.

```purescript
data Command
  = Push Int
  | Pop
  | GetLength
```

The last data class we need to define is the result of a command, which we'll call `Response`. Note that, while responses map 1-to-1 to commands, this is not necessary for the underlying algorithm to work.

```purescript
data Response
  = Pushed
  | Popped (Maybe Int)
  | GotLength Int

derive instance eqResponse ∷ Eq Response
```

This is our generator of commands that will used to create test cases. In this case, we generate commands with an even probability. Note that, unlike other stateful PBT libraries, this library does _not_ use a separate function to impose preconditions. If there are preconditions, you need to include them in the generator of commands that is passed to `testModel`.

```purescript
instance arbitraryCommand ∷ Arbitrary Command where
  arbitrary = do
    i ← arbitrary
    oneOf $ NonEmpty (pure $ Push i) [ pure Pop, pure GetLength ]
```

Then, we define a mock of the system under test.

```purescript
mock ∷ Model → Command → (Tuple Model Response)
mock (Model m) (Push i) = (Tuple (Model (i : m)) Pushed)
mock (Model m) Pop =
  let
    s = unsnoc m
  in
    maybe (Tuple (Model Nil) (Popped Nothing))
      (\x → Tuple (Model x.init) (Popped $ Just x.last))
      s
mock mm@(Model m) GetLength = Tuple mm (GotLength $ length m)
```

We then need to define a postcondition. The postcondition here is simple - we just verify that the mocked response is equal to the real response. This won't always work, ie if a server generates UUIDs. In that case, we'll need some other comparison, but for here, simple equality comparison works.

```purescript
postcondition ∷ Command → Response → Response → (Outcome String String)
postcondition _ r0 r1 = if r0 == r1 then (pure "passed") else (pure "failed")
```

This functino is used to initialize the system under test with a given model. This is used, for example, if you need to initialze a DB in the system under test. Note that the `Env` monad is just a `newtype` around `Effect`.

```purescript
initializer ∷ Model → Env Unit
initializer (Model l) = do
  let
    arr = toUnfoldable l ∷ Array Int
  einit (fromFoldable l)
```

This is the system under test. It uses a file called [./test/Queue.purs](./test/Queue.purs) that, under the hood, uses JavaScript via the FFI to represent a FIFO queue.

```purescript
sut ∷ Command → Env Response
sut (Push i) = do
  epush i
  pure Pushed
sut Pop = Popped <$> epop
sut GetLength = GotLength <$> elng
```

We use a simple shrinker to shrink commands.

```purescript
shrinker ∷ ∀ a. List a → List (List a)
shrinker c = mapWithIndex (\i _ → fromMaybe Nil (deleteAt i c)) c
```

This is the main function that does the PBT and validates the results.

```purescript
isSuccess ∷ ∀ a b. Outcome a b → Boolean
isSuccess (Outcome (Left _)) = false

isSuccess (Outcome (Right _)) = true

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
      100 `shouldEqual` (length $ filter (\r → isSuccess r.outcome) res)
```

Check out the [test](./test/Main.purs) for a full working example.
