# Knode

## Running Commands

Use `devbox run` to execute all commands:

```bash
devbox run stack build
devbox run stack test
devbox run stack ghci
```

## Verification

Always verify changes compile before reporting completion:

```bash
devbox run stack build
```

---

## Code Review Requirements

Reviews must check ALL of the following. Do not skip any check. Flag violations with high confidence.

### Three-Layer Cake Architecture

This project follows the three-layer cake pattern. Every review must verify layer separation:

**Layer 1 (Imperative shell)**: IO, resources, typeclass instances, error boundary
**Layer 2 (Business logic)**: AppM, polymorphic over capabilities, uses MonadError
**Layer 3 (Pure core)**: Domain types, pure validation, Either returns

### Mandatory Checks

#### 1. No `liftIO` in business code
Business functions (Layer 2) must call capability methods, never `liftIO` directly. A `liftIO` in Layer 2 indicates a missing capability.

```haskell
-- VIOLATION: liftIO in business code
completeTodo tid = do
  liftIO (putStrLn ("completing " ++ show tid))  -- belongs behind MonadLogger
  ...

-- CORRECT: use capability
completeTodo tid = do
  logInfo ("completing " <> T.pack (show tid))
  ...
```

#### 2. No DB types in business signatures
`Connection`, `Row`, `Statement`, `Pool` — these are Layer 1 types. They must not appear in Layer 2 function signatures. Convert to domain types in capability instances.

```haskell
-- VIOLATION: DB type in business signature
fetchUser :: Connection -> Int -> AppM User

-- CORRECT: polymorphic over capability
fetchUser :: (MonadUserDb m, MonadError AppError m) => Int -> m User
```

#### 3. No `Either` returns from capability methods
Capability methods must not return `Either`. Use `MonadError` instead — that's the whole point.

```haskell
-- VIOLATION: Either return
class MonadTodoDb m where
  findTodo :: Int -> m (Either AppError (Maybe Todo))

-- CORRECT: use MonadError
class Monad m => MonadTodoDb m where
  findTodo :: Int -> m (Maybe Todo)  -- throws via MonadError on infra failure
```

#### 4. Exceptions caught at instance boundary, not in business code
Business code must never catch exceptions. The boundary is the capability instance, not the business function.

```haskell
-- VIOLATION: catching in business code
completeTodo tid = do
  result <- liftIO (try (someAction tid))
  ...

-- CORRECT: catch at instance boundary via tryIO/withDb
```

#### 5. No `Other Text` escape hatch in AppError
`AppError` must have specific constructors, not a catch-all `Other Text`. If 80%+ of errors become `Other`, it's a code smell.

```haskell
-- VIOLATION: escape hatch
data AppError = NotFound Int | Other Text

-- CORRECT: specific constructors
data AppError = NotFound Int | InvalidTitle Text | AlreadyCompleted Int | DbError Text
```

#### 6. Pure validation uses Either, not MonadError
Layer 3 (pure core) functions return `Either AppError a`, not a monadic type. Bridge to Layer 2 with `liftEither'`.

```haskell
-- CORRECT: pure validation
validateTitle :: Text -> Either AppError Text

-- CORRECT: bridging in business code
createTodo raw = do
  title <- liftEither' (validateTitle raw)
  insertTodo title
```

#### 7. Multi-step operations have rollback strategy
Business functions performing multiple side effects (DB writes, external calls) without rollback leave the system inconsistent on partial failure. Flag `doStepA >> doStepB` patterns where partial completion corrupts state.

#### 8. Polymorphic business signatures
Business functions must be polymorphic in `m`, constrained by exactly the capabilities they use:

```haskell
-- CORRECT: polymorphic, minimal constraints
createTodo
  :: (MonadTodoDb m, MonadLogger m, MonadError AppError m)
  => Text -> m Todo
```

#### 9. No mixing error tracks
If using `MonadError AppError`, do not also throw custom exception types from business code. Pick one error channel.

#### 10. Manual Either unwrapping is an anti-pattern
Use `liftEither'`, not manual case analysis:

```haskell
-- VIOLATION: manual unwrapping
createTodo raw = do
  case validateTitle raw of
    Left err    -> throwError err
    Right title -> insertTodo title

-- CORRECT: use liftEither'
createTodo raw = liftEither' (validateTitle raw) >>= insertTodo
```

### Type Self-Consistency

A type's methods must be correct on their own. Their contracts hold regardless of who calls them, in what order, from what context. Before writing any public method, verify: "Does this method's contract hold no matter who calls it?"

### Review Thoroughness

- Check ALL modified Haskell files against the above criteria
- Trace capability usage through call chains to verify layer separation
- Do not dismiss architectural violations as "minor" — they compound
- When unsure if something is a violation, flag it for discussion
