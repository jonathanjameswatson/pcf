# pcf

An embedding of PCF (Programming Computable Functions) in Haskell.

This library includes an expression GADT that is only well-typed for well-typed PCF terms. The following is a few example PCF terms that can be printed and evaluated with this library:

```hs
succZero, predZero :: ClosedExpr Int
succZero = Succ Zero
predZero = Pred Zero

identity :: ClosedExpr (Int -> Int)
identity = lambda @"x" @Int (var @"x")

project2, project2Shadow :: ClosedExpr (Int -> Int -> Int)
project2 = lambda @"x" @Int (lambda @"y" @Int (var @"y"))
project2Shadow = lambda @"x" @Int (lambda @"x" @Int (var @"x"))

project2BoolShadow :: ClosedExpr (Int -> Bool -> Bool)
project2BoolShadow = lambda @"x" @Int (lambda @"x" @Bool (var @"x"))

multiplyByTwo :: ClosedExpr (Int -> Int)
multiplyByTwo =
  Fix
    ( lambda @"f" @(Int -> Int)
        ( lambda @"x" @Int
            ( If
                (IsZero (var @"x"))
                Zero
                (Succ $ Succ $ Apply (var @"f") (Pred $ var @"x"))
            )
        )
    )
```

```txt
-- succZero --
  [[ succ(0) ]]
  = 1
-- predZero --
  [[ pred(0) ]]
  = 0

-- identity --
  [[ λx. x ]]
    1
  = 1

-- project2 --
  [[ λx. λy. y ]]
    1 2
  = 2
-- project2Shadow --
  [[ λx. λx. x ]]
    1 2
  = 2
-- project2BoolShadow --
  [[ λx. λx. x ]]
    1 True
  = True

-- multiplyByTwo --
  [[ fix(λf. λx. if zero(x) then 0 else succ(succ((f) (pred(x))))) ]]
    5
  = 10
```
