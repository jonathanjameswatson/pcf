{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module                  : Pcf
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (GHC)
--
-- Module for embedding PCF in Haskell.
module Pcf
  ( -- * Expressions
    ClosedExpr,
    Expr
      ( Zero,
        Succ,
        Pred,
        ETrue,
        EFalse,
        IsZero,
        Var,
        If,
        Lambda,
        Apply,
        Fix,
        VarLit
      ),
    var,
    lambda,

    -- * Evaluation
    eval,

    -- * Printing
    showExpr,
  )
where

import Data.Data
import Data.Function (fix)
import Data.Kind (Type)
import GHC.TypeLits
import GHC.TypeLits.Singletons
import Text.Printf
import Unsafe.Coerce

type family Lookup (b :: [(Symbol, Type)]) (s :: Symbol) :: Type where
  Lookup '[] s =
    TypeError ('Text "Symbol " ':<>: 'ShowType s ':<>: 'Text " not found.")
  Lookup ('(s, t) ': xs) s = t
  Lookup ('(s, t) ': xs) s' = Lookup xs s'

-- | The type of a potentially open PCF term.
data Expr (b :: [(Symbol, Type)]) a where
  Zero :: Expr b Int
  Succ :: Expr b Int -> Expr b Int
  Pred :: Expr b Int -> Expr b Int
  ETrue :: Expr b Bool
  EFalse :: Expr b Bool
  IsZero :: Expr b Int -> Expr b Bool
  Var :: (Lookup (b ': bs) s ~ a) => SSymbol s -> Expr (b ': bs) a
  If :: Expr b Bool -> Expr b a -> Expr b a -> Expr b a
  Lambda ::
    SSymbol s -> Proxy (c :: Type) -> Expr ('(s, c) ': b) a -> Expr b (c -> a)
  Apply :: Expr b (v -> a) -> Expr b v -> Expr b a
  Fix :: Expr b (a -> a) -> Expr b a
  VarLit :: a -> Expr (b ': bs) a -- For internal use only

-- | The type of a closed PCF term.
type ClosedExpr a = Expr '[] a

-- | Smart constructor for variables.
var ::
  forall s b bs a. (Lookup (b ': bs) s ~ a, KnownSymbol s) => Expr (b ': bs) a
var = Var (SSym @s)

-- | Smart constructor for lambdas.
lambda ::
  forall s c b a. (KnownSymbol s) => Expr ('(s, c) ': b) a -> Expr b (c -> a)
lambda = Lambda (SSym @s) (Proxy :: Proxy c)

showSSymbol :: SSymbol s -> String
showSSymbol (s :: SSymbol s) = withKnownSymbol s (symbolVal (Proxy :: Proxy s))

class TestEquality f where
  testEquality :: f a -> f b -> Maybe (a :~: b)

instance TestEquality SSymbol where
  testEquality x y =
    if showSSymbol x == showSSymbol y
      then Just (unsafeCoerce Refl)
      else Nothing

subst ::
  Expr b1 a ->
  SSymbol s ->
  Lookup b1 s ->
  Expr b1 a
subst Zero _ _ = Zero
subst (Succ e) v x = Succ (subst e v x)
subst (Pred e) v x = Pred (subst e v x)
subst ETrue _ _ = ETrue
subst EFalse _ _ = EFalse
subst (IsZero e) v x = IsZero (subst e v x)
subst (Var v') v x =
  case testEquality v v' of
    Just Refl -> VarLit x
    Nothing -> Var v'
subst (If e1 e2 e3) s x = If (subst e1 s x) (subst e2 s x) (subst e3 s x)
subst (Lambda v' t e) v x =
  case testEquality v v' of
    Just Refl -> Lambda v t e
    Nothing -> Lambda v' t (subst e v $ unsafeCoerce x)
subst (Apply e1 e2) s x = Apply (subst e1 s x) (subst e2 s x)
subst (Fix e) s x = Fix (subst e s x)
subst (VarLit x) _ _ = VarLit x

evalOpen :: Expr b a -> a
evalOpen Zero = 0
evalOpen (Succ e) = 1 + evalOpen e
evalOpen (Pred e) = max 0 $ evalOpen e - 1
evalOpen ETrue = True
evalOpen EFalse = False
evalOpen (IsZero e) = evalOpen e == 0
evalOpen (Var v) =
  error $
    printf
      "Variable %s should not exist after substitution for a well-typed expression."
      (showSSymbol v)
evalOpen (If e1 e2 e3) = if evalOpen e1 then evalOpen e2 else evalOpen e3
evalOpen (Lambda v (_ :: Proxy c) e) = \(x :: c) -> evalOpen $ subst e v x
evalOpen (Apply e1 e2) = evalOpen e1 $ evalOpen e2
evalOpen (VarLit x) = x
evalOpen (Fix e) = fix $ evalOpen e

-- | Evaluate a closed PCF term.
eval :: ClosedExpr a -> a
eval = evalOpen

-- | Show a PCF term.
showExpr :: Expr b a -> String
showExpr Zero = "0"
showExpr (Succ e) = printf "succ(%s)" (showExpr e)
showExpr (Pred e) = printf "pred(%s)" (showExpr e)
showExpr ETrue = "true"
showExpr EFalse = "false"
showExpr (IsZero e) = printf "zero(%s)" (showExpr e)
showExpr (Var v) = showSSymbol v
showExpr (If e1 e2 e3) =
  printf "if %s then %s else %s" (showExpr e1) (showExpr e2) (showExpr e3)
showExpr (Lambda v (_ :: Proxy c) e) =
  printf "λ%s. %s" (showSSymbol v) (showExpr e)
showExpr (Apply e1 e2) = printf "(%s) (%s)" (showExpr e1) (showExpr e2)
showExpr (Fix e) = printf "fix(%s)" (showExpr e)
showExpr (VarLit _) = "VarLit"
