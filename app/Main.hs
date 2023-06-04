{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Pcf
import Text.Printf

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

evaluate :: (Show b) => String -> (ClosedExpr a) -> [String] -> (a -> b) -> IO ()
evaluate name expr arguments f = do
  putStrLn $ printf "-- %s --" name
  putStrLn $ printf "  [[ %s ]]" (showExpr expr)
  case arguments of
    [] -> return ()
    _ -> putStrLn $ printf "    %s" (unwords arguments)
  putStrLn $ printf "  = %s" (show $ f $ eval expr)

main :: IO ()
main = do
  evaluate "succZero" succZero [] id
  evaluate "predZero" predZero [] id
  putStrLn ""
  evaluate "identity" identity ["1"] ($ 1)
  putStrLn ""
  evaluate "project2" project2 ["1", "2"] (($ 2) . ($ 1))
  evaluate "project2Shadow" project2Shadow ["1", "2"] (($ 2) . ($ 1))
  evaluate
    "project2BoolShadow"
    project2BoolShadow
    ["1", "True"]
    (($ True) . ($ 1))
  putStrLn ""
  evaluate "multiplyByTwo" multiplyByTwo ["5"] ($ 5)
