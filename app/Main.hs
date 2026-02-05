module Main (main) where

import Lib
import Data.Set
import Data.Void


data Proc = P1|P2|P3|P4|P5|P6|P7|P8|P9 deriving (Eq, Ord, Show)
data Act = A|B|C deriving (Eq, Ord,Show)


-- procs = [Proc "p1", Proc "p2", Proc "p3", Proc "p4", Proc "p5", Proc "p6", Proc "p7", Proc "p8", Proc "p9"]
testTrans :: Set (Transition Proc Act)
testTrans = fromList
  [ Transition  P1 A P2,
    Transition  P2 C P1,
    Transition  P2 C P2,
    Transition  P1 B P4,
    Transition  P3 A P1,
    Transition  P3 B P1,
    Transition  P4 B P3,
    Transition  P3 C P8,
    Transition  P4 A P5,
    Transition  P4 C P8,
    Transition  P5 B P2,
    Transition  P5 C P2,
    Transition  P5 A P9,
    Transition  P9 A P5,
    Transition  P6 C P2,
    Transition  P6 B P5,
    Transition  P6 A P9,
    Transition  P6 B P6,
    Transition  P8 B P7,
    Transition  P9 C P8,
    Transition  P7 B P7,
    Transition  P9 B P9,
    Transition  P3 A P7,
    Transition  P7 A P3,
    Transition  P1 C P7
  ]

test = ltsFromTrans testTrans

data Varible = X|Y|Z deriving (Bounded, Eq, Enum, Show, Ord)

decl :: Declaration Act Varible
decl X = Max $ Ex [C] TT `And` Ex [A] (Var Y) `And` All [B] (Var X)
decl Y = Max $ Ex [B] (Var X) `And` All acts (Var Y)                          where acts = Data.Set.toList . act $ test
decl Z = Min $ (All [A] (Var Y) `And` Ex [B] (Var X)) `Or` All [B] (Var Z)

main :: IO ()
main = putStrLn . show . fmap groups $ makeSystem (Var Z) decl
