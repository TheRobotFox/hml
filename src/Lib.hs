{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
(Transition(Transition), LTS(LTS, proc, act, trans), ltsFromTrans, HML(Ex,All,And,Or,TT,FF,Var), Fixpoint(Min, Max), Declaration, makeSystem, solveSystem)
  where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Set (Set, union, intersection)
import qualified Data.Set (Set, fromList, map, singleton, unions, empty, partition, difference, intersection, filter, union, toList)
import Control.Applicative ( liftA3 )
import Data.Functor.Foldable (Recursive(cata))
import Debug.Trace (traceShow, traceShowId, trace)
import Control.Monad.Writer
import Control.Monad (foldM)
import Data.List (intercalate)
import qualified Data.Map
import Data.Map ((!))
import Data.Function (on)

data Transition p a = Transition {from :: p, producing :: a, to :: p} deriving (Show, Eq, Ord)

data LTS p a = LTS {proc :: Set p, act:: Set a, trans :: Set (Transition p a)} deriving (Show)

ltsFromTrans :: (Ord p, Ord a) => Set (Transition p a) -> LTS p a
ltsFromTrans = liftA3 LTS (Data.Set.fromList . concatMap getProcs) (Data.Set.map getActs) id
  where getProcs (Transition a _ b) = [a,b]
        getActs (Transition _ a _) = a

data HML a v = Ex [a] (HML a v) | All [a] (HML a v) | And (HML a v) (HML a v) | Or (HML a v) (HML a v) | TT | FF | Var v
makeBaseFunctor ''HML



setTeX :: Show a => Set a -> [Char]
setTeX s = "\\{"++ (intercalate ", " . map show . Data.Set.toList $ s) ++"\\}"

hmlTeX :: (Show a, Show v) => HML a v -> String
hmlTeX  = cata p
  where p (VarF s)      = show s
        p (AllF acts s) = "\\All{" ++ intercalate ", " (map show acts) ++ "}" ++ s
        p (ExF acts s)  = "\\Ex{"  ++ intercalate ", " (map show acts) ++ "}" ++ s
        p (AndF a b)    = a ++ " \\land " ++ b
        p (OrF a b)     = a ++ " \\lor " ++ b
        p TTF = "\\tt"
        p FFF = "\\ff"

hmlETeX :: (Show a, Show p) => HML a (Set p) -> String
hmlETeX  = cata p
    where
      p (VarF s)      = setTeX $ s
      p (AllF acts s) = "\\All[\\cdot]{" ++ intercalate ", " (map show acts) ++ "}" ++ s
      p (ExF acts s)  = "\\Ex[\\cdot]{"  ++ intercalate ", " (map show acts) ++ "}" ++ s
      p (AndF a b)    = a ++ " \\cap " ++ b
      p (OrF a b)     = a ++ " \\cup " ++ b
      p TTF = "Proc"
      p FFF = "\\emptyset"


data Fixpoint a = Max a | Min a deriving (Show, Eq)
type Declaration a v = v -> Fixpoint (HML a v)

data HMLEquation a v = HMLEquation {f :: HML a v, decl :: v -> HML a v, groups :: [Fixpoint (Set v)]}

getDependencies ::  Ord v => HML a v -> Set v
getDependencies = cata collectVars
  where collectVars (VarF var) = Data.Set.singleton var
        collectVars (AllF _ f) = f
        collectVars (ExF _ f)  = f
        collectVars (AndF a b) =  a `union` b
        collectVars (OrF a b)  =  a `union` b
        collectVars _          = Data.Set.empty


fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a = let r = f a in if a==r then a else fixpoint f r

fixpointM :: (Monad m, Eq a) => (a -> m a) -> a -> m a
fixpointM f a = do r <- f a
                   if r == a then return r else fixpointM f r

getHML :: (t -> Fixpoint a) -> t -> a
getHML declFn var = case declFn var of
      (Max h) -> h
      (Min h) -> h

makeGroups :: Ord v => Declaration a v -> Set v -> Maybe [Fixpoint (Set v)]
makeGroups declFn initF = let (gs, vs) = fixpoint extract ([], reachable initF) in
   if null vs then Just $ reverse gs else Nothing
  where
    extract (gs, vs) = let (vMax, vMin) = Data.Set.partition (isMax . declFn) vs
                           (iMax, iMin) = (independent vMin vMax, independent vMax vMin)
                           independent from = Data.Set.filter (null . Data.Set.intersection from . reachable . Data.Set.singleton)
                        in case (not $ null iMax, not $ null iMin) of
                         (True, _) ->  (Max iMax:gs, Data.Set.difference vs iMax)
                         (_, True) ->  (Min iMin:gs, Data.Set.difference vs iMin)
                         _ -> (gs, vs)
        
    isMax (Max _) = True
    isMax _ = False
    reachable = fixpoint (union <*> Data.Set.unions . Data.Set.map (getDependencies . getHML declFn))

makeSystem :: Ord v => HML a v -> Declaration a v -> Maybe (HMLEquation a v)
makeSystem formular declFn = fmap (HMLEquation formular (getHML declFn)) . makeGroups declFn . getDependencies $ formular

substVars :: (v -> b) -> HML a v -> HML a b
substVars fn = cata subst
  where
    subst (VarF x)   = Var . fn $ x
    subst (AllF a b) = All  a b
    subst (ExF  a b) = Ex   a b
    subst (AndF a b) = And  a b
    subst (OrF  a b) = Or   a b
    subst  TTF       = TT
    subst  FFF       = FF

solveHML :: (Eq a, Ord p) =>  LTS p a -> HML a (Set p) -> Set p
solveHML (LTS ps _ ts) = cata eval
  where
    eval (VarF p) = p
    eval (AllF acts p) = Data.Set.difference ps . Data.Set.map from . Data.Set.filter (liftA2 (&&) ((`elem` badStates) . to) ((`elem` acts) . producing))
                           $ ts where badStates = Data.Set.difference ps p
    eval (ExF acts s) = Data.Set.map from . Data.Set.filter (liftA2 (&&) ((`elem` s) . to) ((`elem` acts) . producing)) $ ts
    eval (AndF a b) = a `intersection` b
    eval (OrF a b) = a `union` b
    eval TTF = ps
    eval FFF = Data.Set.empty

solveSystem :: (Eq a, Ord v, Ord p, Show p, Show a, Show v) => LTS p a -> HMLEquation a v -> String
solveSystem lts (HMLEquation form decl gs) = execWriter . foldM (\e -> liftA2 solveGroup Data.Map.keys (Data.Map.union e) . initialEnv) Data.Map.empty $ gs
  where
    solveGroup vs =
      mapWriter (liftA2 (,) fst (uncurry $ format vs))
        . fixpointM
          ( \env ->
              let evaled = Data.Map.fromList $ zip <*> map (solveHML lts . substVars (env !) . decl) $ vs
               in writer (Data.Map.union evaled env, [on (,) Data.Map.elems (env `Data.Map.intersection` evaled) evaled])
          )

    initialEnv (Max vs) = Data.Map.fromList . zip (Data.Set.toList vs) . repeat . proc $ lts
    initialEnv (Min vs) = Data.Map.fromList . zip (Data.Set.toList vs) . repeat $ Data.Set.empty
    format vs env steps = concatMap fmtStep steps
      where
        fmtStep step =
          "\\mathcal O_{F_i;\\eval{\\chi^{<i}}}("
            ++ (intercalate ", " . map fmtSet . fst $ step)
            ++ ") "
            ++ (fmtLine . zipWith fmtOFn vs . repeat . fst $ step)
            ++ (fmtLine . map (fmtEval . flip Data.Map.union env . Data.Map.fromList . zip vs . fst $ step) $ vs)
            ++ (fmtLine . map setTeX . snd $ step)
    fmtOFn v args = "\\mathcal O_{" ++ (hmlTeX . decl $ v) ++ "}" ++ (fmtParen . map fmtSet $ args)
    fmtEval env = hmlETeX . substVars (env !) . decl
    fmtParen l = "(" ++ intercalate ", " l ++ ")"
    fmtLine l = " &= " ++ fmtParen l ++ "\\\\\n"
    fmtSet s = if s == proc lts then "Proc" else if null s then "\\emptyset" else setTeX s
