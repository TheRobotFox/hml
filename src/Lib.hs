{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
(Transition(Transition), LTS(LTS, proc, act, trans), ltsFromTrans, HML(Ex,All,And,Or,TT,FF,Var), Fixpoint(Min, Max), Declaration, makeSystem, HMLEquation(groups))
  where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Set (Set, fromList, map, singleton, unions, empty, partition, difference, intersection, filter, union)
import Control.Applicative ( liftA3 )
import Data.Functor.Foldable (Recursive(cata))
import Debug.Trace (traceShow, traceShowId)
import Control.Monad.Writer

data Transition p a = Transition p a p deriving (Show, Eq, Ord)

data LTS p a = LTS {proc :: Set p, act:: Set a, trans :: Set (Transition p a)} deriving (Show)

ltsFromTrans :: (Ord p, Ord a) => Set (Transition p a) -> (LTS p a)
ltsFromTrans = liftA3 LTS (fromList . concatMap getProcs) (Data.Set.map getActs) id
  where getProcs (Transition a _ b) = [a,b]
        getActs (Transition _ a _) = a

data HML a v = Ex [a] (HML a v) | All [a] (HML a v) | And (HML a v) (HML a v) | Or (HML a v) (HML a v) | TT | FF | Var v

makeBaseFunctor ''HML


data Fixpoint a = Max a | Min a deriving (Show, Eq)
type Declaration a v = v -> Fixpoint (HML a v)

data VarLog p v = Unevaluated v | Partial (Set p)
data Log p a v = Log [HML a (VarLog p v)]

data HMLEquation a v = HMLEquation {f:: HML a v, decl :: Declaration a v, groups :: [Fixpoint (Set v)]}


getDependencies ::  Ord v => HML a v -> Set v
getDependencies = cata collectVars
  where collectVars (VarF var) = Data.Set.singleton var
        collectVars (AllF _ f) = f
        collectVars (ExF _ f)  = f
        collectVars (AndF a b) = union a b
        collectVars (OrF a b)  = union a b
        collectVars _          = Data.Set.empty



fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a = let r = f a in if a==r then a else fixpoint f r

fixpointM :: (Monad m, Eq a) => (a -> m a) -> a -> m a
fixpointM f a = do r <- f a
                   if r == a then return r else fixpointM f r

makeGroups :: Ord v => Declaration a v -> Set v -> Maybe [Fixpoint (Set v)]
makeGroups declFn initF = let (gs, vs) = fixpoint extract ([], reachable initF) in
   if null vs then Just $ reverse gs else Nothing
  where
    extract (gs, vs) = let (vMax, vMin) = Data.Set.partition (isMax . declFn) vs
                           (iMax, iMin) = (independent vMax vMin, independent vMin vMax)
                           independent s from = fixpoint (Data.Set.filter (null . intersection from . getDependencies . getHML)) s
                        in case (not $ null iMax, not $ null iMin) of
                         (True, _) ->  (Max iMax:gs, difference vs iMax)
                         (_, True) ->  (Min iMin:gs, difference vs iMin)
                         _ -> (gs, vs)
        
    isMax (Max _) = True
    isMax _ = False
    reachable = fixpoint (union <*> unions . Data.Set.map (getDependencies . getHML))
    getHML var = case declFn var of
      (Max h) -> h
      (Min h) -> h

makeSystem :: Ord v => HML a v -> Declaration a v -> Maybe (HMLEquation a v)
makeSystem formular declFn = fmap (HMLEquation formular declFn) . makeGroups declFn . getDependencies $ formular

-- solveSystem :: System a v -> LTS p a ->

-- cata pretty printer

