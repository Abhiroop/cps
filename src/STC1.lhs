> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE InstanceSigs #-}
> module STC1 where

> import Control.Monad.Par as Par
> import Control.Monad.ST
> import Control.Monad.Reader
> import Control.Monad.State
> import Data.Dynamic
> import Data.Maybe
> import Prelude hiding (id)


State Thread Composition Language

> type StateThread  s b = State s b
> type StateThread' s b = ST s b


> newtype S = S Dynamic deriving Show
>
> toS :: (Typeable a) => a -> S
> toS = S . toDyn
>
> fromS :: (Typeable a) => S -> a
> fromS (S x) = fromJust $ fromDynamic x -- partial function

> class Monad m => STCLang m where
>   liftWithIndex :: (Typeable s, NFData s) =>
>                    Int -> (a -> StateThread s b) -> a -> m b
>   runSTCLang    :: m b -> [S] -> b
>   smap  :: (a -> m b) -> [a] -> m [b]
>   smap_ :: (a -> m b) -> [a] -> m ()
>   if'   :: Bool -> m b -> m b -> m b



> runStateThread :: StateThread s b -> s -> (b, s)
> runStateThread = runState



> data GlobalState = GlobalState { initial :: [IVar S]
>                                , results :: [IVar S]}
> data SD result = SD { runSD :: GlobalState -> Par result
>                     ,    id :: GlobalState -> Par ()}


> instance Functor SD where
>   fmap = liftM

> instance Applicative SD where
>   pure  = return
>   f <*> g = SD comp1 comp2
>     where
>      comp1 sn = do
>         gResultVar <- Par.spawn_ $ runSD g sn
>         fp         <- runSD f sn
>         fp <$> Par.get gResultVar
>      comp2 sn = do
>         _  <- Par.spawn_ $ id g sn
>         fp <- runSD f sn
>         return ()

> instance Monad SD where
>   f >>= g = SD comp1 comp2
>    where
>     comp1 sn = do
>       x <- runSD f sn
>       runSD (g x) sn
>     comp2 sn = do
>       x <- runSD f sn
>       id (g x) sn


> instance STCLang SD where
>  if' :: Bool -> SD b -> SD b -> SD b
>  if' cond trueBranch falseBranch = SD comp idIf
>    where
>      (toExecute, toTransfer) = if cond
>          then (trueBranch, falseBranch)
>          else (falseBranch, trueBranch)
>      comp sn = do
>        _ <- id toTransfer sn
>        runSD toExecute sn
>      idIf = id trueBranch >> id falseBranch





Type safety for state threads


> liftST :: (Typeable s, NFData s)
>        => s -> (a -> StateThread s b) -> STCLang' a b

> data CollSt = CollSt { stStates :: [S]}
> type STCLang' a b =
>    forall m . STCLang m => State CollSt (a -> m b)

> liftST localState stateThread = do
>   l <- state $ \ s -> (length $ stStates s
>                       , CollSt $ stStates s ++ [toS localState])
>   pure $ liftWithIndex l stateThread

runSTCLang' :: STCLang' a b -> a -> (b, [S])
runSTCLang' langComp a = do
   (comp, gs) <- runState langComp mempty
   runSTCLang (comp a) $ stStates gs



