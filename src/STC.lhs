> {-# LANGUAGE InstanceSigs #-}
> module STC where

> import Control.Monad.Par as Par
> import Control.Monad.ST
> import Control.Monad.Reader
> import Control.Monad.State
> import Data.Dynamic
> import Data.Maybe

State Thread Composition Language

> type StateThread  s b = State s b
> type StateThread' s b = ST s b

(>>=) :: StateThread s a
      -> (a -> StateThread s b)
      -> StateThread s b

Following is the monad instance of ST

instance Monad (ST s) where

    (ST m) >>= k
      = ST (\ s ->
        case (m s) of { (# new_s, r #) ->
        case (k r) of { ST k2 ->
        (k2 new_s) }})


The above approach encapsulates state inside the monad


How then do we compose something like:

f :: (StateThread StateTypeA a)
g :: a -> (StateThread StateTypeB b)


??


Lets take a practical example:

f :: a -> StateThread sf b
g :: b -> StateThread sg c
h :: c -> StateThread sh d

Analogous to `load`, `decrpyt` and `decompress` in a key value store and each of them uses a local cache



Composition is done using the following :

liftWithIndex :: STCLang m =>
  Int -> (a -> StateThread s b) -> a -> m b

composition :: STCLang m => a -> m d
composition input = do
  bval <- liftWithIndex 0 f input
  cval <- liftWithIndex 1 g bval
  dval <- liftWithIndex 2 h cval
  return dval


Is ST the `s` is simply a type variable. It simply does not expose the mutable state hidden inside.

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



For the purpose of this presentation we are working with State

> runStateThread :: StateThread s b -> s -> (b, s)
> runStateThread = runState


Lets look at the monad implementation of State

instance Monad (State s) where
    m >>= k = State $ \ s -> case runState m s of
        (s', x) -> runState (k x) s'

> data GlobalState = GlobalState { initial :: [IVar S]
>                                , results :: [IVar S]}
> data SD result = SD {runSD :: GlobalState -> Par result}


> instance Functor SD where
>   fmap = liftM

> instance Applicative SD where
>   pure  = return
>   f <*> g = SD $ \sn -> do
>                    gResultVar <- Par.spawn_ $ runSD g sn
>                    fp         <- runSD f sn
>                    fp <$> Par.get gResultVar

> instance Monad SD where
>   f >>= g = SD $ \sn -> do
>                    x <- runSD f sn
>                    runSD (g x) sn

> instance STCLang SD where
>   liftWithIndex idx f a = SD (comp $ f a)
>     where
>      comp st (GlobalState initials results) = do
>        let ivarState  = initials !! idx
>            ivarState' = results  !! idx
>        localState <- fromS <$> (Par.get ivarState)
>        let (r, localState') = runStateThread st localState
>        _ <- Par.put_ ivarState' $ toS localState'
>        return r


>   smap :: (a -> SD b) -> [a] -> SD [b]
>   smap h xs = SD $ \(GlobalState initials results) -> do
>      ysIVars <- compute xs initials results
>      forM ysIVars Par.get
>      where
>        compute [] _ _ = return []
>        compute (xi : xs) currentStates lastStates = do
>          nextStates <- getNextStates xs
>          yIVari <- Par.spawn_ $ runSD (h xi)
>                                   (GlobalState currentStates nextStates)
>          ysIVars <- compute xs nextStates lastStates
>          return (yIVari : ysIVars)
>          where
>            getNextStates [] = return lastStates
>            getNextStates _ =
>              replicateM (length currentStates) Par.new

>   smap_ :: (a -> SD b) -> [a] -> SD ()
>   smap_ h xs = SD $ \(GlobalState initials results) -> do
>      ysIVars <- compute xs initials results
>      forM_ ysIVars Par.get
>      where
>        compute [] _ _ = return []
>        compute (xi : xs) currentStates lastStates = do
>          nextStates <- getNextStates xs
>          yIVari <- Par.spawn_ $ runSD (h xi)
>                                   (GlobalState currentStates nextStates)
>          ysIVars <- compute xs nextStates lastStates
>          return (yIVari : ysIVars)
>          where
>            getNextStates [] = return lastStates
>            getNextStates _ =
>              replicateM (length currentStates) Par.new




> liftWithIndex' idx f a = SD (comp $ f a)
>   where comp st (GlobalState initials results) = do
>           let ivarState  = initials !! idx
>               ivarState' = results  !! idx
>           localState <- Par.get ivarState
>           _          <- Par.put_ ivarState' localState
>           runReader st $ fromS localState
