> {-# LANGUAGE RankNTypes #-}
> module STC where

> import Control.Monad.Par as Par
> import Control.Monad.ST
> import Control.Monad.State
> import Data.Dynamic


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
> toS :: Typeable a => a -> S
> toS = S . toDyn
>
> fromS :: Typeable a => S -> a
> fromS (S x) = undefined -- fromDyn x

> class Monad m => STCLang m where
>   liftWithIndex :: forall a s b . (Typeable s, NFData s) =>
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
>   (<*>) = ap

> instance Monad SD where
>   f >>= g = SD $ \sn -> do
>                    x <- runSD f sn
>                    runSD (g x) sn

-- > instance STCLang SD where
-- >   liftWithIndex idx f a = SD (comp $ f a)
-- >     where
-- >      comp st (GlobalState initials results) = do
-- >        let ivarState  = initials !! idx
-- >            ivarState' = results  !! idx
-- >        localState <- fmap fromS (Par.get ivarState)
-- >        let (r, localState') = runStateThread st localState
-- >        _ <- Par.put ivarState' $ toS localState'
-- >        return r
