module Fixpoint
    ( fixpoint
    , fixpoint'
    ) where

fixpoint :: (a -> a) -> a
fixpoint f = fp where fp = f fp

fixpoint' :: (a -> a) -> a
fixpoint' f = f (fixpoint' f)
