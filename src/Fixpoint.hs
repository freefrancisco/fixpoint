module Fixpoint
    ( fixpoint
    ) where

fixpoint :: (a -> a) -> a
fixpoint f = f (fixpoint f)
