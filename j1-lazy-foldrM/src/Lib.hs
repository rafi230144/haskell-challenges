{-# LANGUAGE Haskell2010
  , GADTSyntax
  , LambdaCase
  , ScopedTypeVariables
#-}

module Lib
  ( foldrM
  ) where

import Data.Foldable
  ( toList
  , foldlM
  )


-- * Auxiliaries

data Peano where
    Zero :: Peano
    Succ :: Peano -> Peano

{- | 'Prelude.take' but lazier\;
    the second argument not having
    at least the first argument number
    of elements is a totality error
-}
take' :: forall a. Peano -> [a] -> [a]
take' = \cases
    Zero      _          -> []
    (Succ n') ~(a : sa') -> a : take' n' sa'

{- | Identical to 'Data.Bifunctor.first'\;
    lazy in the second argument's 'Prelude.(,)'
    constructor
-}
fstmap :: forall a a' b. (a -> a') -> (a, b) -> (a', b)
fstmap = \ f ~(a, b) ->
    (f a, b)

{- | 'Prelude.reverse' but lazier\;
    works even on infinite 'Data.List.List's
    so long as no element is demanded
-}
reverse' :: forall a. [a] -> [a]
reverse' =
    let reverse'A :: Peano -> [a] -> [a] -> (Peano, [a])
        reverse'A = \ n ra -> \case
            []      -> (n, ra)
            a : sa' -> fstmap Succ $ reverse'A n (a : ra) sa'
    in  \sa -> let (n, ra) = reverse'A Zero [] sa in
            take' n ra


-- * 'foldrM'

foldrM :: forall m t a b.
    (Foldable t, Monad m) =>
    (a -> b -> m b) -> b -> t a -> m b
foldrM = \ g b ->
    foldlM (flip g) b . reverse' . toList
