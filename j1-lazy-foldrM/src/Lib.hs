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

data Peano where
    Zero :: Peano
    Succ :: Peano -> Peano

take' :: forall a. Peano -> [a] -> [a]
take' = \cases
    Zero _               -> []
    (Succ n') ~(a : sa') -> a : take' n' sa'

reverse' :: forall a. [a] -> [a]
reverse' =
    let reverse'A = \ n ra -> \case
            []      -> (n, ra)
            a : sa' -> case reverse'A n (a : ra) sa' of
                ~(n', ra') -> (Succ n', ra')
    in  \sa ->
            let (n, ra) = reverse'A Zero [] sa
            in  take' n ra

foldrM :: forall m t a b.
    (Foldable t, Monad m) =>
    (a -> b -> m b) -> b -> t a -> m b
foldrM = \ g b ->
    foldlM (flip g) b . reverse' . toList
