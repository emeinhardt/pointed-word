{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{- | This module contains (currently small) shared functionality across modules.
-}

module PointedWord.Core
  (
    GPwNode ( Context
            , Point
            )

  , splitAround
  , splitAround'
  ) where



import Prelude.Unicode
  ( (∘)
  )

import Data.Bool
  ( bool
  )

import GHC.Generics
  ( Generic
  )
import Control.DeepSeq
  ( NFData
  )

import Data.Sequence.NonEmpty qualified as N
import Data.Sequence.NonEmpty
  ( NESeq
  , toSeq
  , nonEmptySeq
  )
import Data.Sequence qualified as S
import Data.Sequence
  ( Seq
  )



data GPwNode b a = Context b | Point a
  deriving stock    (Eq, Ord, Read, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass  NFData



{- | Given an index @i@ and a @Seq@, split the @Seq@ into the value at @i@ and the rest of the @Seq@ on
either side. An empty sequence of values on either side will be represented as 'Nothing'.

Fails if the 'Seq' is empty or the index is out of bounds. -}
splitAround ∷ ∀ a. Int → Seq a → Maybe (Maybe (Seq a), a, Maybe (Seq a))
splitAround i s
  | i < 0 || i > S.length s - 1 = Nothing
  | otherwise = do
      p ← S.lookup i s
      let (l', r') = S.splitAt i s
      let l = S.deleteAt i l'
      let r = S.deleteAt 0 r'
      return (justWhen (not ∘ S.null) l, p, justWhen (not ∘ S.null) r)


{- | Given an index @i@ and an @NESeq@, split the @NESeq@ into the value at @i@ and the rest of the @NESeq@ on
either side. An empty sequence of values on either side will be represented as 'Nothing'.

Fails if the index is out of bounds. -}
splitAround' ∷ ∀ a. Int → NESeq a → Maybe (Maybe (NESeq a), a, Maybe (NESeq a))
splitAround' i s
  | i < 0 || i > N.length s - 1 = Nothing
  | otherwise = do
      let s' = toSeq s
      p ← S.lookup i s'
      let (l', r') = S.splitAt i s'
      let l = S.deleteAt i l'
      let r = S.deleteAt 0 r'
      return (nonEmptySeq l, p, nonEmptySeq r)


-- | Given a predicate @p@ and a value @a@, return @Just a@ when @p a@.
justWhen ∷ (a → Bool) → a → Maybe a
justWhen = ((bool Nothing ∘ Just) <*>)
