{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{- | This package models the domain of __pointed words__.

= What's a pointed word?

A __singly-pointed word__ over /a/ is a sequence of:

 - A possibly empty initial context.
 - A distinguished point of type /a/.
 - A possibly empty final context.

For example, /abba\>x\<aaa/ is a pointed word with /abba/ as preceding context, /aaa/ as following context, and
/x/ as the point or point(ed) symbol. (The metalinguistic symbols />/ and /</ surround a point.)


A __multipointed word__ over /a/ consists of a sequence of points, with a (possibly empty) context on either
side of the points:

 - /abba\>x\<aaa\>y\</ is a pointed word with /abba/ and /aaa/ as contexts, and /x/ and /y/ as points.
 - /\>x\<a\>y\</ is a pointed word with /a/ as a context, and /x/ and /y/ as points.
 - /\>x\<\>y\</ is a pointed word with no contexts, and /x/ and /y/ as points.


An __unpointed word__ is a pointed word that has no points, and consists only of a single (possibly empty)
context.

We can transform an unpointed word /w/, /|w| = n/ into a singly-pointed word by specifying an index
/0 ≤ i ≤ n -1/ to become a point:

 - /point 4 abbaxaaa = abba\>x\<aaa/

We can similarly transform a singly-pointed word into a multipointed one by specifying an index to transform
into a point if it isn't already:

 - /point 8 abba\>x\<aaay = abba\>x\<aaa\>y\</
 - /point 4 abba\>x\<aaay = abba\>x\<aaay/

Dually, we can project off a point of a pointed word:

 - /unpoint 8 abba\>x\<aaa\>y\< = abba\>x\<aaay/
 - /unpoint 4 abba\>x\<aaay = abbaxaaay/



== Operations

We can concatenate pointed words:

 - /abba\>x\< ⋅ aaa\>y\< = abba\>x\<aaa\>y\</
 - /abba ⋅ x = abbax/
 - /abba\>x\<a ⋅ aa\>y\< = abba\>x\<aaa\>y\</


Given two pointed words whose unpointed words are the same, we can also take their
/meet/ and /join/:

 - /abba\>x\<aaay ∨ abbaxaaa\>y\< = abba\>x\<aaa\>y\</
 - /abba\>x\<aaa\>y\< ∧ abba\>x\<aaay = abba\>x\<aaay/

For a word /w/, /|w| = n/, there are /2ⁿ/ pointed words, with a bottom element consisting of the unpointed word,
and a top element consisting of the word where every symbol is pointed: the set of all pointed versions of a
particular unpointed word /w/ form a finite Boolean (powerset) lattice.


Given two unpointed words /l, r/ of the same length, we can compute an aligned pair with points at every
location where /l/ and /r/ differ:

 - /axbayazab Δ atbauavab = (a\>x\<ba\>y\<a\>z\<ab, a\>t\<ba\>u\<a\>v\<ab)/

Equivalent, but aligned for easier spotting of differences:

>     a x ba y a z ab
>   Δ a t ba u a v ab
> ≡ ( a>x<ba>y<a>z<ab
>   , a>t<ba>u<a>v<ab
>   )



== Why might I want to think about pointed words?

 - Modeling diffs of same-length sequences — or patches between them.
 - Reasoning about cokleisli arrows in list-zipper-like comonads.
 - Modeling lookaround machines over sequences.

These are roughly the reasons I wrote the package — reasoning about a particular finite-state model of string
transformations called /bimachines/.



=== Pointed words and automata theory

**Note:** This subsection assumes you are already familiar with finite-state automata and basic finite-state
transducers ("[one-way] automata with output").

A /bimachine/ is defined by two contradirectional deterministic (semi)automata and an output function /ω/:

 - /(Aₗ, Aᵣ, ω : Qₗ → Qᵣ → Σ → Γ*)/
 - /Aₗ = (Σ, Qₗ, δₗ : Qₗ → Σ → Qₗ)/
 - /Aᵣ = (Σ, Qᵣ, δᵣ : Qᵣ → Σ → Qᵣ)/

One automaton reads an input word from left-to-right, the other reads from right-to-left, and /ω/ maps each
input symbol to an output string. (Note that this only defines a function on non-empty input strings; it is
common to stipulate that an empty string in the input alphabet /Σ/ is mapped to the empty string in the output
alphabet /Γ/.) Modulo the minor wrinkle of deciding how to handle empty input strings, bimachines can represent
any functional rational string transduction — any rational transduction that maps an input string to a unique
output string; they are equivalent in expressivity to one-way (incrementally) non-deterministic finite-state
transducers.

As you can see from the type signature of /ω : Qₗ → Qᵣ → Σ → Γ*/, a bimachine represents a string function
/φ/ characterized by a "local" transformation that has (limited) bidirectional access to the surrounding
context, and this associates every string in /φ/'s domain of definition with a set of singly-pointed words

 - /a₁ ⋅ a₂ ⋅ a₃ ⋅ ... \>aᵢ\< ⋅ ... ⋅ aₙ/

where /aᵢ/ is a "focused" symbol (in the sense of a zipper comonad) and the symbols to each side represent
the context that each automaton summarizes or classifies into one of a finite number of categories
(i.e. states).

If this were the end of the story and the main or only structure of interest, a list zipper comonad or some
variation on them would be appropriate.

However, if we want to explicitly represent the automaton-summarized view of these pointed words that /ω/ acts
on, we instead have a variation on the basic presentation of pointed words used so far: we have a set of
singly-pointed words where the focus and contexts are over different alphabets:

\[
(l_{i-1}, r_{i+1}) ⋅ a_i ⋅ (l_{i+1}, r_{i-1})
\]


where \(l_j : Qₗ\) is a left-to-right state, \(r_j : Qᵣ\) is a right-to-left state, and
for \(q ∈ \{l, r\}\), \( q_{i-1} \) and \( q_{i+1} \) are the states occurring __before__ and __after__
(respectively) the /i/th symbol has been read.

In both cases, note that we can also associate each input string with a (maximally pointed) multipointed string
of essentially the same character. One of the main motivations for this package is a less dense multipointed
string for the special case of bimachines whose output function is of type /ω : Qₗ → Qᵣ → Σ → Γ/ for /Σ ⊆ Γ/.

For these kinds of bimachines, any given input string can be associated with a multipointed string where the
pointed symbols are just those input symbols that /ω/ changes. The set of such multipointed strings are a
mildly parsed version of the associated string function's domain of definition that can be used to analyze
its behavior, help infer its definition from observations, or reason about the behavior of compositions of
such machines. (Also again, this is not something a list zipper comonad models.)



== This module

There are a two variations of pointed words in this package to support different use cases with respect to
basic pointed words, parameterized by their tolerance for where and when the empty string can occur:

 - "PointedWord" can represent the empty string and epsilon-free non-empty strings.
 - "PointedWord.Eps" can represent the empty string and non-empty strings where epsilon is permitted
   internally.

The first variation — exposed here — is the simpler one:

> newtype GPW b a = GPW { unGPW ∷ Seq (GPwNode b a) }
> data GPwNode b a = Context b | Point a
> type PW a = GPW (NESeq a) a
> type PwNode a = GPwNode (NESeq a) a

'PW' corresponds to the basic presentation of a pointed word described above; a "generalized" pointed word
'GPW' is allowed to have a distinct context type from that of the point.

The invariant a 'GPW' value is expected to have is that there will never be a consecutive sequence of two
'Context' nodes.

Note that

 - A pointed word (@PW a@) that is the empty string has one representation: a 'GPW' with an empty 'Seq':
   @lift "" = GPW Seq.empty@.
 - There is no ability to represent empty strings anywhere in the interior of a pointed word: the only
   representable pointed word in this module containing the empty string is the pointed word
   consisting of the empty string itself and no other symbols: @lift "" = GPW Seq.empty@.

This second quality makes it simpler and less expressive than the other implementation in "PointedWord.Eps",
where pointed words are essentially an 'NESeq' of 'PwNode's, and @type PW a = GPW (__Seq__ a) a@: hence,
in that module,

 - A pointed word ('PW') that is the empty string has one representation: a 'GPW' with a 'Context' that is an
   empty 'Seq': @lift "" = GPW $ NESeq.singleton $ Context $ Seq.empty@.
 - A 'Context' with an empty 'Seq' can appear anywhere within a word, representing an empty string somewhere
 in the word's interior.



== Examples of usage

Lifting, pointing, and unpointing:

>>> import Text.Pretty.Simple (pPrint)
>>> import Data.Sequence qualified as S
>>> import Data.Sequence (Seq)
>>> import PointedWord
>>> :set -XOverloadedLists
>>> pPrint $ lift "abbaxaaay"
GPW
    { unGPW = fromList
        [ Context
            ( fromList
                ( 'a' :| "bbaxaaay" )
            )
        ]
    }
it :: ()
>>> w = (lift "abba") <> (fromJust $ lift' 0 "x") <> (lift "aaa") <> (fromJust $ lift' 0 "y") <> (lift "tuvw")
w :: PW Char
>>> pPrint w
GPW
    { unGPW = fromList
        [ Context
            ( fromList
                ( 'a' :| "bba" )
            )
        , Point 'x'
        , Context
            ( fromList
                ( 'a' :| "aa" )
            )
        , Point 'y'
        , Context
            ( fromList
                ( 't' :| "uvw" )
            )
        ]
    }
it :: ()
>>> w == (point' 4 . point' 8 $ lift "abbaxaaaytuvw")
True
it :: Bool
>>> w == ((foldr (\i f → point' i . f) id ([4, 8] :: [Int])) $ lift "abbaxaaaytuvw")
True
it :: Bool
>>> pPrint $ unpoint' 4 w
GPW
    { unGPW = fromList
        [ Context
            ( fromList
                ( 'a' :| "bba" )
            )
        , Point 'x'
        , Context
            ( fromList
                ( 'a' :| "" )
            )
        , Point 'y'
        , Context
            ( fromList
                ( 't' :| "uvw" )
            )
        ]
    }
it :: ()

Working with points of a word or with the indices of the underlying lowered (≈ unpointed) word:

>>> points w
fromList "xy"
it :: Seq Char
>>> pPrint $ lowerIdxed $ point' 4 $ lift "abbax"
fromList
    [ Context
        ( fromList
            (
                ( 0
                , 'a'
                ) :|
                [
                    ( 1
                    , 'b'
                    )
                ,
                    ( 2
                    , 'b'
                    )
                ,
                    ( 3
                    , 'a'
                    )
                ]
            )
        )
    , Point
        ( 4
        , 'x'
        )
    ]
it :: ()
>>> lowerLookup 4 $ point' 4 $ lift "abbax"
Just 'x'
it :: Maybe Char
>>> (lower . lift $ "abbax") == "abbax"
True
it :: Bool

Operations on pointed words:

>>> pPrint $ (fromJust $ lift' 4 "abbax") <> (fromJust $ lift' 3 "aaay")
GPW
    { unGPW = fromList
        [ Context
            ( fromList
                ( 'a' :| "bba" )
            )
        , Point 'x'
        , Context
            ( fromList
                ( 'a' :| "aa" )
            )
        , Point 'y'
        ]
    }
it :: ()
>>> pPrint $ (fromJust $ lift' 2 "abxby") `join` (fromJust $ lift' 4 "abxby")
Just
    ( GPW
        { unGPW = fromList
            [ Context
                ( fromList
                    ( 'a' :| "b" )
                )
            , Point 'x'
            , Context
                ( fromList
                    ( 'b' :| "" )
                )
            , Point 'y'
            ]
        }
    )
it :: ()
>>> pPrint $ (fromJust $ lift' 2 "abxby") `meet` (fromJust $ lift' 4 "abxby")
Just
    ( GPW
        { unGPW = fromList
            [ Context
                ( fromList
                    ( 'a' :| "bxby" )
                )
            ]
        }
    )
it :: ()
>>> pPrint $ "axbayazab" `delta` "atbauavab"
Just
    ( GPW
        { unGPW = fromList
            [ Context
                ( fromList
                    ( 'a' :| "" )
                )
            , Point 'x'
            , Context
                ( fromList
                    ( 'b' :| "a" )
                )
            , Point 'y'
            , Context
                ( fromList
                    ( 'a' :| "" )
                )
            , Point 'z'
            , Context
                ( fromList
                    ( 'a' :| "b" )
                )
            ]
        }
    , GPW
        { unGPW = fromList
            [ Context
                ( fromList
                    ( 'a' :| "" )
                )
            , Point 't'
            , Context
                ( fromList
                    ( 'b' :| "a" )
                )
            , Point 'u'
            , Context
                ( fromList
                    ( 'a' :| "" )
                )
            , Point 'v'
            , Context
                ( fromList
                    ( 'a' :| "b" )
                )
            ]
        }
    )
it :: ()
-}

-- TODO A lot of operations depend on indexing into the underlying lowered word; whenever performance
-- becomes important and you have benchmarks, figure out regenerating this index is actually costly.

module PointedWord
  ( -- * Construction
    GPW ( GPW
        , unGPW
        )
  , GPwNode ( Context
            , Point
            )
  , PW
  , PwNode
  , lift
  , lift'
  , lower

    -- * Queries and related helpers
  , lowerLength
  , lowerLookup
  , lowerIdxed
  , subIdxed
  , points
  , pointIdxs
  , pointIdxs'
  , contexts
  -- , contextIdxs
  , contextIdxs'
  , invalidr
  , invalidl

    -- * Operations and Newtypes
  , mapWithIdx
  , point
  , point'
  , unpoint
  , unpoint'
  , unpointX
  , unpointX'
  , join
  , meet
  , relComp
  , delta
  , Lowered ( Lowered
            , unLowered
            )

    -- * Working with nodes
  , isContext
  , isPoint
  , toNodeIndex
  , fromNodeIndexL
  , fromNodeIndexR
  , fromNodeIndex
  , rightmostN
  , leftmostN
  , lengthN
  , subLookup
  , lowerIdxedN
  , subIdxedN
  , mapWithIdxN
  , maintain
  , maintain'
  , splitNode
  , splitAround
  , splitAround'

  ) where

import Prelude.Unicode
  ( (∘)
  , (≠)
  )
import Data.Foldable.Unicode
  ( (∈)
  , (∉)
  )

import Data.Composition
 ( (.:)
 , (.:.)
 )
import Data.Function
  ( on
  )

import Control.Arrow
  ( (&&&)
  , (***)
  )
import Data.Bifunctor
  ( first
  )

import Data.Maybe
  ( fromJust
  , fromMaybe
  )
import Control.Applicative
  ( (<|>)
  )
import Control.Monad qualified as M
import Control.Monad
  ( liftM2
  )

import Control.DeepSeq
  ( NFData
  )
import GHC.Generics
  ( Generic
  )
import Control.Newtype.Generics
  ( Newtype, over, ala
  )

import Data.Monoid
  ( Sum ( Sum
        )
  )

import Data.Foldable qualified as F
import Data.Sequence qualified as S
import Data.Sequence
  ( Seq ((:<|), (:|>))
  , (><)
  , (!?)
  )
import Data.Sequence.NonEmpty qualified as N
import Data.Sequence.NonEmpty
  ( NESeq ((:<||), (:||>))
  , unsafeFromSeq
  , toSeq
  )
import Data.Set qualified as Set
import Data.Set
  ( Set
  )

import PointedWord.Core
  ( GPwNode ( Context
            , Point
            )
  , splitAround
  , splitAround'
  )


---------------
-- CONSTRUCTION
---------------

-- | A generalized pointed word.
newtype GPW b a = GPW { unGPW ∷ Seq (GPwNode b a) }
  deriving stock    (Eq, Ord, Read, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Newtype, NFData)

type PW     a = GPW     (NESeq a) a
type PwNode a = GPwNode (NESeq a) a

-- | Lift a 'Seq' into an unpointed word — a context.
lift ∷ ∀ a. Seq a → PW a
lift S.Empty = GPW S.Empty
lift s       = GPW ∘ S.singleton ∘ Context ∘ unsafeFromSeq $ s

{- | @lift' i w@ lifts a 'Seq' into a pointed word by making the /i/th element a 'Point' and splitting
the rest of the word into a left 'Context' and a right 'Context'.

Fails iff @i@ is not an index of @w@.

Note that @lift' i w@ is essentially sugar for @point i . lift $ w@. -}
lift' ∷ ∀ a. Int → Seq a → Maybe (PW a)
lift' = fmap fuse .: splitAround where
  fuse ∷ (Maybe (Seq a), a, Maybe (Seq a)) → PW a
  fuse (l,p,r) =
    GPW
    $  maybe S.singleton (\l' → (S.singleton  (Context ∘ unsafeFromSeq $ l') :|>)) l (Point p)
    >< maybe S.empty            (S.singleton ∘ Context ∘ unsafeFromSeq)            r


-- | \"Flatten\" a pointed word into an undifferentiated sequence of values.
lower ∷ ∀ a. PW a → Seq a
lower = F.foldr δ S.empty ∘ unGPW where
  δ ∷ PwNode a → Seq a → Seq a
  δ (Context as) as' = toSeq as >< as'
  δ (Point    a) as' = a :<| as'



----------
-- QUERIES
----------

isContext ∷ GPwNode b a → Bool
isContext (Context _) = True
isContext _           = False

isPoint ∷ GPwNode b a → Bool
isPoint (Point _) = True
isPoint _         = False

-- | Project a 'GPW' to just the sequence of 'Point' values it contains.
points ∷ ∀ b a. GPW b a → Seq a
points = F.foldr δ S.empty ∘ unGPW where
  δ ∷ GPwNode b a → Seq a → Seq a
  δ (Context _) as = as
  δ (Point   a) as = a :<| as

-- | Return an ascending 'Seq' of indices in the underlying lowered word of all 'Point's.
pointIdxs ∷ ∀ a. PW a → Seq Int
pointIdxs = (fmap ∘ toLowerIndexed) <*> pointIdxs' where
  toLowerIndexed ∷ PW a → Int → Int
  toLowerIndexed w i = i + ala Sum foldMap (fmap (subtract 1 ∘ lengthN) ∘ S.take i $ unGPW w)

-- | Return an ascending 'Seq' of indices of all 'Point's.
pointIdxs' ∷ ∀ b a. GPW b a → Seq Int
pointIdxs' = S.fromList ∘ S.findIndicesL isPoint ∘ unGPW

-- | Project a 'GPW' to just the sequence of 'Context' values it contains.
contexts ∷ ∀ b a. GPW b a → Seq b
contexts = F.foldr δ S.empty ∘ unGPW where
  δ ∷ GPwNode b a → Seq b → Seq b
  δ (Point   _) bs = bs
  δ (Context b) bs = b :<| bs

-- | Return an ascending 'Seq' of indices of all 'Context's.
contextIdxs' ∷ ∀ b a. GPW b a → Seq Int
contextIdxs' = S.fromList ∘ S.findIndicesL isContext ∘ unGPW

-- | The rightmost value of a 'PwNode'.
rightmostN ∷ ∀ a. PwNode a → a
rightmostN (Point            a ) = a
rightmostN (Context (_  :||> a)) = a

-- | The leftmost value of a 'PwNode'.
leftmostN ∷ ∀ a. PwNode a → a
leftmostN (Point            a ) = a
leftmostN (Context (a :<||  _)) = a

-- | The length of the contents of a 'PwNode'.
lengthN ∷ ∀ a. PwNode a → Int
lengthN (Point    _) = 1
lengthN (Context as) = N.length as

-- | The length of the underlying lowered word.
lowerLength ∷ ∀ a. PW a → Int
lowerLength = ala Sum foldMap ∘ fmap lengthN ∘ unGPW

-- | Zip each value in a 'PwNode' with its index within the node.
subIdxedN ∷ ∀ a. PwNode a → PwNode (Int, a)
subIdxedN (Point    a) = Point (0, a)
subIdxedN (Context as) = let l = N.length as in Context $ N.zip (N.fromFunction l id) as

-- | Map 'subIdxedN' over the nodes of a 'PW'.
subIdxed ∷ ∀ a. PW a → PW (Int, a)
subIdxed = over GPW (fmap subIdxedN)

{- | Given the length of the underlying lowered word so far, plus a `subIdxedN` node, adjust each
zipped index in the node so it now represents the index of the node element with respect to the
underlying lowered word. -}
lowerIdxedN ∷ ∀ a. Int → PwNode (Int, a) → PwNode (Int, a)
lowerIdxedN i (Point (n, a)) = Point (n+i, a)
lowerIdxedN i (Context   as) = Context $ first (+i) <$> as

-- | Zip (left-to-right) each value in a 'PW' with its index in the underlying lowered word.
lowerIdxed ∷ ∀ a. PW a → Seq (PwNode (Int, a))
lowerIdxed = F.foldl' δ S.empty ∘ unGPW ∘ subIdxed where
  δ ∷ Seq (PwNode (Int, a)) → PwNode (Int, a) → Seq (PwNode (Int, a))
  δ S.Empty     p = S.singleton p
  δ s@(_ :|> n) p = s :|> lowerIdxedN ((+1) ∘ fst ∘ rightmostN $ n) p

-- | Lookup a value in a 'PwNode' by its index within that node.
subLookup ∷ ∀ a. PwNode a → Int → Maybe a
subLookup (Point a) i
  | i == 0    = Just a
  | otherwise = Nothing
subLookup (Context as) i = N.lookup i as

-- | Lookup the value in a 'PW' given an index into the underlying lowered word.
lowerLookup ∷ ∀ a. Int → PW a → Maybe a
lowerLookup i p@(GPW w) = do
  (j, k) ← toNodeIndex p i
  n      ← w !? j
  a      ← subLookup n k
  return a




{- | Given a pointed word and an index into the underlying lowered word, return the index of the 'Context'
or 'Point' node containing the value with the given underlying lowered word index, plus an offset within the
node.

Fails if the index is out of bounds with respect to the underlying lowered word. -}
toNodeIndex ∷ ∀ a. PW a → Int → Maybe (Int, Int)
toNodeIndex p i
  | i < 0 || i > lowerLength p - 1 = Nothing
  | otherwise = let
        containsI ∷ Int → PwNode (Int, a) → Bool
        containsI j (Point (k, _)) = j == k
        containsI j (Context   as) = F.any ((j ==) ∘ fst) as

        indexWithI ∷ Int → Seq (PwNode (Int, a)) → Int
        indexWithI = fromJust .: S.findIndexL ∘ containsI

        -- given i + a lowerindexed node containing i, calculate the offset
        offset ∷ Int → PwNode (Int, a) → Int
        offset _ (Point    _) = 0
        offset j (Context as) = fromJust $ N.findIndexL ((j ==) ∘ fst) as

        p' = lowerIdxed p
        n  = indexWithI i p'
      in Just (n, offset i ∘ fromJust ∘ S.lookup n $ p')

{- | Given a pointed word and an index into the nodes of the pointed word, return the lowered word index
corresponding to the leftmost value in that node.

Fails if the provided node index is out of bounds. -}
fromNodeIndexL ∷ ∀ a. PW a → Int → Maybe Int
fromNodeIndexL p i = fst ∘ leftmostN <$> S.lookup i (lowerIdxed p)

{- | Given a pointed word and an index into the nodes of the pointed word, return the lowered word index
corresponding to the rightmost value in that node.

Fails if the provided node index is out of bounds. -}
fromNodeIndexR ∷ ∀ a. PW a → Int → Maybe Int
fromNodeIndexR p i = fst ∘ rightmostN <$> S.lookup i (lowerIdxed p)

{- | Given a pointed word and an index into the nodes of the pointed word, return the pair of
lowered word indices corresponding to the leftmost and rightmost values in that node.

Fails if the provided node index is out of bounds. -}
fromNodeIndex ∷ ∀ a. PW a → Int → Maybe (Int, Int)
fromNodeIndex p i = (fst ∘ leftmostN &&& fst ∘ rightmostN) <$> S.lookup i (lowerIdxed p)


-------------
-- INVARIANTS
-------------

{- | Processes the 'PointedWord' from the right and returns either a 'Left' with the longest
valid suffix (tail) of the input that was valid, or returns a 'Right' containing the original
'PointedWord'.

A valid 'PointedWord' has no consecutive pairs of 'Context's. -}
invalidr ∷ ∀ b a. GPW b a → Either (Seq (GPwNode b a)) (GPW b a)
invalidr = out ∘ F.foldr δ (False, S.empty) ∘ unGPW where
  out (True, validAcc) = Left          validAcc
  out (_   , pw      ) = Right ∘ GPW $ pw

  δ ∷ GPwNode b a → (Bool, Seq (GPwNode b a)) → (Bool, Seq (GPwNode b a))
  δ _ z@(True, _)                            = z
  δ (Context _) (False, (Context b') :<| xs) = (True, Context b' :<| xs)
  δ n           (t    ,                  xs) = (t   , n          :<| xs)

{- | Processes the 'PointedWord' from the left and returns either a 'Left' with the longest
valid prefix (init) of the input that was valid, or returns a 'Right' containing the original
'PointedWord'.

A valid 'PointedWord' has no consecutive pairs of 'Context's. -}
invalidl ∷ ∀ b a. GPW b a → Either (Seq (GPwNode b a)) (GPW b a)
invalidl = out ∘ F.foldl' δ (False, S.empty) ∘ unGPW where
  out (True, validAcc) = Left          validAcc
  out (_   , pw      ) = Right ∘ GPW $ pw

  δ ∷ (Bool, Seq (GPwNode b a)) → GPwNode b a → (Bool, Seq (GPwNode b a))
  δ z@(True, _)                  _           = z
  δ (False, xs :|> (Context b')) (Context _) = (True, xs :|> Context b')
  δ (t    ,                  xs) n           = (t   , xs :|> n         )

-- | Find all runs of consecutive contexts and '<>' them.
maintain ∷ ∀ b a. (Semigroup b) ⇒ GPW b a → GPW b a
maintain = GPW ∘ F.foldr δ S.empty ∘ unGPW where
  δ ∷ GPwNode b a → Seq (GPwNode b a) → Seq (GPwNode b a)
  δ (Context b) ((Context b') :<| xs) = Context (b <> b') :<| xs
  δ x           xs                    = x :<| xs

-- | Find all runs of consecutive contexts and '<>' them; remove 'mempty' contexts.
maintain' ∷ ∀ b a. (Eq b, Monoid b) ⇒ GPW b a → GPW b a
maintain' = GPW ∘ F.foldr δ S.empty ∘ unGPW where
  lapp ∷ GPwNode b a → Seq (GPwNode b a) → Seq (GPwNode b a)
  lapp n@(Context b) w
    | b == mempty      = w
    | otherwise        = n :<| w
  lapp n             w = n :<| w

  δ ∷ GPwNode b a → Seq (GPwNode b a) → Seq (GPwNode b a)
  δ (Context b) ((Context b') :<| xs) = Context (b <> b') `lapp` xs
  δ x           xs                    = x `lapp` xs



-------------
-- OPERATIONS
-------------

{- | Concatenate the underlying 'Sequences' while consolidating the right-edge context (if any) of the
left argument with the left-edge context (if any) of the right argument. -}
instance (Semigroup b) ⇒ Semigroup (GPW b a) where
  (GPW (ls :|> (Context l))) <> (GPW ((Context r) :<| rs)) = GPW ((ls :|> Context (l <> r)) >< rs)
  (GPW  ls                 ) <> (GPW                  rs ) = GPW ( ls                       >< rs)

instance (Semigroup b) ⇒ Monoid (GPW b a) where
  mempty = GPW S.Empty

{- | Split a 'PwNode' that is a 'Context' around its /i/th element.

Undefined on 'Point' nodes, and undefined when /i/ is not a valid index into the given 'Context' node. -}
splitNode ∷ ∀ a. Int → PwNode a → Maybe (Maybe (PwNode a), PwNode a, Maybe (PwNode a))
splitNode _ (Point    _) = Nothing
splitNode i (Context as) = do
    (l, p, r) ← splitAround' i as
    return (fmap Context l, Point p, fmap Context r)

{- | Map over the elements of a 'PwNode' with access to the index of each element within the node. -}
mapWithIdxN ∷ ∀ a b. (Int → a → b) → PwNode a → PwNode b
mapWithIdxN f (Point    a) = Point   $ f 0 a
mapWithIdxN f (Context as) = Context $ N.mapWithIndex f as

{- | Map over the elements of a 'PW' with access to the index of the element in the underlying lower
word while preserving the 'PW' structure. -}
mapWithIdx ∷ ∀ a b. (Int → a → b) → PW a → PW b
mapWithIdx f = GPW ∘ fmap f' ∘ lowerIdxed where
  f' ∷ PwNode (Int, a) → PwNode b
  f' (Point  (i, a)) = Point   $ f i a
  f' (Context    as) = Context $ fmap (uncurry f) as


{- | @point i@ lifts the value at index @i@ of the underlying lowered word into a 'Point'; it has no effect if
that value is already pointed.

Fails if the index is out of bounds or if there is no point at that index. -}
point ∷ ∀ a. Int → PW a → Maybe (PW a)
point i p@(GPW w)
  | i < 0 || i > lowerLength p - 1 = Nothing
  | i ∈ pointIdxs p                = Just p
  | otherwise = do
      (j, k)     ← toNodeIndex p i
      n          ← S.lookup    j w
      (l, p', r) ← splitNode   k n
      let offsetp = maybe (+0) (const (+1)) l
      let offsetr = maybe (+1) (const (+2)) l
      let insertl = maybe id              (        S.update            j)      l
      let insertp = maybe (S.update j p') (const $ S.insertAt (offsetp j)  p') l
      let insertr = maybe id              (        S.insertAt (offsetr j))     r
      return $ GPW ∘ insertr ∘ insertp ∘ insertl $ w


{- | @point i@ lifts the value at index @i@ of the underlying lowered word into a 'Point'; it has no effect if
there is no point at that index or if that value is already pointed. -}
point' ∷ ∀ a. Int → PW a → PW a
point' = (fromMaybe <*>) ∘ point

{- | @unpoint i@ projects off the point at index @i@ of the underlying lowered word; it has no effect if
there is no point at that index.

Fails if the index is out of bounds. -}
unpoint ∷ ∀ a. Int → PW a → Maybe (PW a)
unpoint i p@(GPW w)
  | i < 0 || i > lowerLength p - 1 = Nothing
  | i ∉ pointIdxs p                = Just p
  | otherwise = let
      -- The fromJust call here can fail iff 'pointIdxs' is incorrect.
      (i', _) = fromJust $ toNodeIndex p i
      -- Can fail iff pointIdxs in the guard statement above is incorrect.
      p' = N.singleton ∘ fromJust $ lowerLookup i p
      l = w !? (i'-1)
      r = w !? (i'+1)

      -- First we try merging p', l, and r; this only succeeds if
      -- both l and r exist and are Contexts.
      mergeLR1 ∷ NESeq a → PwNode a → PwNode a → Maybe (PwNode a)
      mergeLR1 p_ (Context ls) (Context rs) = Just $ Context $ ls <> p_ <> rs
      mergeLR1 _  _            _            = Nothing
      -- Follow-up if mergeLR succeeds is deleting i+1, deleting i, updating i-1 with the result of
      -- mergeLR1
      mergeLR2 ∷ Int → PwNode a → (Seq (PwNode a) → Seq (PwNode a))
      mergeLR2 i_ ctxt = S.update (i_-1) ctxt ∘ S.deleteAt i_ ∘ S.deleteAt (i_+1)
      mergeLR ∷ Int → NESeq a → Maybe (PwNode a) → Maybe (PwNode a) → Maybe (Seq (PwNode a) → Seq (PwNode a))
      mergeLR i_ = fmap (mergeLR2 i_) .:. (M.join .:. liftM2 ∘ mergeLR1)

      -- If mergeLR fails, we try merging p' into l.
      -- This fails if l doesn't exist or if l is a Point.
      mergeL1 ∷ NESeq a → PwNode a → Maybe (PwNode a)
      mergeL1 _  (Point _)    = Nothing
      mergeL1 p_ (Context ls) = Just $ Context $ ls <> p_
      -- Follow-up if mergeL1 succeeds is deleting i, updating i-1 with the result of mergeL1
      mergeL2 ∷ Int → PwNode a → (Seq (PwNode a) → Seq (PwNode a))
      mergeL2 i_ ctxt = S.update (i_-1) ctxt ∘ S.deleteAt i_
      mergeL ∷ Int → NESeq a → Maybe (PwNode a) → Maybe (Seq (PwNode a) → Seq (PwNode a))
      mergeL i_ p_ = fmap (mergeL2 i_) ∘ (mergeL1 p_ =<<)

      -- If we failed to merge into l, we try merging p' into r;
      -- this has the same failure conditions as mergeL1.
      mergeR1 _  (Point _)    = Nothing
      mergeR1 p_ (Context rs) = Just $ Context $ p_ <> rs
      -- Follow-up if mergeR1 succeeds is deleting i+1, updating i with the result of mergeR1
      mergeR2 i_ ctxt = S.update i_ ctxt ∘ S.deleteAt (i_+1)
      mergeR i_ p_ = fmap (mergeR2 i_) ∘ (mergeR1 p_ =<<)

      -- If all other merger attempts have failed, we Just lift
      -- p' into a context, and update i. This will necessarily succeed.
      fallback i_ = fmap (S.update i_) ∘ Just ∘ Context

    in Just
     $ GPW
     ∘ fromJust (mergeLR i' p' l r <|> mergeL i' p' l <|> mergeR i' p' r <|> fallback i' p')
     $ w

{- | @unpoint' i@ projects off the point at index @i@ of the underlying lowered word; it has no effect if
that index does not exist or there is no point at that index. -}
unpoint' ∷ ∀ a. Int → PW a → PW a
unpoint' = (fromMaybe <*>) ∘ unpoint

{- | @unpointX i w@ unpoints all pointed elements in a pointed word __except__ for the element at index @i@
of the underlying lowered word; fails iff @i@ is not a valid index into the underlying lowered word. -}
unpointX ∷ ∀ a. Int → PW a → Maybe (PW a)
unpointX i p
  | i < 0 || i > lowerLength p - 1 = Nothing
  | i ∉ pointIdxs p                = Just p
  | otherwise = Just $ (F.foldr (\i' → (unpoint' i' ∘)) id =<< (S.filter (i ==) ∘ pointIdxs)) p

{- | @unpointX i w@ unpoints all pointed elements in a pointed word __except__ for the element at index @i@
of the underlying lowered word; returns the word unchanged if that index does not exist or there is no
point at that index. -}
unpointX' ∷ ∀ a. Int → PW a → PW a
unpointX' = (fromMaybe <*>) ∘ unpointX


{- | Newtype for redefining equality of a 'PW': two 'Lowered' values are equal iff
they are equal after being 'lower'ed. -}
newtype Lowered a = Lowered { unLowered ∷ PW a }
  deriving stock   (Read, Show, Generic)
  deriving anyclass Newtype

instance (Eq a) ⇒ Eq (Lowered a) where
  (==) = (==) `on` (lower ∘ unLowered)



{- | Given two pointed words that are equal when 'lower'ed, their 'join' is a pointed word
that has all of the points that both words have.

If both pointed words are equal, returns the left argument. -}
join ∷ ∀ a. (Eq a) ⇒ PW a → PW a → Maybe (PW a)
join l r
  |         l ==         r = Just l
  | Lowered l ≠  Lowered r = Nothing
  | otherwise = let
      pL = Set.fromList ∘ F.toList ∘ pointIdxs $ l
      pR = Set.fromList ∘ F.toList ∘ pointIdxs $ r
      pU = pL `Set.union` pR
      δL = pU `Set.difference` pL  -- Changes that need to be made (points) to L to be equal to the join
      δR = pU `Set.difference` pR  -- Changes that need to be made (points) to R to be equal to the join
      merged ∷ Set Int → Set Int → PW a → PW a → PW a
      merged dL dR l' r'
        | Set.size dL >= Set.size dR = F.foldr (\i → (point' i ∘)) id δL l'
        | otherwise                  = F.foldr (\i → (point' i ∘)) id δR r'
    in Just $ merged δL δR l r

{- | Given two pointed words that are equal when 'lower'ed, their 'meet' is a pointed word
that has only the points that both words have in common.

If both pointed words are equal, returns the left argument. -}
meet ∷ ∀ a. (Eq a) ⇒ PW a → PW a → Maybe (PW a)
meet l r
  |         l ==        r = Just l
  | Lowered l ≠ Lowered r = Nothing
  | otherwise = let
      pL = Set.fromList ∘ F.toList ∘ pointIdxs $ l
      pR = Set.fromList ∘ F.toList ∘ pointIdxs $ r
      pI = pL `Set.intersection` pR
      δL = pL `Set.difference` pI  -- Changes that need to be made (unpoints) to L to be equal to the meet
      δR = pR `Set.difference` pI  -- Changes that need to be made (unpoints) to R to be equal to the meet
      merged ∷ Set Int → Set Int → PW a → PW a → PW a
      merged dL dR l' r'
        | Set.size dL >= Set.size dR = F.foldr (\i → (unpoint' i ∘)) id δL l'
        | otherwise                  = F.foldr (\i → (unpoint' i ∘)) id δR r'
    in Just $ merged δL δR l r

{- | Given two pointed words /l/, /r/ that are equal when 'lower'ed, their relative complement
/l - r/ is the pointed word with the points of /l/ not also present in /r/.

If both pointed words are equal, returns the left argument. -}
relComp ∷ ∀ a. (Eq a) ⇒ PW a → PW a → Maybe (PW a)
relComp l r
  |         l ==        r = Just l
  | Lowered l ≠ Lowered r = Nothing
  | otherwise = let
      pL = Set.fromList ∘ F.toList ∘ pointIdxs $ l
      pR = Set.fromList ∘ F.toList ∘ pointIdxs $ r
      pD = pL `Set.difference` pR -- Changes that need to be made (unpoints) to L to be equal to the rel. comp.
      merged ∷ Set Int → PW a → PW a
      merged = F.foldr (\i → (unpoint' i ∘)) id
    in Just $ merged pD l

{- | Given two lowered words /l/, /r/ that are equal in length, their 'delta' is is the pair of pointed
- versions of /l/ and /r/ with as many points as there are aligned discrepencies between /l/ and /r/:

 - /axbayazab Δ atbauavab = (a\>x\<ba\>y\<a\>z\<ab, a\>t\<ba\>u\<a\>v\<ab)/

- Equivalent, but aligned for easier spotting of differences:

>     a x ba y a z ab
>   Δ a t ba u a v ab
> ≡ ( a>x<ba>y<a>z<ab
>   , a>t<ba>u<a>v<ab
>   )
-}
delta ∷ ∀ a. (Eq a) ⇒ Seq a → Seq a → Maybe (PW a, PW a)
delta l r
  |          l ==         r = Just ∘ (lift &&& lift) $ l
  | S.length l ≠ S.length r = Nothing
  | otherwise = let
       δs      = S.filter (uncurry (≠) ∘ ((l !?) &&& (r !?))) [0..(S.length l - 1)]
       points_ = F.foldr (\i → (point' i ∘)) id δs ∘ lift
      in Just $ (points_ *** points_) (l, r)


-- -- TODO Floyd-Warshall variation when the carrier is a monoid
-- -- deltaM ∷ ∀ m. (Monoid m, Eq m) ⇒ Seq m → Seq m → (PW m, PW m)
-- -- deltaM l r
-- --    | l == r = (lift &&& lift) $ l
-- --    | otherwise = let
-- --        in undefined


