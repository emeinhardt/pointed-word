# `pointed-word`

This package models the domain of **(multi)pointed words** in the context of formal language and automata theory.

## What's a pointed word?

In a nutshell, a pointed word is a sequence ("string") over some type *a* consisting of 

 - 0 or more distinguished values of type *a* — **points** or **point(ed) symbols**.
 - 0 or more background sequences of context, subject to the restriction that there are no consecutive pairs
   of contexts.
   
An **unpointed word** is a pointed word that has no points, and consists only of a single (possibly empty)
context.

We can transform an unpointed word *w*, *|w| = n* into a **singly-pointed word** by specifying an index
*0 ≤ i ≤ n -1* to become a point:

 - *point 4 abbaxaaa = abba>x<aaa*
 
A **singly-pointed word** over *a* is a sequence of:

 - A possibly empty initial context.
 - A distinguished point of type *a*.
 - A possibly empty final context.

For example, *abba>x<aaa* is a pointed word with *abba* as preceding context, *aaa* as following context, and
*x* as the point or point(ed) symbol. (The metalinguistic symbols *>* and *<* surround a point.)


A **multipointed word** over *a* consists of a sequence of points, with a (possibly empty) context on either
side of the points:

 - *abba>x<aaa>y<* is a pointed word with *abba* and *aaa* as contexts, and *x* and *y* as points.
 - *>x<a>y<* is a pointed word with *a* as a context, and *x* and *y* as points.
 - *>x<>y<* is a pointed word with no contexts, and *x* and *y* as points.

We can transform a singly-pointed word into a multipointed one by specifying an index to transform
into a point if it isn't already:

 - *point 8 abba>x<aaay = abba>x<aaa>y<*
 - *point 4 abba>x<aaay = abba>x<aaay*

Dually, we can project off a point of a pointed word:

 - *unpoint 8 abba>x<aaa>y< = abba>x<aaay*
 - *unpoint 4 abba>x<aaay = abbaxaaay*

## Operations

We can concatenate pointed words:

 - *abba>x< ⋅ aaa>y< = abba>x<aaa>y<*
 - *abba ⋅ x = abbax*
 - *abba>x<a ⋅ aa>y< = abba>x<aaa>y<*


Given two pointed words whose unpointed words are the same, we can also take their
*meet* and *join*:

 - *abba>x<aaay ∨ abbaxaaa>y< = abba>x<aaa>y<*
 - *abba>x<aaa>y< ∧ abba>x<aaay = abba>x<aaay*

For a word *w*, *|w| = n*, there *2ⁿ* pointed words, with a bottom element consisting of the unpointed word,
and a top element consisting of the word where every symbol is pointed: the set of all pointed versions of a
particular unpointed word *w* form a finite Boolean (powerset) lattice.


Given two unpointed words *l, r* of the same length, we can compute an aligned pair with points at every
location where *l* and *r* differ:

 - *axbayazab Δ atbauavab = (a>x<ba>y<a>z<ab, a>t<ba>u<a>v<ab)*

Equivalent, but aligned for easier spotting of differences:

>     a x ba y a z ab
>   Δ a t ba u a v ab
> ≡ ( a>x<ba>y<a>z<ab
>   , a>t<ba>u<a>v<ab
>   )



## Why might I want to think about pointed words?

 - Modeling diffs of same-length sequences — or patches between them.
 - Reasoning about cokleisli arrows in list-zipper-like comonads.
 - Modeling particular classes of state machines ("lookaround machines") over sequences.

These are roughly the reasons I wrote the package — reasoning about a particular finite-state model of string
transformations called *bimachines*.


See the main module documentation for more information.
