---
layout: post
title:  "Fun with Pattern Synonyms"
date:   2014-04-20 16:28:29
categories: haskell
---

One of the awesome new features in GHC 7.8 is Pattern Synonyms - they allow you give a name to a pattern and reuse them so you don't need to write the same boilerplate in multiple places. Pattern Synonyms help in a lot of situations, but are especially useful when we try to 
use recursion schemes to abstract away how we recurse over data strutures. We'll be focusing on this today.

First off, let's define the usual preamble we need for the functions we'll write later on:

> {-# LANGUAGE PatternSynonyms, DeriveFunctor, UndecidableInstances #-} 

A Brief Introduction to Functor Fixpoints and Recursion Schemes
---

<small>*You can <a href="#sect2">skip this section</a> if you understand functor fixpoints.*</small>

We'll look at how to write some recursive functions over a simple AST - how annoying it is to rewrite the structure of the recursion, how functor fixpoints can help us to remove this boilerplate but introduce their own and finally how Pattern Synonyms can reduce this.

The normal way you'd write a an abstract syntax tree for expressions in Haskell would be something like this:

> data Expression
>   = Number Int
>   | Variable String
>   | AddE Expression Expression
>   | SubE Expression Expression
>   deriving Show

We can't evaluate these expressions without a mapping from `Variable -> Int`, but we can write a function to simplify these expressions:

> simplifyE :: Expression -> Expression
> simplifyE (Number x)                   = Number x
> simplifyE (Variable n)                 = Variable n 
> simplifyE (AddE (Number 0) b)          = simplifyE b
> simplifyE (AddE a (Number 0))          = simplifyE a
> simplifyE (AddE (Number x) (Number y)) = Number (x + y)
> simplifyE (AddE a b)                   = AddE (simplifyE a) (simplifyE b)
> -- ... similar for SubE ...

However, we have to explicitly write out the structure of the recursion - we must specify that `Number` and `Variable` remain the same and that the subexpressions of `AddE` and `SubE` should also be passed through the same `simplify` function. If we were to write another recursive function over `Expression` we'd have to rewrite all this recursion structure!

We can use Functor Fixpoints to help us here. Rather than writing the the simplistic AST before, we can write the expression AST like so:

> data Expr' a
> 	= Num' Int
> 	| Var' String
> 	| Add' a a
> 	| Sub' a a
> 	| Mul' a a
> 	deriving (Show, Functor)

Here we have *paramterised* the data type in terms of it's subexpressions so we now have a type parameter for all recursive occurences of `Expression`.

* `Expr' a` represents expressions of depth one.
* `Expr' (Expr' a)` represents expressions of depth two.
* `Expr' (Expr' (Expr' a))` represents expressions of depth three.

But how do we represent expressions of arbitrary depths? We can use the `Fix` datatype:

> newtype Fix f = In { out :: f (Fix f) }

This looks like the y-combinator - and indeed it is, but for types! We can use a type synonym to define `Expr`, expressions of arbitrary depth:

> type Expr = Fix Expr'

If we expand out this out step by step we get: `Fix Expr'` -> `Expr' (Fix Expr')` -> `Expr' (Expr' (Fix Expr'))` etc. To make expressions we have to wrap each type of `Expr` in `In` - `Add' (Num' 1) (Var' "b")` becomes `In $ Add' (In $ Num' 1) (In $ Var "b")`

We also need to define an instances for `Fix` for `Show` (requires `UndecidableInstances`):

> instance Show (f (Fix f)) => Show (Fix f) where
>     show (In f) = "(" ++ show f ++ ")"

We can now use the fact that `Expr` is a functor to write recursion schemes like `bottomUp` that describe how we recurse over it:

> bottomUp :: (Fix f -> Fix f) -> Fix f -> Fix f
> bottomUp f = f . In . fmap (bottomUp f) . out

<a href="#" name="sect2">Using Pattern Synonyms to Simplify Fixed Expressions</a>
---

We can now write simplify, this time using the fixed point version so we don't have to explicitly write out the recursion scheme:

> simplify :: Expr -> Expr
> simplify (In (Add' (In (Num' 0)) b)) = b
> simplify (In (Mul' (In (Num' 0)) b)) = In (Num' 0)
> simplify (In (Add' (In (Num' a)) (In (Num' b)))) = In (Num' $ a + b)

`simplify` is just one step of the recursion - to apply it over the whole tree we must use `bottomUp simplify`.

But we've now we have all this `In` noise in our pattern matches and on the right hand sides of our function definition. We can use pattern synonyms to get rid of this - let's define a pattern synonym for each variant of our `Expr'` type.

> pattern Num a = In (Num' a)
> pattern Var a = In (Var' a)
> pattern Add a b = In (Add' a b)
> pattern Sub a b = In (Sub' a b)
> pattern Mul a b = In (Mul' a b)

`pattern PName a <- [pattern]` is called a ? pattern and only allows us to use it for matching. We have used the `pattern PName a = [pattern]` syntax which allows us to use it in the reverse - we can now write `Num 3` as an expression and this will expand out to `In (Num 3)`. `simplify` now looks like:

> simplify' :: Expr -> Expr
> simplify' (Add (Num 0) b) = b
> simplify' (Mul (Num 0) b) = Num 0
> simplify' (Add (Num a) (Num b)) = Num $ a + b

Much nicer! In fact, it looks very similar to the non-fixed `Expression` version. We can go even further and define patterns for `Zero` and `One` so we don't have to repeat `Num 0` and `Num 1`:

> pattern Zero = Num 0
> pattern One = Num 1

> simplify'' :: Expr -> Expr
> simplify'' (Add Zero b) = b
> simplify'' (Mul Zero b) = Zero
> simplify'' (Mul a One) = a

Problems
---

We have to write out a new pattern for each variant we have in our fixed datatype. This could be avoided if we used Template Haskell to generate the patterns for us automatically; however Template Haskell does not currently support Pattern Synonyms. If you want to it too, [place a comment on this ticket][2] saying you are interested and it'll hopefully make it's way into GHC.

The code
---

This entire post is a Literate Haskell file you can load up in GHCi directly. You can [download it here][1].

[0]: http://patrickthomson.ghost.io/an-introduction-to-recursion-schemes/
[1]: ?
[2]: ?