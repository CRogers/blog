---
layout: post
title:  "Fun with Pattern Synonyms"
date:   2014-04-20 16:28:29
categories: haskell
---

# Woop

> {-# LANGUAGE PatternSynonyms, DeriveFunctor #-}

Foo bar baz

> data Expr' a
> 	= Num' Int
> 	| Var' String
> 	| Add' a a
> 	| Sub' a a
> 	| Mul' a a
> 	deriving (Functor)

Quux

> newtype Fix f = In { out :: f (Fix f) }

> type Expr = Fix Expr'

> simplify :: Expr -> Expr
> simplify (In (Add' (In (Num' 0)) b)) = b
> simplify (In (Mul' (In (Num' 0)) b)) = In (Num' 0)
> simplify (In (Add' (In (Num' a)) (In (Num' b)))) = In (Num' $ a + b)

> pattern Num a = In (Num' a)
> pattern Var a = In (Var' a)
> pattern Add a b = In (Add' a b)
> pattern Sub a b = In (Sub' a b)
> pattern Mul a b = In (Mul' a b)

> pattern Zero = Num 0
> pattern One = Num 1

> simplify' :: Expr -> Expr
> simplify' (Add (Num 0) b) = b
> simplify' (Add Zero b) = b
> simplify' (Mul (Num 0) b) = Num 0
> simplify' (Mul Zero b) = Zero
> simplify' (Add (Num a) (Num b)) = Num $ a + b