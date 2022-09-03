{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Eval (run) where

import Control.Monad.State
import Syntax

flatfn :: Stack -> Term -> Term
flatfn = foldr (.) id

-- | 'subst t x t'' substitutes 'x' in the term 't' to 't''
subst :: Term -> String -> Term -> Term
subst (Var x) x' t
  | x == x' = t
  | otherwise = Var x
subst (f :@: a) x t = subst f x t :@: subst a x t
subst (WithH h e) x t = WithH (subst h x t) (subst e x t)
subst (Perform eff e) x t = Perform (subst eff x t) (subst e x t)
subst (Fun xf body) x t
  | xf == x = Fun xf body
  | otherwise = Fun xf (subst body x t)
subst (Handler eff (xv, ev) (xe, k, ee)) x t = Handler eff' vh effh
  where
    eff' = subst eff x t
    vh =
      (xv,) $
        if xv == x
          then ev
          else subst ev x t
    effh =
      (xe,k,) $
        if xe == x || k == x
          then ee
          else subst ee x t
subst (e1 :+: e2) x t = subst e1 x t :+: subst e2 x t
subst (e1 :-: e2) x t = subst e1 x t :-: subst e2 x t
subst (e1 :*: e2) x t = subst e1 x t :*: subst e2 x t
subst (e1 :/: e2) x t = subst e1 x t :/: subst e2 x t
-- 'let' in lambdaeff is not (implicitly) recursive!
subst (Let y e body) x t
  | x == y = Let y (subst e x t) body
  | otherwise = Let y (subst e x t) (subst body x t)
subst others _ _ = others

substs :: Term -> [(String, Term)] -> Term
substs = foldl (\t' (x, u) -> subst t' x u)

-- | is the term a value?
valuable :: Term -> Bool
valuable = \case
  Var _ -> True
  Handler {} -> True
  Fun {} -> True
  Eff _ -> True
  Int _ -> True
  Abort -> True
  _ -> False

binapp :: Term -> Term
binapp (Int i :+: Int j) = Int $ i + j
binapp (Int i :-: Int j) = Int $ i - j
binapp (Int i :*: Int j) = Int $ i * j
binapp (Int i :/: Int j) = Int $ div i j

-- the hole in evaluation context
hole = Var "□"

kfun :: Stack -> Term
kfun es = do
  let var = "◇"
  Fun var $ flatfn es (Var var)

vh (Handler _ it _) = it

effh (Handler _ _ it) = it

-- small-step evaluation
eval1 :: MonadState EffectP m => (Term, Stack, Stack) -> m (Term, Stack, Stack)
-- pop
eval1 (v, f : s, es) | valuable v = pure (f v, s, es)
-- result
eval1 m@(v, [], _) | valuable v = pure m
-- -- apply
eval1 (Fun x body :@: v, s, es) | valuable v = pure (subst body x v, s, es)
eval1 (e@(e1 :+: e2), s, es)
  | valuable e1 && valuable e2 = pure (binapp e, s, es)
  | valuable e1 = pure (e2, (e1 :+:) : s, es)
  | otherwise = pure (e1, (:+: e2) : s, es)
eval1 (e@(e1 :-: e2), s, es)
  | valuable e1 && valuable e2 = pure (binapp e, s, es)
  | valuable e1 = pure (e2, (e1 :-:) : s, es)
  | otherwise = pure (e1, (:-: e2) : s, es)
eval1 (e@(e1 :*: e2), s, es)
  | valuable e1 && valuable e2 = pure (binapp e, s, es)
  | valuable e1 = pure (e2, (e1 :*:) : s, es)
  | otherwise = pure (e1, (:*: e2) : s, es)
eval1 (e@(e1 :/: e2), s, es)
  | valuable e1 && valuable e2 = pure (binapp e, s, es)
  | valuable e1 = pure (e2, (e1 :/:) : s, es)
  | otherwise = pure (e1, (:/: e2) : s, es)
-- -- push
eval1 (f :@: e, s, es)
  | valuable f = pure (e, (f :@:) : s, es)
  | otherwise = pure (f, (:@: e) : s, es)
eval1 (pf@(Perform eff e), s, es)
  | valuable eff && valuable e = pure $ send eff e s es
  | valuable eff = pure (e, Perform eff : s, es)
  | otherwise = pure (eff, flip Perform e : s, es)
  where
    -- take top of the stack as 'f'
    send eff v s@(f : s') es =
      case f hole of
        -- with (handler eff' _ ((x, k) -> e)) □
        WithH (Handler eff' _ (x, k, e)) hole
          | eff' == eff -> do
            let k' = kfun es
                e' = substs e [(x, v), (k, k')]
            -- 'f' remains in 's': it means the handler is *deep*
            (e', s, [])
          | otherwise -> resend
        _ -> resend
      where
        resend = (pf, s', f : es)
    send _ _ [] es = (Abort, s, es)
eval1 (Let x e body, s, es)
  | valuable e = pure (subst body x e, s, es)
  | otherwise = pure (e, flip (Let x) body : s, es)
eval1 (WithH h e, s, es)
  | valuable e = do
    let (x, ev) = vh h
    pure (subst ev x e, s, es)
  | otherwise = pure (e, WithH h : s, es)
eval1 (Inst, s, es) = do
  i <- get
  let idx' = i + 1
  put idx'
  pure (Eff idx', s, es)

eval :: Term -> Stack -> Stack -> EffectP -> Term
eval t s es = go (t, s, es)
  where
    go mod idx =
      case flip runState idx $ eval1 mod of
        ((v, [], _), _) | valuable v -> v
        (mod', idx') -> go mod' idx'

run :: Term -> Term
run t = eval t [] [] 0
