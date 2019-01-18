{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Eval where

import Syntax

flatfn :: Stack -> Term -> Term
flatfn = foldr (.) id

subst :: Term -> String -> Term -> Term
subst (Var x) x' t
    | x == x' = t
    | otherwise = Var x
subst (f :@: a) x t = subst f x t :@: subst a x t
subst (WithH h e) x t = WithH (subst h x t) (subst e x t)
subst (Perform eff e) x t = Perform (subst eff x t) (subst e x t)
subst (Fun xf body) x t | xf == x = Fun xf body
    | otherwise = Fun xf (subst body x t)
subst (Handler eff (xv, ev) (xe, k, ee)) x t = Handler eff' vh effh
    where eff' = subst eff x t
          vh   = (xv, ) $
              if xv == x
                 then ev
                 else subst ev x t
          effh = (xe, k, ) $
              if xe == x || k == x
                 then ee
                 else subst ee x t
subst (e1 :+: e2) x t = subst e1 x t :+: subst e2 x t
subst (e1 :-: e2) x t = subst e1 x t :-: subst e2 x t
subst (e1 :*: e2) x t = subst e1 x t :*: subst e2 x t
subst (e1 :/: e2) x t = subst e1 x t :/: subst e2 x t
subst (Let y e body) x t
    | x == y = Let y (subst e x t) body
    | otherwise = Let y (subst e x t) (subst body x t)
subst others _ _ = others

substs :: Term -> [(String, Term)] -> Term
substs = foldl (\t' (x, u) -> subst t' x u)

valuable :: Term -> Bool
valuable = \case
    Var _     -> True
    Handler{} -> True
    Fun{}     -> True
    Eff _     -> True
    Int _     -> True
    Abort     -> True
    _         -> False

binapp :: Term -> Term
binapp (Int i :+: Int j) = Int $ i + j
binapp (Int i :-: Int j) = Int $ i - j
binapp (Int i :*: Int j) = Int $ i * j
binapp (Int i :/: Int j) = Int $ div i j

hole = Var "HOLE"

kfun :: Stack -> Term
kfun es = do
        let var = "KHOLE"
        Fun var $ flatfn es (Var var)

vh (Handler _ it _) = it
effh (Handler _ _ it) = it

-- small-step evaluation
eval1 :: (Term, Stack, Stack) -> EffectP -> ((Term, Stack, Stack), EffectP)
-- pop
eval1 (v, f : s, es) idx | valuable v = ((f v, s, es), idx)
-- result
eval1 m@(v, [], _) idx | valuable v = (m, idx)
-- apply
eval1 (Fun x body :@: v, s, es) idx | valuable v = ((subst body x v, s, es), idx)
eval1 (e@(v1 :+: v2), s, es) idx | valuable v1 && valuable v2 = ((binapp e, s, es), idx)
eval1 (e@(v1 :-: v2), s, es) idx | valuable v1 && valuable v2 = ((binapp e, s, es), idx)
eval1 (e@(v1 :*: v2), s, es) idx | valuable v1 && valuable v2 = ((binapp e, s, es), idx)
eval1 (e@(v1 :/: v2), s, es) idx | valuable v1 && valuable v2 = ((binapp e, s, es), idx)
-- push
eval1 (f :@: e, s, es) idx
    | valuable f = ((e, (f :@:) : s, es), idx)
    | otherwise  = ((f, (:@: e) : s, es), idx)
eval1 (pf@(Perform eff e), s, es) idx
    | valuable eff && valuable e = send eff e s es
    | valuable eff               = ((e, Perform eff : s, es), idx)
    | otherwise                  = ((eff, flip Perform e : s, es), idx)
    where send eff v s es =
           case s of
           f : s ->
                case f hole of
                WithH (Handler eff' (xv, ev) (xeff, k, eeff)) hole
                    | eff' == eff -> do
                        let kf = kfun es
                            eeff' = substs eeff [(xeff, v), (k, kf)]
                        ((eeff', f : s, []), idx)
                    | otherwise   -> resend
                _ -> resend
                where resend = ((pf, s, f : es), idx)
           [] -> ((Abort, s, es), idx)
eval1 (Let x e body, s, es) idx
    | valuable e = ((subst body x e, s, es), idx)
    | otherwise  = ((e, flip (Let x) body : s, es), idx)
eval1 (WithH h e, s, es) idx
    | valuable e = do
        let (x, ev) = vh h
        ((subst ev x e, s, es), idx)
    | otherwise  = ((e, WithH h : s, es), idx)
eval1 (Inst, s, es) idx = ((Eff idx', s, es), idx') where idx' = idx + 1
eval1 (e1 :+: e2, s, es) idx
    | valuable e1 = ((e2, (e1 :+:) : s, es), idx)
    | otherwise   = ((e1, (:+: e2) : s, es), idx)
eval1 (e1 :-: e2, s, es) idx
    | valuable e1 = ((e2, (e1 :-:) : s, es), idx)
    | otherwise   = ((e1, (:-: e2) : s, es), idx)
eval1 (e1 :*: e2, s, es) idx
    | valuable e1 = ((e2, (e1 :*:) : s, es), idx)
    | otherwise   = ((e1, (:*: e2) : s, es), idx)
eval1 (e1 :/: e2, s, es) idx
    | valuable e1 = ((e2, (e1 :/:) : s, es), idx)
    | otherwise   = ((e1, (:/: e2) : s, es), idx)

eval :: Term -> Stack -> Stack -> EffectP -> Term
eval t s es = go (t, s, es)
    where go mod idx =
           case eval1 mod idx of
           ((v, [], _), _) | valuable v -> v
           (mod', idx')                 -> go mod' idx'

run :: Term -> Term
run t = eval t [] [] 0
