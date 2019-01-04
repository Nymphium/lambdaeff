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
subst (App f a) x t = App (subst f x t) (subst a x t)
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
subst others _ _ = others

valuable :: Term -> Bool
valuable = \case
    Var _     -> True
    Handler{} -> True
    Fun{}     -> True
    Eff _     -> True
    Int _     -> True
    _         -> False

toValue :: Term -> Value
toValue = \case
    Handler eff vh effh -> HandlerV (toValue eff) vh effh
    Var x               -> VarV x
    Fun x body          -> FunV x body
    Eff eff             -> EffV eff
    Int i               -> IntV i

binapp :: Term -> Term
binapp (Add (Int i) (Int j)) = Int $ i + j
binapp (Mul (Int i) (Int j)) = Int $ i * j

hole = Var "HOLE"

-- small-step evaluation
eval1 :: (Term, Stack, Stack) -> EffectP -> ((Term, Stack, Stack), EffectP)
-- pop
eval1 (v, f : s, es) idx | valuable v = ((f v, s, es), idx)
-- result
eval1 m@(v, [], _) idx | valuable v = (m, idx)
-- apply
eval1 (App (Fun x body) v, s, es) idx | valuable v = ((subst body x v, s, es), idx)
eval1 (e@(Add v1 v2), s, es) idx | valuable v1 && valuable v2 = ((binapp e, s, es), idx)
eval1 (e@(Mul v1 v2), s, es) idx | valuable v1 && valuable v2 = ((binapp e, s, es), idx)
-- push
eval1 (App f e, s, es) idx
    | valuable f = ((e, App f : s, es), idx)
    | otherwise  = ((f, flip App e : s, es), idx)
eval1 (Perform eff e, s, es) idx
    | valuable eff && valuable e = send eff e s es
    | valuable eff               = ((e, Perform eff : s, es), idx)
    | otherwise                  = ((eff, flip Perform e : s, es), idx)
    where send eff v s es = undefined
eval1 (Let x e body, s, es) idx
    | valuable e = ((subst body x e, s, es), idx)
    | otherwise  = ((e, flip (Let x) body : s, es), idx)
eval1 (WithH v e, s, es) idx = ((e, WithH v : s, es), idx)
eval1 (Inst, s, es) idx = ((Eff idx', s, es), idx') where idx' = idx + 1
eval1 (Add e1 e2, s, es) idx
    | valuable e1 = ((e2, Add e1 : s, es), idx)
    | otherwise   = ((e1, flip Add e2 : s, es), idx)

eval :: Term -> Stack -> Stack -> EffectP -> Term
eval t s es = go (t, s, es)
    where go mod idx =
           case eval1 mod idx of
           ((v, [], _), _) | valuable v -> v
           (mod', idx')                 -> go mod' idx'

run :: Term -> Value
run t = toValue $ eval t [] [] 0
