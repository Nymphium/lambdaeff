{-# LANGUAGE TypeOperators #-}
module Syntax where

type EffectP = Int

data Term = Var String
          | Fun String Term
          | Eff EffectP
          | Term :@: Term
          | Perform Term Term
          | Let String Term Term
          | Inst
          | WithH Term Term
          | Handler Term (String , Term) (String , String , Term)
          | Int Int
          | Term :+: Term
          | Term :*: Term
          deriving (Show, Eq)

data Value = VarV String
           | HandlerV Value (String, Term) (String, String, Term)
           | FunV String Term
           | EffV EffectP
           | IntV Int
           | Abort
           deriving (Show, Eq)

type Stack = [Term -> Term]