{-# LANGUAGE ConstraintKinds #-}

module Parse where

import Control.Applicative
import Control.Lens
import qualified Data.ByteString.Char8 as S
import Data.Functor
import qualified Data.HashSet as HSet
import Data.String
import Syntax
import Text.Parser.Expression
import Text.Parser.Token
import Text.Parser.Token.Style
import Text.Trifecta
import Text.Trifecta.Combinators
import Text.Trifecta.Delta (Delta (Directed))

type MonadicParsing m = (TokenParsing m, Monad m)

(<!>) :: MonadicParsing m => m a -> m a -> m a
(<!>) l = (l <|>) . try

leffIdent :: MonadicParsing m => IdentifierStyle m
leffIdent = styleReserved .~ HSet.fromList ["let", "in", "val", "perform", "inst", "with", "handle", "handler", "fun"]
          $ emptyIdents

identifier :: MonadicParsing m => m String
identifier = ident leffIdent <?> "identifier"

reserved :: MonadicParsing m => String -> m ()
reserved = reserve leffIdent

bo :: MonadicParsing m => m Char
bo = symbolic '(' <?> "bo"

be :: MonadicParsing m => m Char
be = symbolic ')' <?> "be"

arrow :: MonadicParsing m => m String
arrow = symbol "->" <?> "arrow"

expr :: MonadicParsing m => m Term
expr  = letIn <!> handler <!> withH <!> binOps
   <?> "expr"

binOps :: MonadicParsing m => m Term
binOps = buildExpressionParser bins expr1 <?> "binops"
   where
      bins = [ [ op (symbolic '*') (:*:) AssocLeft
               , op (symbolic '/') (:/:) AssocLeft ]
             , [ op (symbolic '+') (:+:) AssocLeft
               , op (symbolic '-') (:-:) AssocLeft ]
             ]
      op s f = Infix $  s $> f

expr1 :: MonadicParsing m => m Term
expr1 = inst <|> perform <|> fun <!> app <!> expr0
   <?> "expr1"

expr0 :: MonadicParsing m => m Term
expr0 = vexpr0 <|> int
   <?> "expr0"

vexpr0 :: MonadicParsing m => m Term
vexpr0 = parens expr <|> var
   <?> "vexpr0"

app :: MonadicParsing m => m Term
app = pure (foldl (:@:)) <*> vexpr0 <*> endBy1 expr0 spaces
   <?> "app"

letIn :: MonadicParsing m => m Term
letIn = Let <$ reserved "let"
      <*> identifier
      <* symbolic '=' <*> expr
      <* reserved "in" <*> expr
   <?> "letIn"

inst :: MonadicParsing m => m Term
inst = Inst <$ reserved "inst" <* parens spaces
   <?> "inst"

perform :: MonadicParsing m => m Term
perform = Perform <$ reserved "perform" <*> var <*> expr
   <?> "perform"

withH :: MonadicParsing m => m Term
withH = WithH <$ reserved "with" <*> vexpr0 <* reserved "handle" <*> expr
   <?> "withH"

handler :: MonadicParsing m => m Term
handler = Handler <$> (reserved "handler" *> var)
   <*> parens ((,) <$ reserved "val" <*> identifier <* arrow <*> expr)
   <*> parens (parens ((,,) <$> identifier <* comma <*> identifier) <* arrow <*> expr)
   <?> "handler"

fun :: MonadicParsing m => m Term
fun = Fun <$ reserved "fun" <*> identifier <* arrow <*> expr
   <?> "fun"

int :: MonadicParsing m => m Term
int = Int . fromIntegral <$> (spaces *> decimal <* spaces)
   <?> "int"

var :: MonadicParsing m => m Term
var = Var <$> identifier
   <?> "var"

parseE :: Parser a -> String -> Result a
parseE e s = parseString (spaces *> e <* eof) initDelta s
   where initDelta = Directed (S.pack s) 0 0 0 0

parse :: String -> Result Term
parse = parseE expr
