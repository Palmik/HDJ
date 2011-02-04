{-#LANGUAGE NoMonomorphismRestriction #-}

module HDJ.Parser.Core
( identifier
, symbol
, integer
, parens
, eol
, operatorCall
, numberValue
, variable
) where

import Text.Parsec
import qualified Text.Parsec.Token as TP
import Text.Parsec.Language
import Text.Parsec.Expr

import HDJ.AST
import HDJ.Exceptions
import HDJ.Primitives

djDef :: LanguageDef st
djDef = emptyDef
        { TP.reservedOpNames = ["+", "-", "*", "/"]
        , TP.reservedNames   = ["if", "then", "else", "fi"]
        }

operatorCall = buildExpressionParser operatorTable

operatorTable = [ [binary "*" (exprMul) AssocLeft, binary "/" (exprDiv) AssocLeft ]
                , [binary "+" (exprAdd) AssocLeft, binary "-" (exprSub) AssocLeft ]
                ]
                
binary name fun assoc = Infix (do { reservedOp name; return fun }) assoc
prefix name fun       = Prefix (do { reservedOp name; return fun })

djLexer = TP.makeTokenParser (djDef)

numberValue = (integer) >>= (return . Value . Number)
variable    = (identifier) >>= (return . Variable)

identifier = TP.identifier djLexer
symbol     = TP.symbol     djLexer
integer    = TP.integer    djLexer
parens     = TP.parens     djLexer
reservedOp = TP.reservedOp djLexer

eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"