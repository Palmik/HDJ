module HDJ.Parser.Defines
( parseFunctionDefinitions
, parseExpression
, right
, integer
) where

import Text.Parsec
import qualified Text.Parsec.Token as TP
import Text.Parsec.Language
import Text.Parsec.Expr
import Control.Monad (liftM2, liftM3)

import HDJ.AST
import HDJ.Exceptions
import HDJ.Primitives
import HDJ.Parser.Core

functionDefinitions = manyTill functionDefinition eof

functionDefinition = do
    fid <- (spaces >> identifier)
    vars <- parameters
    (symbol "=")
    expr <- expression
    return (fid, FDefinition vars expr)
    
parameters = between (symbol "(") (symbol ")") ((spaces >> identifier) `sepBy` (symbol ","))

expression = operatorCall ((try conditionCall) <|> (parens expression) <|> (numberValue) <|> (try functionCall) <|> (variable)) <?> "expression"
functionCall = liftM2 (FCall) (identifier) (between (symbol "(") (symbol ")") (expression `sepBy` (symbol ","))) <?> "function call"
conditionCall = liftM3 (exprCon) ((symbol "if") >> expression) ((symbol "then") >> expression) ((symbol "else") >> expression >>= (\x -> (symbol "fi") >> (return x))) <?> "conditionCall"

parseFunctionDefinitions = parse functionDefinitions "(Function Definitions)"
right x = case x of
               Left  _ -> error "Left!"
               Right y -> y
parseExpression = parse expression "(Expression)"