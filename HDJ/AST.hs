module HDJ.AST
( Identifier
, Value(..)
, Expression(..)
, FPrototype(..)
, FDefinition(..)
, PDefinition(..)
, functionArity
, Enviroment(..)
, eval
, callFunction
) where

import Data.List (genericLength)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Monad.Reader

import HDJ.Exceptions

type Identifier = String
type NumberType = Integer

-- | Values our expressions can evaluate into. In our case only Integer.
data Value = Number NumberType deriving (Eq)

instance Show Value where
    show (Number x) = show x

-- | This is the AST of the language
data Expression = Value Value -- | By design, this langugae has only one type of value (natural number)
                | PCall PDefinition [Expression] -- | Call of primitive function with given PDefinition and list of arguments
                | FCall Identifier [Expression] -- | Call of function with given identifier and given list of arguments
                | Variable Identifier -- | Placeholder in function definition
          
instance Show Expression where
    show (Value x) = show x
    show (PCall _ [e1, e2]) = "(" ++ show e1 ++ " <?> " ++ show e2 ++ ")"
    show (PCall _ [c, e1, e2]) = "(if " ++ show c ++ " then " ++ show e1 ++ " else " ++ show e2 ++ " fi)"
    show (FCall fid args) = fid ++ (show args)
    show (Variable vid) = vid

-- | Since we have only one type of value out expressions can evaluate into, the prototype is just a list of identifers
type FPrototype  = [Identifier] -- | Since we only have one type, the prototype is just list of the parameters

-- | Definition consists of protoype and the definition itself (an expression)
data FDefinition = FDefinition { prototype :: FPrototype -- | Prototype
                               , definition :: Expression -- | Definition
                               } deriving (Show)
                               
-- | We have three basic kinds of primitive functions.                                             
data PDefinition = Unary   (Expression -> Reader Enviroment Value)
                 | Binary  (Expression -> Expression -> Reader Enviroment Value)
                 | Ternary (Expression -> Expression -> Expression -> Reader Enviroment Value)
                                             
functionArity = genericLength . prototype

-- | In our case, the enviroment is only list of function definitions mapped to their identifiers
data Enviroment = Enviroment { functions :: Map String FDefinition } deriving (Show)
lookupFunctionD :: String -> Enviroment -> FDefinition
lookupFunctionD fid s
    | Map.member fid (functions s) = (functions s) ! fid
    | otherwise                    = throw (UndefinedReferenceException fid)

genericReplace :: (a -> Bool) -> a -> [a] -> [a]
genericReplace _ _ [] = []
genericReplace pr y (x:xs)
    | pr x = y:(genericReplace pr y xs)
    | otherwise = x:(genericReplace pr y xs)              

-- | Calling function basicly means replacing variables with the given expressions
callFunction :: FDefinition -> [Expression] -> Expression
callFunction fdef args = replaceParameters (definition fdef) (Map.fromList $ zip (prototype fdef) (args))
    where replaceParameters :: Expression -> Map Identifier Expression -> Expression
          replaceParameters (Variable vid) args
              | Map.member vid args = (args ! vid)
              | otherwise           = error "Unknown error"
          replaceParameters (PCall def pfargs) args = (PCall def (map ((flip replaceParameters) args) pfargs)) 
          replaceParameters (FCall def pfargs) args = (FCall def (map ((flip replaceParameters) args) pfargs)) 
          replaceParameters x                  _    = x
          
isFCallValid :: String -> FDefinition -> [Value] -> Bool
isFCallValid fid fdef args
     | (genericLength $ prototype fdef) /= (genericLength args) = throw (ArityException fid (genericLength  $ prototype fdef) (genericLength args))
     | otherwise                                                = True

eval :: Expression -> Reader Enviroment Value
eval (Value x)        = return x
eval (PCall (Unary def) [e1]) = def e1
eval (PCall (Unary _)   args) = throw (ArityException "unary operator" 1 (genericLength args))
eval (PCall (Binary def) [e1, e2]) = def e1 e2
eval (PCall (Binary _)   args)     = throw (ArityException "binary operator" 2 (genericLength args))
eval (PCall (Ternary def) [e1, e2, e3]) = def e1 e2 e3
eval (PCall (Ternary _)   args)         = throw (ArityException "ternary operator" 3 (genericLength args))
eval (FCall fid args) = do
    env <- ask
    eval $ callFunction (lookupFunctionD fid env) args
eval (Variable _)     = throw UndefinedException

