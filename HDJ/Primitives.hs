module HDJ.Primitives
( primAdd
, primSub
, primMul
, primDiv
, primCon
, exprAdd
, exprSub
, exprMul
, exprDiv
, exprCon
) where

import Data.List (genericLength)
import Control.Monad.Reader
import Control.Monad (liftM2)

import HDJ.AST
import HDJ.Exceptions

primAdd :: Expression -> Expression -> Reader Enviroment Value
primAdd e1 e2 = (liftM2 (calc)) (eval e1) (eval e2)
    where calc (Number x) (Number y) = (Number (x+y))

primSub :: Expression -> Expression -> Reader Enviroment Value
primSub e1 e2 = (liftM2 (calc)) (eval e1) (eval e2)
    where calc (Number x) (Number y) = (Number (x-y))

primMul :: Expression -> Expression -> Reader Enviroment Value
primMul e1 e2 = (liftM2 (calc)) (eval e1) (eval e2)
    where calc (Number x) (Number y) = (Number (x*y))

primDiv :: Expression -> Expression -> Reader Enviroment Value
primDiv e1 e2 = (liftM2 (calc)) (eval e1) (eval e2)
    where calc (Number x) (Number y) = (Number (x `div` y))

primCon :: Expression -> Expression -> Expression -> Reader Enviroment Value
primCon c e1 e2 = (liftM gtz) (eval c) >>= (\cond -> if cond then (eval e1) else (eval e2))
    where gtz (Number x) = x /= 0
          
exprAdd x y   = PCall (Binary primAdd) [x, y]
exprSub x y   = PCall (Binary primSub) [x, y]
exprMul x y   = PCall (Binary primMul) [x, y]
exprDiv x y   = PCall (Binary primDiv) [x, y]
exprCon x y z = PCall (Ternary primCon) [x, y, z]


