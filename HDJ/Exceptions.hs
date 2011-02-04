{-# LANGUAGE DeriveDataTypeable #-}

module HDJ.Exceptions
( CompilerException(..)
, Control.Exception.Base.throw
) where

import Control.Exception.Base
import Data.Typeable

data CompilerException = ArityException String Integer Integer
                       | UndefinedReferenceException String
                       | UndefinedException
                       | ParserException
                       deriving (Typeable)

instance Show CompilerException where
     show (ArityException fid ex re)        = ("Syntactic error: Wrong number of arguments passed to \"" ++ fid ++ "\" function; Expected: " ++ (show ex) ++ "; Received: " ++ (show re) ++ ";")
     show (UndefinedReferenceException fid) = ("Syntactic error: Undefined reference to function \"" ++ fid)
     show (UndefinedException)              = ("Undefined error: Too bad.")
     show (ParserException)                 = ("Parser error: Too bad.")

instance Exception CompilerException