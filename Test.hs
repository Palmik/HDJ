import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Monad.Reader

import HDJ.AST
import HDJ.Parser.Defines

main = (putStr "Enter path to file with DJ definitions: ") >> getLine >>= readFile >>= (return . Enviroment . Map.fromList . right . parseFunctionDefinitions) >>= (forever . getAndEvalExpr)
    where getAndEvalExpr env = (putStr "> ") >> (getLine) >>= (\expr -> putStrLn . show $ runReader (eval (right $ parseExpression expr)) env)
    