module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f  = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert


-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo. Al evaluar solo nos interesa el valor
-- del estado.
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm (Let v e)            = do n <- evalExp e
                                   update v n
                                   return Skip

stepComm (Seq Skip c1)        = return c1
stepComm (Seq c0 c1)          = do c0' <- stepComm c0
                                   return (Seq c0' c1)

stepComm (IfThenElse b c0 c1) = do vb <- evalExp b
                                   if vb then return c0 else return c1

stepComm (Repeat b c)         = return (Seq c c')
                                where c' = (IfThenElse b (Repeat b c) Skip)

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
evalExp (Const n)        = return n
evalExp (Var x)          = lookfor x

evalExp (UMinus e)       = evalUnary (negate) e 
evalExp (Plus e0 e1)     = evalBinary (+)   e0 e1
evalExp (Minus e0 e1)    = evalBinary (-)   e0 e1
evalExp (Times e0 e1)    = evalBinary (*)   e0 e1
evalExp (Div e0 e1)      = evalBinary (div) e0 e1


evalExp (VarInc x)       = modifyVariable x (succ)
evalExp (VarDec x)       = modifyVariable x (pred)


evalExp BTrue            = return True
evalExp BFalse           = return False

evalExp (Lt e0 e1)       = evalBinary (<) e0 e1
evalExp (Gt e0 e1)       = evalBinary (>) e0 e1

evalExp (And e0 e1)      = evalBinary (&&) e0 e1
evalExp (Or e0 e1)       = evalBinary (||) e0 e1
evalExp (Not e)          = evalUnary (not) e
evalExp (Eq e0 e1)       = evalBinary (==) e0 e1 
evalExp (NEq e0 e1)      = evalBinary (/=) e0 e1



modifyVariable :: MonadState m => Variable -> (Int -> Int) -> m Int 
modifyVariable x op = do n <- lookfor x
                         (let n' = op n
                          in (do update x n'
                                 return n'))




evalBinary :: MonadState m => (a -> a -> b) -> Exp a -> Exp a -> m b
evalBinary op e0 e1 = do v0 <- evalExp e0
                         v1 <- evalExp e1
                         return (op v0 v1)

evalUnary :: MonadState m => (a -> b) -> Exp a -> m b
evalUnary op e = do v <- evalExp e
                    return (op v)

