module Eval3
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )


-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Ejercicio 3.a: Proponer una nueva moonada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 

newtype StateErrorTrace a = StateErrorTrace { runStateErrorTrace :: Env -> Pair (Either Error (Pair a Env)) Trace }

instance Monad StateErrorTrace where
  return x = StateErrorTrace (\s -> (Right (x :!: s) :!: ""))
  m >>= f  = StateErrorTrace (\s ->  
    let (eit :!: w) = runStateErrorTrace m s
    in case eit of
      Left e           -> (Left e :!: w)
      Right (a :!: s') -> let (eit' :!: w') = runStateErrorTrace (f a) s'
                          in  (eit' :!: w ++ w'))


-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
  pure  = return
  (<*>) = ap

-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where
  write t = StateErrorTrace (\s -> (Right (() :!: s) :!: t))

-- Ejercicio 3.d: Dar una instancia de MonFadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw e = StateErrorTrace (\s -> (Left e :!: ""))


-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v   = StateErrorTrace (\s -> lookfor' v s)
    where lookfor' v s = case M.lookup v s of
                          Nothing -> (Left UndefVar    :!: "error: " ++ v ++ " is not defined\n")
                          Just x  -> (return (x :!: s) :!: "")
          
  update v i = StateErrorTrace (\s -> (Right (() :!: insert' v i s) :!: ""))
    where insert' = M.insert






-- -- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- -- Evalua un programa en el estado nulo
eval :: Comm -> Either Error (Env, Trace)
eval p =  let (eit :!: t) = (runStateErrorTrace (stepCommStar p) initEnv)
          in eit >>= \ (x :!: s) -> return (s , t)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo segun corresponda]
stepCommStar :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm (Let v e)            = do n <- evalExp e
                                   update v n
                                   write (message v n)
                                   return Skip

stepComm (Seq Skip c1)        = return c1
stepComm (Seq c0 c1)          = do c0' <- stepComm c0
                                   return (Seq c0' c1)

stepComm (IfThenElse b c0 c1) = do vb <- evalExp b
                                   if vb then return c0 else return c1

stepComm (Repeat b c)         = return (Seq c c')
                                where c' = (IfThenElse b (Repeat b c) Skip)


-- Evalua una expresion 
evalExp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> m a
evalExp (Const n)        = return n
evalExp (Var x)          = lookfor x

evalExp (UMinus e)       = evalUnary (negate)  e 
evalExp (Plus e0 e1)     = evalBinary (+)   e0 e1
evalExp (Minus e0 e1)    = evalBinary (-)   e0 e1
evalExp (Times e0 e1)    = evalBinary (*)   e0 e1
evalExp (Div e0 e1)      = do v0 <- evalExp e0
                              v1 <- evalExp e1
                              if v1 == 0 then do write ("error: " ++ show e1 ++ "evaluates to 0 in the denominator")
                                                 throw DivByZero
                                         else return (div v0 v1)

evalExp (VarInc x)       = do n <- lookfor x
                              update x (succ n)
                              write (message x (succ n))
                              return (succ n)

evalExp (VarDec x)       = do n <- lookfor x
                              update x (pred n)
                              write (message x (pred n))
                              return (pred n)

evalExp BTrue            = return True
evalExp BFalse           = return False

evalExp (Lt e0 e1)       = evalBinary (<) e0 e1
evalExp (Gt e0 e1)       = evalBinary (>) e0 e1
evalExp (And e0 e1)      = evalBinary (&&) e0 e1
evalExp (Or e0 e1)       = evalBinary (||) e0 e1
evalExp (Not e)          = evalUnary (not) e
evalExp (Eq e0 e1)       = evalBinary (==) e0 e1 
evalExp (NEq e0 e1)      = evalBinary (/=) e0 e1


evalBinary :: (MonadState m, MonadError m, MonadTrace m) => (a -> a -> b) -> Exp a -> Exp a -> m b
evalBinary op e0 e1 = do v0 <- evalExp e0
                         v1 <- evalExp e1
                         return (op v0 v1)

evalUnary :: (MonadState m, MonadError m, MonadTrace m) => (a -> b) -> Exp a -> m b
evalUnary op e = do v <- evalExp e
                    return (op v)

message :: Variable -> Int -> String
message x n = "Let " ++ x ++ " " ++ show n ++ "\n"