module Eval2
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

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error (Pair a Env) }

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> return (x :!: s))
  m >>= f  = StateError (\s -> do (x :!: s') <- runStateError m s
                                  runStateError (f x) s')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> lookfor' v s)
    where lookfor' v s = case M.lookup v s of
                          Nothing -> Left UndefVar
                          Just x  -> return (x :!: s)

  update v i = StateError (\s -> return (() :!: update' v i s))
   where update' = M.insert

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadError StateError where
  throw e = StateError (\_ -> Left e)

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = do (x :!: s) <- runStateError (stepCommStar p) initEnv 
            return s

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
stepComm (Let v e)            = do n <- evalExp e
                                   update v n
                                   return Skip

stepComm (Seq Skip c1)        = return c1
stepComm (Seq c0 c1)          = do c0' <- stepComm c0
                                   return (Seq c0' c1)

stepComm (IfThenElse b c0 c1) = do vb <- evalExp b
                                   if vb then return c0 else return c1

-- Ejecutamos una vez c y luego, si se cumple b, seguimos repitiendo.
stepComm (Repeat b c)         = return (Seq c c')
                                where c' = (IfThenElse b (Repeat b c) Skip)

-- Evalua una expresion
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
evalExp (Const      n)  = return  n
evalExp (Var        v)  = lookfor v
evalExp (UMinus     n)  = evalUnary  negate n
evalExp (Plus   e0 e1)  = evalBin (+)   e0 e1
evalExp (Minus  e0 e1)  = evalBin (-)   e0 e1
evalExp (Times  e0 e1)  = evalBin (*)   e0 e1
evalExp (Div    e0 e1)  = do v0 <- evalExp e0
                             v1 <- evalExp e1
                             if v1 == 0 then throw DivByZero
                                        else return (div v0 v1)

evalExp BTrue           = return True
evalExp BFalse          = return False 

evalExp (Not        e)  = evalUnary     not e
evalExp (Lt     e0 e1)  = evalBin (<)   e0 e1
evalExp (Gt     e0 e1)  = evalBin (>)   e0 e1
evalExp (And    e0 e1)  = evalBin (&&)  e0 e1
evalExp (Or     e0 e1)  = evalBin (||)  e0 e1
evalExp (Eq     e0 e1)  = evalBin (==)  e0 e1
evalExp (NEq    e0 e1)  = evalBin (/=)  e0 e1


evalBin :: (MonadState m, MonadError m) => (a -> a -> b) -> Exp a -> Exp a -> m b
evalBin op e0 e1 = do v0 <- evalExp e0
                      v1 <- evalExp e1
                      return (op v0 v1)

evalUnary :: (MonadState m, MonadError m) => (a -> b) -> Exp a -> m b
evalUnary op e = do v <- evalExp e
                    return (op v)

