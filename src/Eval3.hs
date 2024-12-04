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

-- Traza vacia
emptyTrace :: Trace
emptyTrace = ""

-- Ejercicio 3.a: Proponer una nueva moonada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 

newtype StateErrorTrace a = StateErrorTrace { runStateErrorTrace :: Env -> Either Error (Pair (Pair a Env) Trace) }


instance Monad StateErrorTrace where
  return x = StateErrorTrace (\s -> return ((x :!: s) :!: emptyTrace))
  m >>= f  = StateErrorTrace (\s -> do ((a :!: s') :!: w)    <- runStateErrorTrace m s
                                       ((a' :!: s'') :!: w') <- runStateErrorTrace (f a) s'
                                       return ((a' :!: s'') :!: w ++ w'))
                                        
instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
  pure  = return
  (<*>) = ap

-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where
  write w = StateErrorTrace (\s -> return ((() :!: s) :!: w))

-- Ejercicio 3.d: Dar una instancia de MonFadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw e = StateErrorTrace (\_ -> Left e)

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v   = StateErrorTrace (\s -> lookfor' v s)
    where lookfor' v s = case M.lookup v s of
                            Nothing -> Left UndefVar
                            Just x  -> Right ((x :!: s) :!: emptyTrace)

  update v i = StateErrorTrace (\s -> return ((() :!: insert' v i s) :!: emptyTrace))
    where insert' = M.insert

-- -- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- -- Evalua un programa en el estado nulo
eval :: Comm -> Either Error (Env, Trace)
eval p = do ((x :!: s) :!: w) <- runStateErrorTrace (stepCommStar p) initEnv
            return (s, w)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo segun corresponda]
stepCommStar :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm (Let v e)            = do n <- evalExp e
                                   update v n
                                   write (letMessage v n)
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

letMessage :: Show a => Variable -> a -> String
letMessage x n = "Let " ++ x ++ " " ++ show n ++ "; "