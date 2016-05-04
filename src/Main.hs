{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Applicative  (liftA2, liftA3)
import           Control.Monad.Reader
import qualified Data.Map             as Map
type Name = String
type Env = Map.Map Name Val


data Val = TInt Int | TBool Bool | TArrow (Val -> Val) | Error RuntimeError

instance Show Val where
  show (TInt i)  = show i
  show (TBool b) = show b
  show (Error e)   = show e

data RuntimeError = UnknownVar Name | TypeError deriving Show

data Expr
  = Lit Val
  | Var Name
  | Arith ArithOp Expr Expr
  | If Expr Expr Expr
  | Lam Name Expr
  | App Expr Expr
  | Let Name Expr Expr
  deriving Show


data ArithOp = Add | Sub | Mul | LtEq | GtEq deriving (Show)

type Eval a = Reader Env a

eval :: Expr -> Eval Val
eval (Lit v) = return v
eval (Var v) = do
        env  <- ask
        case Map.lookup v env of
          Just val -> return val
          _        -> return (Error (UnknownVar v))

eval (Arith op e e')  = liftA2 (f op) (eval e) (eval e')
      where
        f Add (TInt x) (TInt y) = TInt (x + y)
        f Sub (TInt x) (TInt y) = TInt (x - y)
        f Mul (TInt x) (TInt y) = TInt (x * y)
        f LtEq (TInt x) (TInt y) = TBool (x <= y)
        f _ _ _ = Error TypeError

eval (If c t f) = eval c >>= \case
          TBool True  -> eval t
          TBool False -> eval f
          _             -> return (Error TypeError)

eval (Lam v e) = do
        env <- ask
        return . TArrow $ \x -> runReader (eval e) (Map.insert v x env)

eval (App e e') = liftA2 (,) (eval e) (eval e') >>= \case
          (TArrow f, x) -> return (f x)
          _             -> return (Error TypeError)

eval (Let v e b) =
        eval (Lam v e) >>= \case
          TArrow rec -> local (Map.insert v (fix rec)) (eval b)
          _          -> return (Error TypeError)

fact :: Expr
fact =
    Let "factorial" (
      Lam "n"
        (If (Arith LtEq (Var "n") (Lit (TInt 0))) (Lit (TInt 1))
            (Arith Mul (App (Var "factorial") (Arith Sub (Var "n") (Lit (TInt 1)))) (Var "n"))
        )
    ) (App (Var "factorial") (Lit (TInt 5)))

main :: IO ()
main = print $ runReader (eval fact) Map.empty
