module Classwork8 where
import Data.List
import Control.Applicative

type Sym = String
data Expr
    = Var Sym
    | App Expr Expr
    | Lam Sym Expr
    deriving (Eq, Read, Show)

data Type = TVar Sym
    | Type :-> Type
    deriving (Eq, Read, Show)

newtype Env = Env [(Sym, Type)] deriving Show

freeVars :: Expr -> [Sym]
freeVars (Var v) = [v]
freeVars (App t1 t2) = freeVars t1 `union` freeVars t2
freeVars (Lam v t) = freeVars t \\ [v]

freeTvars :: Type -> [Type]
freeTvars (TVar a) = [TVar a]
freeTvars (x :-> y) = (freeTvars x) ++ (freeTvars y)


emptyEnv :: Env
emptyEnv = Env []

extend :: Sym -> Type -> Env -> Env
extend s t (Env r) = Env $ (s, t) : r

apply _ Nothing = Nothing
apply l (Just []) = Just l
apply l@(TVar v) (Just (((TVar a), r):xs)) 
    | v == a = Just r
    | otherwise = apply l (Just xs)

apply (s :-> d) r = (:->) <$> (apply s r) <*> (apply d r)
    
u Nothing _ = Nothing
u _ Nothing = Nothing
u (Just(l@(TVar x))) (Just r) = 
    if (l == r) 
        then Just [] 
        else 
            if (l `elem` freeTvars(r)) 
                then Nothing 
                else Just [(l, r)]
u t (Just (TVar a)) = u (Just (TVar a)) t
u (Just (s1 :-> s2)) (Just (t1 :-> t2)) = 
    (++) <$> u (apply s1 u22) (apply t1 u22) 
         <*> u22
    where u22 = u (Just s2) (Just t2)

testU = u (Just (TVar "b" :-> TVar "b")) (Just (((TVar "g" :-> TVar "s") 
    :-> TVar "e") :-> (TVar "a" :-> TVar "s")))

testU' = u (Just (TVar "b" :-> TVar "b")) (Just ((TVar "a" :-> TVar "s") 
    :-> ((TVar "g" :-> TVar "s") :-> TVar "e")))

-- permutation
-- U(E) = U(b -> b, (a -> s) -> ((g -> s) -> e))
-- = U(U(b, (g -> s) -> e)*b, U(b, (g -> s) -> e)*(a -> s)) * U(b, (g -> s) -> e) =
-- = U((g -> s) -> e, a -> s) * [b := (g -> s) -> e]
-- = [a := (g -> s)] * [s := e] * [b := (g -> s) -> e]


eh (Env env) (Var x) s cnt = [(lookup x env, s)]
eh env (App m n) s cnt = (union) (eh env m (alpha :-> s) (cnt + 1)) (eh env n alpha (cnt + 2)) where 
    alpha = TVar $ "t" ++ show cnt
eh env (Lam x m) s cnt = (union) (eh (extend x alpha env) m beta (cnt + 2)) [(Just s, alpha :-> beta)] where 
    alpha = TVar $ "t" ++ show cnt
    beta  = TVar $ "t" ++ show (cnt + 1)

e env x s = eh env x s 0

testLam = Lam "x" $ Var "x"
test = Lam "x" $ Lam "y" $ App (Var "y") $ Lam "z" $ App (Var "y") $ Var "x"