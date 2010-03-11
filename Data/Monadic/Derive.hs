-- | 
-- Module      : Data.Monadic.Derive
-- Copyright   : Sebastian Fischer
-- License     : PublicDomain
-- Maintainer  : Sebastian Fischer <mailto:sebf@informatik.uni-kiel.de>
-- Stability   : experimental
-- 
-- Automatic deriving of monadic data types and corresponding instances.
module Data.Monadic.Derive (

  monadic, makeMData, makeShareable, makeConvertible

  ) where

import Language.Haskell
import Data.Derive.Internal.Derivation

import Control.Monad.Error
import Control.Applicative
import Control.Arrow

typeParamName, branchResName, funArgName :: String
typeParamName = "m"; branchResName = "a"; funArgName = "fun"

-- | Derives monadic datatypes and instances for explicit sharing as
--   well as conversion. Combines the other three derivations which
--   provide the same functionality split into different parts.
-- 
--   You usually need the following preamble when deriving monadic code:
-- 
-- > {-# LANGUAGE TemplateHaskell
-- >   , KindSignatures
-- >   , MultiParamTypeClasses
-- >   , FlexibleInstances
-- >   #-}
-- > import Control.Monad.Sharing
-- > import Data.Monadic.Derive
-- > import Data.DeriveTH
-- 
--   If your types contain lists, you also need to
-- 
-- > import Data.Monadic.List
-- 
-- With this prerequisites, you can derive a monadic 'Maybe' type by
-- 
-- > $(derive monadic ''Maybe)
-- 
monadic :: Derivation
monadic = derivationCustom "Monadic"
            (\ (_,d) -> concat <$> mapM ($d) [convData,shareableInst,convInsts])

-- | Generates a monadic datatype and corresponding con- and
--   destructor functions from a Haskell datatype.
-- 
--   For example, the datatype
-- 
-- > data Maybe a = Nothing | Just a
-- 
--   can be translated into its monadic counterpart by typing
-- 
-- > $(derive makeMData ''Maybe)
-- 
--   This call generates the following datatype
-- 
-- > data MMaybe m a = MNothing | MJust (m a)
-- 
--   and the following auxiliary functions for constructing and
--   matching monadic values:
-- 
-- > mNothing :: Monad m => m (MMaybe m a)
-- > mNothing = return MNothing
-- 
-- > mJust :: Monad m => m a -> m (MMaybe m a)
-- > mJust a = return (MJust a)
-- 
-- > matchMMaybe :: Monad m => m (MMaybe m a) -> m b -> (m a -> m b) -> m b
-- > matchMMaybe x n j = x >>= \x -> case x of { MNothing -> n; MJust a -> j a }
-- 
makeMData :: Derivation
makeMData = derivationCustom "MData" (convData . snd)

-- | Generates a 'Shareable' instance for a monadic datatype.
-- 
--   For example the call
-- 
-- > $(derive makeShareable ''Maybe)
-- 
--   generates the following instance:
-- 
-- > instance (Monad m, Shareable m a) => Shareable (Maybe m a) where
-- >   shareArgs fun MNothing  = return MNothing
-- >   shareArgs fun (MJust a) = fun a >>= \a -> mJust a
-- 
makeShareable :: Derivation
makeShareable = derivationCustom "Shareable" (shareableInst . snd)

-- | Generates 'Convertible' instances to convert between monadic and
--   non-monadic datatypes.
-- 
--   For example, the call
-- 
-- > $(derive makeConvertible ''Maybe)
-- 
--   generates the following instances:
-- 
-- > instance (Monad m, Convertible m a a')
-- >       => Convertible m (Maybe a) (MMaybe m a') where
-- >   convArgs fun Nothing  = mNothing
-- >   convArgs fun (Just a) = mJust (fun a)
-- > 
-- > instance (Monad m, Convertible m a' a)
-- >       => Convertible m (MMaybe m a') (Maybe a) where
-- >   convArgs fun MNothing  = return Nothing
-- >   convArgs fun (MJust a) = (a >>= fun) >>= \a -> return (Just a)
-- 
makeConvertible :: Derivation
makeConvertible = derivationCustom "Convertible" (convInsts . snd)

-- printDecls (Left s) = Left s
-- printDecls (Right ds) = trace (unlines . map prettyPrint $ ds) (Right ds)

type Conv a = Either String a

convData :: Decl -> Conv [Decl]
convData (DataDecl _ dataOrNew ctx name args cs _) = 
  do cs' <- mapM convQCons cs
     let decl = DataDecl sl dataOrNew ctx (convName True name) (m:args) cs' []
     cfuns <- concat <$> mapM (makeCFunT name args) cs
     mfun <- makeMatchFunT name args cs
     return (decl:cfuns++mfun)
 where m = KindedVar (Ident typeParamName) (KindStar `KindFn` KindStar)

convData _ = fail "Data.Monadic.Derive: no data declaration"

-- Derive does not create Symbols.
-- Hence, we cannot rely on: Ident => prefix, Symbol => infix
convName :: Bool -> Name -> Name
convName isCons name | all isSymbolChar s && isCons = Symbol (s++"~")
                     | all isSymbolChar s           = Symbol ('~':s)
                     | isCons                       = Ident  ('M':s)
                     | otherwise                    = Ident  ('m':s)
 where s = fromName name

convQCons :: QualConDecl -> Conv QualConDecl
convQCons (QualConDecl sl vs ctx con) = QualConDecl sl vs ctx <$> convCons con

convCons :: ConDecl -> Conv ConDecl

convCons (ConDecl name args) =
  ConDecl (convName True name) <$> mapM convBType args

convCons (InfixConDecl l name r) =
  do [l',r'] <- mapM convBType [l,r]
     return $ InfixConDecl l' (convName True name) r'

convCons (RecDecl name fields) =
  RecDecl (convName True name) <$> mapM convField fields
 where convField (ns,bt) = do bt' <- convBType bt
                              return (map (convName False) ns, bt')

convBType :: BangType -> Conv BangType
convBType (BangedTy   t) = BangedTy <$> convType t
convBType (UnpackedTy t) = UnpackedTy <$> convType t
convBType (UnBangedTy t) = UnBangedTy . TyApp m <$> convType t
 where m = TyVar (Ident typeParamName)

convType :: Type -> Conv Type
convType (TyVar name) = return $ TyVar name
convType (TyParen t) = TyParen <$> convType t
convType (TyApp f x) = liftM2 TyApp (convType f) (convType x)
convType (TyCon (UnQual name)) =
  return $ TyApp (TyCon (UnQual (convName True name))) m
 where m = TyVar (Ident typeParamName)
convType (TyCon (Special ListCon)) =
  return $ TyApp (TyCon (UnQual (Ident "List"))) m
 where m = TyVar (Ident typeParamName)
convType (TyFun a b) =
  liftM2 TyFun (fromBangType <$> convBType (UnBangedTy a))
               (fromBangType <$> convBType (UnBangedTy b))

convType t = fail $ "Data.Monadic.Derive: unsupported type " ++ show t

makeCFunT :: Name -> [TyVarBind] -> QualConDecl -> Conv [Decl]
makeCFunT tname targs (QualConDecl _ _ ctx con) =
  do argts <- mapM (convBType . UnBangedTy . fromBangType) (consArgs con)
     let typ = TyForall Nothing (ClassA (UnQual (Ident "Monad")) [m]:ctx) $
                 foldr (TyFun . fromBangType) (makeType tname targs) argts
     return [TypeSig sl [convName False (consName con)] typ,
             makeCFun (consName con) (consArgs con)]
 where m = TyVar (Ident typeParamName)

makeType :: Name -> [TyVarBind] -> Type
makeType name args =
  TyApp m (foldl TyApp (TyCon (UnQual (convName True name)))
                       (m:map tVar args))
 where m = TyVar (Ident typeParamName)

makeCFun :: Name -> [BangType] -> Decl
makeCFun name [] =
  PatBind sl (PVar (convName False name))
    Nothing (UnGuardedRhs rhs) (BDecls [])
 where
  rhs = App (Var (UnQual (Ident "return"))) $ Con (UnQual (convName True name))

makeCFun name argts =
  FunBind [Match sl (convName False name) (map PVar argvs)
    Nothing (UnGuardedRhs rhs) (BDecls [])]
 where
  argvs = take (length argts) $ map (Ident . (:[])) ['a'..]
  rhs   = foldr bind (App (Var (UnQual (Ident "return")))
                          (foldl App (Con (UnQual (convName True name)))
                                     (map (Var . UnQual) argvs)))
                     (map fst . filter (isBangType . snd) $ zip argvs argts)
  bind a b = InfixApp (Var (UnQual a)) (QVarOp (UnQual (Symbol ">>=")))
                      (Lambda sl [PVar a] b)

makeMatchFunT :: Name -> [TyVarBind] -> [QualConDecl] -> Conv [Decl]
makeMatchFunT (Symbol _) _ _ = return []
makeMatchFunT tname@(Ident s) targs cs =
  do ts <- mapM branchTypeC cs
     let typ = TyForall Nothing [ClassA (UnQual (Ident "Monad")) [m]] $
                 foldr TyFun (TyApp m a) (makeType tname targs:ts)
     return [TypeSig sl [name] typ,
             makeMatchFun name (map conDecl cs)]
 where
  m = TyVar (Ident typeParamName)
  a = TyVar (Ident branchResName)
  name = Ident $ "matchM" ++ s

branchTypeC :: QualConDecl -> Conv Type
branchTypeC (QualConDecl _ _ ctx con) =
  TyForall Nothing ctx <$> branchType (consArgs con)

branchType :: [BangType] -> Conv Type
branchType ts =
  foldr (TyFun . fromBangType) (TyApp m a) <$>
    mapM (convBType . UnBangedTy . fromBangType) ts
 where m = TyVar (Ident typeParamName)
       a = TyVar (Ident branchResName)

makeMatchFun :: Name -> [ConDecl] -> Decl
makeMatchFun name cs =
  FunBind [Match sl name (map PVar args) Nothing (UnGuardedRhs rhs) (BDecls [])]
 where
  args = take (1+length cs) . map (Ident . (:[])) $ ['a'..]
  var = head args
  rhs = InfixApp (Var (UnQual var))
                 (QVarOp (UnQual (Symbol ">>=")))
                 (Lambda sl [PVar var] $
                    Case (Var (UnQual var)) (zipWith makeAlt cs (tail args)))

makeAlt :: ConDecl -> Name -> Alt
makeAlt con fun =
  Alt sl (PApp (UnQual name) (map PVar args)) (UnGuardedAlt rhs) (BDecls [])
 where
  name = convName True (consName con)
  args = map Ident . zipWith (const (:"'")) (consArgs con) $ ['a'..]

  rhs = foldl App (Var (UnQual fun))
      . zipWith arg (consArgs con)
      $ map (Var . UnQual) args

  arg t x | isBangType t = App (Var (UnQual (Ident "return"))) x
          | otherwise    = x

shareableInst :: Decl -> Conv [Decl]
shareableInst (DataDecl _ _ _ name args cs _) =
  do rules <- mapM makeShareArgsRule cs
     return [InstDecl sl ctx (UnQual (Ident "Shareable")) [m,t] $
               [InsDecl $ FunBind rules]]
 where
  m   = TyVar (Ident typeParamName)
  t   = foldl TyApp (TyCon (UnQual (convName True name))) (m:map tVar args)
  ctx = ClassA (UnQual (Ident "Monad")) [m]
        : map (\a -> ClassA (UnQual (Ident "Shareable")) [m,tVar a]) args

shareableInst d = fail $ "Cannot make Shareable instance for " ++ show d

makeShareArgsRule :: QualConDecl -> Conv Match
makeShareArgsRule (QualConDecl _ [] [] con) =
  return $ Match sl (Ident "shareArgs") [PVar fun,cpat] Nothing 
             (UnGuardedRhs rhs) (BDecls [])
 where
  name = convName True $ consName con
  args = map (Ident.(:[]).fst) . zip ['a'..] $ consArgs con

  fun  = Ident funArgName
  cpat = UnQual name `PApp` map PVar args
  cexp = foldl App (Con (UnQual name)) $ map (Var . UnQual) args

  rhs  = foldr (\ (x,b) e ->
                 InfixApp (if b then
                             Var (UnQual (Ident "shareArgs"))
                              `App` Var (UnQual fun)
                              `App` Var (UnQual x)
                            else Var (UnQual fun) `App` Var (UnQual x))
                          (QVarOp (UnQual (Symbol ">>=")))
                          (Lambda sl [PVar x] e))
           (Var (UnQual (Ident "return")) `App` cexp)
           (zipWith (curry $ second isBangType) args (consArgs con))

makeShareArgsRule c = fail $ "Cannot make shareArgs rule for " ++ show c

convInsts :: Decl -> Conv [Decl]
convInsts (DataDecl _ _ _ name args cs _) =
  do rules  <- mapM makeConvToM cs
     rules' <- mapM makeConvFromM cs
     return [InstDecl sl (mctx:zipWith convCtx (map tVar args) args')
               cname [m,t,t'] [InsDecl $ FunBind rules],
             InstDecl sl (mctx:zipWith convCtx args' (map tVar args))
               cname [m,t',t] [InsDecl $ FunBind rules']]
 where
  m     = TyVar $ Ident typeParamName
  cname = UnQual $ Ident "Convertible"
  mctx  = ClassA (UnQual (Ident "Monad")) [m]
  args' = map (TyVar . Ident . (:[]) . fst) $ zip ['a'..] args
  t     = foldl TyApp (TyCon (UnQual name)) (map tVar args)
  t'    = foldl TyApp (TyCon (UnQual (convName True name))) (m:args')

  convCtx a b = ClassA cname [m,a,b]

convInsts d = fail $ "Cannot make Convertible instances for " ++ show d

makeConvToM :: QualConDecl -> Conv Match
makeConvToM (QualConDecl _ [] [] con) =
  return $ Match sl (Ident "convArgs") [PVar fun,cpat] Nothing
             (UnGuardedRhs rhs) (BDecls [])
 where
  name = consName con
  args = map (Ident.(:[]).fst) . zip ['a'..] $ consArgs con

  fun  = Ident funArgName
  cpat = UnQual name `PApp` map PVar args

  rhs  = foldl App (Var (UnQual (convName False name))) $
           map (\x -> Var (UnQual fun) `App` Var (UnQual x)) args

makeConvToM c = fail $ "Cannot make convArgs rule for " ++ show c

makeConvFromM :: QualConDecl -> Conv Match
makeConvFromM (QualConDecl _ [] [] con) =
  return $ Match sl (Ident "convArgs") [PVar fun,cpat] Nothing
             (UnGuardedRhs rhs) (BDecls [])
 where
  name = consName con
  args = map (Ident.(:[]).fst) . zip ['a'..] $ consArgs con

  fun  = Ident funArgName
  cpat = UnQual (convName True name) `PApp` map PVar args
  cexp = foldl App (Con (UnQual name)) $ map (Var . UnQual) args

  rhs  = foldr (\ (x,b) e ->
                  InfixApp
                    (if b then Var (UnQual fun) `App` Var (UnQual x)
                      else Paren $ InfixApp
                             (Var (UnQual x))
                             (QVarOp (UnQual (Symbol ">>=")))
                             (Var (UnQual fun)))
                    (QVarOp (UnQual (Symbol ">>=")))
                    (Lambda sl [PVar x] e))
           (Var (UnQual (Ident "return")) `App` cexp)
           (zipWith (curry $ second isBangType) args (consArgs con))

makeConvFromM c = fail $ "Cannot make convArgs rule for " ++ show c

fromName :: Name -> String
fromName (Ident  name) = name
fromName (Symbol name) = name

-- does not recognise unicode symbols
isSymbolChar :: Char -> Bool
isSymbolChar c = c `elem` ":'!#$%&*+./<=>?@\\^|_~"

consName :: ConDecl -> Name
consName (ConDecl name _)        = name
consName (InfixConDecl _ name _) = name
consName (RecDecl name _)        = name

consArgs :: ConDecl -> [BangType]
consArgs (ConDecl _ args)     = args
consArgs (InfixConDecl l _ r) = [l,r]
consArgs (RecDecl _ fs)       = [t | (ns,t) <- fs, _ <- ns]

tVar :: TyVarBind -> Type
tVar (UnkindedVar name) = TyVar name
tVar (KindedVar name kind) = TyKind (TyVar name) kind

isBangType :: BangType -> Bool
isBangType (UnBangedTy _) = False
isBangType _              = True

conDecl :: QualConDecl -> ConDecl
conDecl (QualConDecl _ _ _ con) = con

{- example derivations

data  List               a =  Nil |  Cons    a      (List   a)
data MList (m :: * -> *) a = MNil | MCons (m a) (m (MList m a))

mNil :: Monad m => m (MList m a)
mNil = return MNil

mCons :: Monad m => m a -> m (MList m a) -> m (MList m a)
mCons x xs = return (MCons x xs)

matchMList :: Monad m
           => m (MList m a)
           -> m b -> (m a -> m (MList m a) -> m b)
           -> m b
matchMList list nil cons =
  list >>= \list -> 
  case list of
    MNil       -> nil
    MCons x xs -> cons x xs

instance (Monad m, Shareable m a) => Shareable m (List m a)
 where
  shareArgs f MNil = return MNil
  shareArgs f (MCons x xs) =
    f x >>= \x -> f xs >>= \xs -> return (MCons x xs)

instance (Monad m, Convertible m a a') => Convertible m (List a) (MList m a')
 where
  convArgs f Nil = mNil
  convArgs f (Cons x xs) = mCons (f x) (f xs)

instance (Monad m, Convertible m a a') => Convertible m (MList m a) (List a')
 where
  convArgs f MNil = return Nil
  convArgs f (MCons x xs) =
    (x >>= f) >>= \x -> (xs >>= f) >>= \xs -> return (Cons x xs)

data  SList               a =  SNil |  SCons !a     (SList   a)
data MSList (m :: * -> *) a = MSNil | MSCons !a (m (MSList m a))

mSNil :: Monad m => m (MSList m a)
mSNil = return MSNil

mSCons :: Monad m => m a -> m (MSList m a) -> m (MSList m a)
mSCons x xs = x >>= \x -> return (MSCons x xs)

matchMSList :: Monad m
            => m (MSList m a)
            -> m b -> (m a -> m (MSList m a) -> m b)
            -> m b
matchMSList list nil cons =
  list >>= \list -> 
  case list of
    MSNil       -> nil
    MSCons x xs -> cons (return x) xs

instance (Monad m, Shareable m a) => Shareable m (MSList m a)
 where
  shareArgs f MSNil = return MSNil
  shareArgs f (MSCons x xs) =
    shareArgs f x >>= \x -> f xs >>= \xs -> return (MSCons x xs)

instance (Monad m, Convertible m a a') => Convertible m (SList a) (MSList m a')
 where
  convArgs f SNil = mSNil
  convArgs f (SCons x xs) = mSCons (f x) (f xs)

instance (Monad m, Convertible m a a') => Convertible m (MSList m a) (SList a')
 where
  convArgs f MSNil = return SNil
  convArgs f (MSCons x xs) =
    f x >>= \x -> (xs >>= f) >>= \xs -> return (Cons x xs)

-}
