{-# LANGUAGE TemplateHaskell, TypeFamilies, CPP #-}
module Test.LazySmallCheck2012.TH (deriveSerial, deriveArgument, viewPretty, viewRaw) where

import Control.Applicative
import Control.Monad
import Data.Generics.Uniplate.Data
import Data.List
import Language.Haskell.TH

import Test.LazySmallCheck2012.Core
import Test.LazySmallCheck2012.FunctionalValues

-- Type-level template holes
data THole = THole

-- Expression-level template holes
thole = error "Haven't you replaced this yet?"

-- Utility functions
simpleCon (NormalC c ts)   = (c, map snd ts)
simpleCon (RecC c vts)     = (c, [ t | (_, s, t) <- vts ])
simpleCon (InfixC t0 c t1) = (c, map snd [t0, t1])
simpleCon _ = error "simpleCon: Unsupported datatype"

unwrapTvar (PlainTV  n)   = n
unwrapTvar (KindedTV n k) = n

-- We use GLASGOW_HASKELL since MIN_VERSION_template-haskell(2,10,0) causes a
-- CPP error
applyClass :: Name -> [Name] -> [Type]
#if __GLASGOW_HASKELL__ >= 710
applyClass cls tvars = [ AppT (ConT cls) (VarT tv) | tv <- tvars ]
#else
applyClass cls tvars = [ ClassP     cls  [VarT tv] | tv <- tvars ]
#endif

-- | Deriving a `Serial` instance
deriveSerial :: Name -> DecsQ
deriveSerial tname = do
  TyConI (DataD _ _ tvars tconstrs _) <- reify tname
  let tvars' = map unwrapTvar tvars
  when (null tconstrs) $ fail "deriveSerial: Empty datatype."
  template <- [d| instance Serial THole where
                    series = thole
              |]
  -- Change instance name
  let instName (AppT serial _) = appT (return serial) $
         foldl1 appT (conT tname : [ varT tv | tv <- tvars' ])
      instName x = return x
  -- Change instance contexts
  let instCtx (InstanceD _ name@(AppT (ConT serial) _) decls) = instanceD
         (return (applyClass serial tvars'))
         (return name) (map return decls)
      instCtx x = return x
  -- Change instance function body
  let union xs ys = [| $xs \/ $ys |]
  let body = normalB $ foldr1 union
         [ appE (varE $ mkName $ "cons" ++ show (length ts)) (conE c)
         | (c, ts) <- map simpleCon tconstrs ]
  let instFunc (ValD seriesF _ _) = valD (return seriesF) body []
      instFunc x = return x
  -- Perform transformations
  transformBiM instName template >>=
    transformBiM instCtx >>=
    transformBiM instFunc

-- | Deriving a `Argument` instance
deriveArgument :: Name -> DecsQ
deriveArgument tname = do
  TyConI (DataD _ _ tvars tconstrs _) <- reify tname
  let tconstrs' = map simpleCon  tconstrs
      tvars'    = map unwrapTvar tvars
  when (null tconstrs) $ fail "deriveSerial: Empty datatype."
  template <- [d| instance Argument THole where
                    type Base THole = THole
                    toBase _ = thole
                    fromBase _ = thole
              |]
  -- Change instance name
  let tfullname = foldl1 appT (conT tname : [ varT tv | tv <- tvars' ])
  let instName (AppT argument _) = appT (return argument) tfullname
      instName x = return x
  -- Change instance contexts
  let instCtx (InstanceD _ name@(AppT (ConT argument) _) decls) = instanceD
         (return (applyClass argument tvars'))
         (return name) (map return decls)
      instCtx x = return x
  -- Change instance of Base
  let unitT = [t| () |]
  let sumT t0 t1 = [t| Either $(t0) $(t1) |]
  let proT t0 t1 = [t| ($(t0), $(t1)) |]
  let instBase (TySynInstD base (TySynEqn _ _)) =
        tySynInstD base
          (tySynEqn [tfullname]
            (foldr sumT unitT
             [ foldr proT unitT [ [t| BaseCast $(return t) |] | t <- ts ]
             | (c, ts) <- tconstrs' ]))
      instBase x = return x
  -- Change instance for toBase
  let proE x y = [| ($x, $y) |]
  let toBaseE name = [| toBaseCast $(varE name) |]
  let buildBaseE 0 vs = [| Left  $(foldr (proE . toBaseE) [| () |] vs) |]
      buildBaseE n vs = [| Right $(buildBaseE (n - 1) vs) |]
  let instTo (FunD to _) | "toBase" `isSuffixOf` show to = funD to
        [ do vs <- mapM (const $ newName "x") ts
             let lhs = ConP c $ map VarP vs
             clause [return lhs] (normalB (buildBaseE i vs)) []
        | (i, (c, ts)) <- zip [0..] tconstrs' ]
      instTo x = return x
  -- Change instance for fromBase
  let proP x y = conP '(,) [x, y]
  let fromBaseE name = [| fromBaseCast $(varE name) |]
  let buildBaseP 0 vs = conP 'Left [ foldr (proP . varP) wildP vs ]
      buildBaseP n vs = conP 'Right [ buildBaseP (n - 1) vs ]
  let instFrom (FunD from _) | "fromBase" `isSuffixOf` show from = funD from
        [ do vs <- mapM (const $ newName "x") ts
             let rhs = foldl1 appE (conE c : map fromBaseE vs)
             clause [buildBaseP i vs] (normalB rhs) []
        | (i, (c, ts)) <- zip [0..] tconstrs' ]
      instFrom x = return x
  transformBiM instName template >>=
    transformBiM instCtx >>=
    transformBiM instFrom >>=
    transformBiM instTo >>=
    transformBiM instBase


viewRaw :: (Name -> DecsQ) -> Name -> ExpQ
viewRaw f = f >=> stringE . show

viewPretty :: (Name -> DecsQ) -> Name -> ExpQ
viewPretty f = f >=> stringE . pprint
