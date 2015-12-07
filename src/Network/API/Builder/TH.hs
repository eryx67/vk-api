{-# LANGUAGE TemplateHaskell #-}
-- | based on http://hackage.haskell.org/package/web-routes-th-0.22.3/docs/src/web-routes-th.html#derivepathinfo%27

module Network.API.Builder.TH (deriveQueryable
                              , deriveQueryable'
                              , standard
                              ) where

import           Control.Monad                 (replicateM)
import           Data.Char                     (isUpper, toLower)
import           Data.List                     (intercalate)
import           Data.List.Split               (dropInitBlank, keepDelimsL,
                                                split, whenElt)
import qualified Data.Text                     as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax    (lift)
import           Network.API.Builder.Query
import           Network.API.Builder.Queryable

-- | use Template Haskell to create 'Queryable' instances for a type.
--
-- > $(deriveQueryable ''MyQuery)
--
-- Uses the 'standard' formatter by default.
deriveQueryable :: Name -> Q [Dec]
deriveQueryable = deriveQueryable' standard


-- | use Template Haskell to create 'Queryable' instances for a type.
--
-- This variant allows the user to supply a function that transforms
-- the constructor name to a prettier rendering. It is important that
-- the transformation function generates a unique output for each
-- input. For example, simply converting the string to all lower case
-- is not acceptable, because then 'FooBar' and 'Foobar' would be
-- indistinguishable.
--
-- > $(deriveQueryable' standard ''MyQuery)
--
-- see also: 'standard'
deriveQueryable' :: (String -> String) -> Name -> Q [Dec]
deriveQueryable' formatter name = do
  c <- parseInfo name
  case c of
    Tagged cons cx keys -> do
      let context = [ mkType ''ToQuery [varT key] | key <- keys ] ++ map return cx
      i <- instanceD (sequence context) (mkType ''Queryable [mkType name (map varT keys)])
           [toQueryableFn cons]
      return [i]
    where
      toQueryableFn :: [(Name, [Name])] -> DecQ
      toQueryableFn cons = do
        inp <- newName "inp"
        let body = caseE (varE inp) $ map (uncurry conBody) cons
        funD 'toURLParams [clause [varP inp] (normalB body)  []]
      conBody :: Name -> [Name] -> MatchQ
      conBody  conName argNames = do
        args <- replicateM (length argNames) (newName "arg")
        let matchCon = conP conName (map varP args)
        match matchCon (normalB (genBody argNames args)) []

      genBody :: [Name] -> [Name] -> ExpQ
      genBody argNames args =
        listE $ zipWith argToURLParam argNames args
      argToURLParam :: Name -> Name -> ExpQ
      argToURLParam an a =
          let anQ = [| T.pack $(lift . formatter $ nameBase an) |]
          in
          [| toQuery $(anQ) $(varE a) |]

mkType :: Name -> [TypeQ] -> TypeQ
mkType con = foldl appT (conT con)

data Class = Tagged [(Name, [Name])] Cxt [Name]

parseInfo :: Name -> Q Class
parseInfo name = do
  info <- reify name
  case info of
    TyConI (DataD cx _ keys cs _)    -> return $ Tagged (map conInfo cs) cx (map conv keys)
    _                                ->  error $ "deriveQueryable - invalid input: " ++ pprint info
  where
    conInfo (RecC n args) = (n, [an | (an, _, _) <- args])
    conInfo (ForallC _ _ con) = conInfo con
    conInfo ci = error $ "deriveQueryable - invalid input: " ++ pprint ci
    conv (PlainTV nm) = nm
    conv (KindedTV nm _) = nm

-- | the standard formatter
--
-- Converts @CamelCase@ to @camel-case@.
--
-- see also: 'deriveQueryable' and 'deriveQueryable''
standard :: String -> String
standard =
    intercalate "_" . map (map toLower) . split splitter
  where
    splitter = dropInitBlank . keepDelimsL . whenElt $ isUpper
