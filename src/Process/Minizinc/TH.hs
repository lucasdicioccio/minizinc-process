{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell splices to generate the model Input/Output
-- datatype with introspection.
module Process.Minizinc.TH where

import Process.Minizinc.Inspect
import Language.Haskell.TH
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text


-- | Generates some Input and Ouput data types by inspecting a minizinc file.
-- Supported types are int, bools, float and their nested arrays.
genModelData :: String -> FilePath -> Q [Dec]
genModelData prefix path = do
  miface <- runIO $ inspect path
  iface <- case miface of
               Nothing -> fail "no interface" 
               Just x -> pure x
  sequence [ genFromTypeDecls prefix "Input" (_input iface)
           , genFromTypeDecls prefix "Output" (_output iface)
           ]

genFromTypeDecls :: String -> String -> TypeDeclarations -> Q Dec
genFromTypeDecls prefix base typedecls = do
  let dataname = prefix <> base
  let pairs = List.sort $ Map.assocs typedecls
  let derivations = [ DerivClause Nothing [ ConT (mkName "Show")
                                          , ConT (mkName "Eq")
                                          , ConT (mkName "Ord")
                                          , ConT (mkName "Hashable")
                                          , ConT (mkName "Generic")
                                          , ConT (mkName "ToJSON")
                                          , ConT (mkName "FromJSON")
                                          ]
                    ]
  let bang = Bang NoSourceUnpackedness NoSourceStrictness
  pure $ DataD []
     (mkName dataname)
     []
     Nothing
     [RecC (mkName dataname) [ (mkName $ Text.unpack n, bang, typeFor typedecl) | (n,typedecl) <- pairs]]
     derivations
  where
     typeFor (TypeInfo "int" False Nothing)   = ConT (mkName "Int")
     typeFor (TypeInfo "bool" False Nothing)  = ConT (mkName "Bool")
     typeFor (TypeInfo "float" False Nothing) = ConT (mkName "Float")
     typeFor (TypeInfo "int" False (Just n))  = nestedlist n $ ConT (mkName "Int")
     typeFor (TypeInfo "bool" False (Just n))  = nestedlist n $ ConT (mkName "Bool")
     typeFor (TypeInfo "float" False (Just n))  = nestedlist n $ ConT (mkName "Float")
     typeFor (TypeInfo "int" True Nothing)   = mznSet $ ConT (mkName "Int")
     typeFor (TypeInfo "bool" True Nothing)  = mznSet $ ConT (mkName "Bool")
     typeFor (TypeInfo "float" True Nothing) = mznSet $ ConT (mkName "Float")
     typeFor typedecl = error $ "unsupported type info when generating Haskell code from MiniZinc files: " <> show typedecl

     mznSet ty = AppT (ConT (mkName "MznSet")) ty
     nestedlist 0 ty = ty
     nestedlist n ty = AppT ListT (nestedlist (n-1) ty)
