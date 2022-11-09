--DEPRECATED!

{-# LANGUAGE TemplateHaskell #-}
module TH where

import Language.Haskell.TH
import Data.String (IsString)
import Data.Maybe (fromJust)

enumDec :: String -> String -> [String] -> Q [Dec]
enumDec name prefix strs = sequence [ dataDec, parseDec, stringInstance ]
  where
    clause' ps x = clause ps (normalB x) []
    ctorName s = mkName (prefix ++ s)
    parseName = mkName ("parse" ++ name)

    dataDec =
      dataD (return []) (mkName name) [] Nothing
        [ normalC (ctorName s) [] | s <- strs ]
        [ derivClause Nothing [ conT ''Eq, conT ''Ord, conT ''Show ] ]

    parseDec =
      funD parseName $ clauses ++
        [ clause' [ wildP ] (conE 'Nothing) ]

    clauses =
      [ clause' [ litP (StringL s) ]
          (appE (conE 'Just) (conE (ctorName s)))
      | s <- strs ]

    stringInstance =
      instanceD (return []) (appT (conT ''IsString) (conT (mkName name)))
        [ funD (mkName "fromString")
            [ clause' [] (uInfixE (varE 'fromJust) (varE '(.)) (varE parseName)) ]
        ]
