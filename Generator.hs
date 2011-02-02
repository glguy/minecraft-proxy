{-# LANGUAGE TemplateHaskell #-}
module Generator  where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Control.Monad
import Data.List (partition)
import Data.Maybe (isJust)

import Data.Int
import Data.Word
import JavaBinary

data Member = Member
  { memberName :: Name
  , memberTag  :: Maybe Integer
  , memberFields :: [Field]
  }

data Field = Field
  { fieldType :: StrictTypeQ
  , fieldGet  :: ExpQ
  , fieldPut  :: ExpQ -> ExpQ
  }

standardField :: TypeQ -> Field
standardField ty = Field
  { fieldType = strictType isStrict ty
  , fieldGet  = [| getJ |]
  , fieldPut  = \ x -> [| putJ $(x)|]
  }

con :: Integer -> String -> [TypeQ] -> Member
con tag name memberTypes = Member
  { memberName = mkName name
  , memberTag  = Just tag
  , memberFields = map standardField memberTypes
  }

def :: String -> Member
def name = Member
  { memberName = mkName name
  , memberTag  = Nothing
  , memberFields = [standardField [t| Int8 |]]
  }

con0 tag name = con tag name []

con' tag name memberNames = con tag name (map conT memberNames)

enum name defaultName xs =
  packetData name $ [ con tag n [] | (tag,n) <- xs] ++ [def defaultName]

packetData :: String -> [Member] -> Q [Dec]
packetData typeName members =
  do let tName = mkName typeName
           
     dataDecl <- dataD
       (cxt [])
       tName
       []
       [normalC (memberName member) (map fieldType (memberFields member)) | member <- members]
       [''Show,''Read,''Eq]
     instanceDecl <- instanceD
       (cxt [])
       [t| $(conT ''JavaBinary) $(conT tName) |]
       [ funD 'putJ (map putClause members)
       , funD 'getJ [getClause members]
       ]
     return [dataDecl,instanceDecl]

putClause :: Member -> ClauseQ
putClause member =
  do names <- mapM (const (newName "x")) (memberFields member)
  
     let putTag = case memberTag member of
            Nothing -> []
            Just t  -> [[| putJ (fromIntegral t :: Int8) |]]

     let body = doE . map noBindS
              $ putTag ++ [ fieldPut field (varE n)
                          | (field,n) <- zip (memberFields member) names]
     clause
       [conP (memberName member) (map varP names)]
       (normalB body)
       []

getClause :: [Member] -> ClauseQ
getClause members = clause [] (normalB body) []
 where
 (tagged, untagged) = partition (isJust . memberTag) members

 body = [| do tag <- getJ
              $(caseE [| tag :: Int8 |]
                  (map toCase (tagged ++ untagged)))
        |]

 toCase member = case memberTag member of
    Just tag -> match (litP (integerL tag))
                      (normalB (rhs (memberName member)
                                    (memberFields member)))
                      []

    Nothing -> do
      n <- newName "x"
      match (varP n)
            (normalB [| return $! $(appE (conE (memberName member)) (varE n)) |])
            []

 rhs conName fields =
   do ns <- replicateM (length fields) (newName "x")
      doE $ [bindS (varP n) (fieldGet field) | (field,n) <- zip fields ns]
         ++ [ noBindS [| return $(appsE (conE conName : map varE ns) ) |] ]
