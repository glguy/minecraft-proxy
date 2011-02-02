{-# LANGUAGE TemplateHaskell #-}
module Generator  where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Control.Monad
import Data.List (partition)
import Data.Maybe (isJust)
import Data.Binary

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
  , fieldPut  :: ExpQ
  }

addField member field = member { memberFields = memberFields member ++ [field] }

standardField :: TypeQ -> Field
standardField ty = Field
  { fieldType = strictType isStrict ty
  , fieldGet  = [| getJ |]
  , fieldPut  = [| putJ |]
  }

con :: Integer -> String -> [TypeQ] -> Member
con tag name memberTypes = Member
  { memberName = mkName name
  , memberTag  = Just tag
  , memberFields = map standardField memberTypes
  }

def :: TypeQ -> String -> Member
def ty name = Member
  { memberName = mkName name
  , memberTag  = Nothing
  , memberFields = [standardField ty]
  }

con0 tag name = con tag name []

con' tag name memberNames = con tag name (map conT memberNames)

enum name defaultName xs = packetData name $ [con0 tag n | (tag,n) <- xs]
                                          ++ [def [t|Int8|] defaultName]

enum16 name defaultName xs = packetData' name [t|Int16|] $ [con0 tag n | (tag,n) <- xs] 
                                                      ++ [def [t|Int16|] defaultName]

packetData typeName members = packetData' typeName [t|Int8|] members

packetData' :: String -> TypeQ -> [Member] -> Q [Dec]
packetData' typeName tagType members =
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
       [ funD 'putJ (map (putClause tagType) members)
       , funD 'getJ [getClause tagType members]
       ]
     return [dataDecl,instanceDecl]

putClause :: TypeQ -> Member -> ClauseQ
putClause tagType member =
  do names <- mapM (const (newName "x")) (memberFields member)
  
     let putTag = case memberTag member of
            Nothing -> []
            Just t  -> [[| putJ (fromIntegral t :: $(tagType)) |]]

     let body = doE . map noBindS
              $ putTag ++ [ [| $(fieldPut field) $(varE n) |]
                          | (field,n) <- zip (memberFields member) names]
     clause
       [conP (memberName member) (map varP names)]
       (normalB body)
       []

getClause :: TypeQ -> [Member] -> ClauseQ
getClause tagType members = clause [] (normalB body) []
 where
 (tagged, untagged) = partition (isJust . memberTag) members

 body = [| do tag <- getJ
              $(caseE [| tag :: $(tagType) |]
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
            (normalB [| return $! $(conE (memberName member)) $(varE n) |])
            []

 rhs conName fields =
   do ns <- replicateM (length fields) (newName "x")
      doE $ [bindS (varP n) (fieldGet field) | (field,n) <- zip fields ns]
         ++ [ noBindS [| return $(appsE (conE conName : map varE ns) ) |] ]
