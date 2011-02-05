{-# LANGUAGE TemplateHaskell #-}
module Generator  where

import Language.Haskell.TH
import Control.Monad
import Data.List (partition)
import Data.Maybe (isJust,fromJust)

import Data.Int
import JavaBinary
import Data.Binary.Get(lookAheadM)

data Fields = Prefix [Field]
            | Infix  Field Field

data Member = Member
  { memberName :: Name
  , memberTag  :: Maybe Integer
  , memberFields :: Fields
  }

data Field = Field
  { fieldType :: StrictTypeQ
  , fieldGet  :: ExpQ
  , fieldPut  :: ExpQ
  }

-- | 'addField' is used to add a new custom field to the end of a member.
addField :: Member -> Field -> Member
addField member field = member { memberFields = case memberFields member of
                                                  Prefix fields -> Prefix (fields ++ [field])
                                                  Infix {} -> error "addField not supported on infix fields" }

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
  , memberFields = Prefix $ map standardField memberTypes
  }

untagged :: String -> [TypeQ] -> Member
untagged name tys = Member
  { memberName = mkName name
  , memberTag  = Nothing
  , memberFields = Prefix $ map standardField tys
  }

untaggedInfix :: TypeQ -> String -> TypeQ -> Member
untaggedInfix x name y = Member
  { memberName = mkName name
  , memberTag  = Nothing
  , memberFields = Infix (standardField x) (standardField y)
  }

con0 :: Integer -> String -> Member
con0 tag name = con tag name []

con' :: Integer -> String -> [Name] -> Member
con' tag name memberNames = con tag name (map conT memberNames)

enum :: String -> String -> [(Integer, String)] -> Q [Dec]
enum name defaultName xs = packetData name $ [con0 tag n | (tag,n) <- xs]
                                          ++ [untagged defaultName [[t|Int8|]]]

enum16 :: String -> String -> [(Integer, String)] -> Q [Dec]
enum16 name defaultName xs = packetData' name [t|Int16|] $ [con0 tag n | (tag,n) <- xs] 
                                                      ++ [untagged defaultName [[t|Int16|]]]

packetData :: String -> [Member] -> Q [Dec]
packetData typeName members = packetData' typeName [t|Int8|] members

declareMember member = case memberFields member of
  Prefix xs -> normalC (memberName member) (map fieldType xs)
  Infix x y -> infixC  (fieldType x) (memberName member) (fieldType y)

memberFieldsList member = case memberFields member of
  Prefix xs -> xs
  Infix x y -> [x,y]

packetData' :: String -> TypeQ -> [Member] -> Q [Dec]
packetData' typeName tagType members =
  do let tName = mkName typeName
     dataDecl <- dataD
       (cxt [])
       tName
       []
       (map declareMember members)
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
  do names <- mapM (const (newName "x")) (memberFieldsList member)
  
     let putTag = case memberTag member of
            Nothing -> []
            Just t  -> [[| putJ (fromInteger t :: $(tagType)) |]]

     let body = doE . map noBindS
              $ putTag ++ [ [| $(fieldPut field) $(varE n) |]
                          | (field,n) <- zip (memberFieldsList member) names]
     clause
       [conP (memberName member) (map varP names)]
       (normalB body)
       []

getClause :: TypeQ -> [Member] -> ClauseQ
getClause tagType members = clause [] (normalB body) []
 where
 (tagged, untagged) = partition (isJust . memberTag) members

 body = [| do mb <- lookAheadM $
                 do tag <- getJ
                    $(caseE [| tag :: $(tagType) |] (
                        map toCase tagged ++
                        [ match wildP (normalB [| return Nothing |]) [] ]
                     ))
              case mb of
                Just a  -> return a
                Nothing -> $(case untagged of
                               u : _ -> rhs (memberName u) (memberFieldsList u)
                               []    -> [| fail "Unmatched tag" |]
                            )
        |]

 toCase member = match (litP $ integerL $ fromJust $ memberTag member)
                       (normalB [| Just `fmap` $(rhs (memberName member)
                                                     (memberFieldsList member)) |])
                       []

 rhs conName fields =
   do ns <- replicateM (length fields) (newName "x")
      doE $ [bindS (varP n) (fieldGet field) | (field,n) <- zip fields ns]
         ++ [ noBindS [| return $! $(appsE (conE conName : map varE ns) ) |] ]
