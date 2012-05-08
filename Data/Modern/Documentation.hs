module Data.Modern.Documentation
  (documentSchema,
   quoteString,
   escapeString)
  where

import qualified Data.ByteString.UTF8 as UTF8
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Modern.Context
import Data.Modern.Initial
import Data.Modern.Types


data Node =
  Node {
      nodeName :: Maybe String,
      nodeType :: Maybe NodeType,
      nodeBody :: Maybe [Node]
    }


data NodeType
  = Int8NodeType
  | Int16NodeType
  | Int32NodeType
  | Int64NodeType
  | Nat8NodeType
  | Nat16NodeType
  | Nat32NodeType
  | Nat64NodeType
  | Float32NodeType
  | Float64NodeType
  | UTF8NodeType
  | BlobNodeType
  | ListNodeType
  | TupleNodeType
  | UnionNodeType
  | StructureNodeType


documentSchema :: ModernContext -> String
documentSchema context =
  let allNodes = foldl' (\nodesSoFar (theTypeHash, theType) ->
			   if typeInContext theType initialContext
			     then nodesSoFar
			     else Map.insert theTypeHash
					     (nodeFor theType)
					     nodesSoFar)
	                Map.empty
	                (Map.toList $ modernContextTypes context)
      primitiveNode theNodeType =
	Node {
	    nodeName = Nothing,
	    nodeType = Just theNodeType,
	    nodeBody = Nothing
	  }
      nodeFor ModernInt8Type = primitiveNode Int8NodeType
      nodeFor ModernInt16Type = primitiveNode Int16NodeType
      nodeFor ModernInt32Type = primitiveNode Int32NodeType
      nodeFor ModernInt64Type = primitiveNode Int64NodeType
      nodeFor ModernNat8Type = primitiveNode Nat8NodeType
      nodeFor ModernNat16Type = primitiveNode Nat16NodeType
      nodeFor ModernNat32Type = primitiveNode Nat32NodeType
      nodeFor ModernNat64Type = primitiveNode Nat64NodeType
      nodeFor ModernFloat32Type = primitiveNode Float32NodeType
      nodeFor ModernFloat64Type = primitiveNode Float64NodeType
      nodeFor ModernUTF8Type = primitiveNode UTF8NodeType
      nodeFor ModernBlobType = primitiveNode BlobNodeType
      nodeFor (ModernListType contentType) =
	Node {
	    nodeName = Nothing,
	    nodeType = Just ListNodeType,
	    nodeBody = Nothing
	  }
      nodeFor (ModernTupleType contentTypes) =
        -- (Maybe (Array Nat64 ModernType))
	Node {
	    nodeName = Nothing,
	    nodeType = Just TupleNodeType,
	    nodeBody = Nothing
	  }
      nodeFor (ModernUnionType possibilities) =
        -- (Maybe (Nat8, (Array Nat64 ModernType)))
        Node {
            nodeName = Nothing,
	    nodeType = Just UnionNodeType,
	    nodeBody = Nothing
          }
      nodeFor (ModernStructureType fieldTypes) =
        -- (Maybe (Array Nat64 (ModernFieldName, ModernType)))
        Node {
            nodeName = Nothing,
	    nodeType = Just StructureNodeType,
            nodeBody = Nothing
          }
      nodeFor (ModernNamedType (ModernTypeName name) contentType) =
        Node {
            nodeName = Just $ UTF8.toString name,
	    nodeType = Nothing,
	    nodeBody = Nothing
	  }
      outputNodes indentation nodes =
	intercalate (outputIndentation indentation ++ "\n")
		    (map (outputNode indentation) nodes)
      outputNode indentation node =
	let headerItems =
	      concat [case nodeName node of
		        Nothing -> []
		        Just name -> [quoteString name],
		      case nodeType node of
		        Nothing -> []
                        Just Int8NodeType -> ["int8"]
                        Just Int16NodeType -> ["int16"]
                        Just Int32NodeType -> ["int32"]
                        Just Int64NodeType -> ["int64"]
                        Just Nat8NodeType -> ["nat8"]
                        Just Nat16NodeType -> ["nat16"]
                        Just Nat32NodeType -> ["nat32"]
                        Just Nat64NodeType -> ["nat64"]
                        Just Float32NodeType -> ["float32"]
                        Just Float64NodeType -> ["float64"]
                        Just UTF8NodeType -> ["utf8"]
                        Just BlobNodeType -> ["blob"]
                        Just ListNodeType -> ["list"]
                        Just TupleNodeType -> ["tuple"]
                        Just UnionNodeType -> ["union"]
                        Just StructureNodeType -> ["structure"]]
	    header = intercalate " " headerItems
	 in case nodeBody node of
	      Nothing ->
		outputIndentation indentation ++ header ++ ";\n"
	      Just children ->
	        outputIndentation indentation ++ header ++ " {\n"
	        ++ outputNodes (indentation + 1) children
	        ++ outputIndentation indentation ++ "}\n"
      outputIndentation indentation =
	take (indentation * 4) (repeat ' ')
  in outputNodes 0 (Map.elems allNodes)


quoteString :: String -> String
quoteString unquoted = "'" ++ escapeString unquoted ++ "'"


escapeString :: String -> String
escapeString unescaped =
  concat $ map (\c -> if c == '\''
                        then "''"
			else [c])
               unescaped

