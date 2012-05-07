module Data.Modern.Documentation
  (documentSchema)
  where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Modern.Types


data Node =
  Node {
      nodeName :: String,
      nodeBody :: Maybe (BodyType, [Node])
    }


data BodyType
  = ListBodyType
  | TupleBodyType
  | UnionBodyType
  | StructureBodyType


documentSchema :: ModernContext -> String
documentSchema context =
  let allNodes =
	foldl' (\nodesSoFar (theHash, theType) ->
		  let named name =
			Map.insert theHash
				   (Node {
					nodeName = name,
					nodeBody = Nothing
				      })
				   nodesSoFar
		  in case theType of
		       ModernInt8Type -> named "Int8"
		       ModernInt16Type -> named "Int16"
		       ModernInt32Type -> named "Int32"
		       ModernInt64Type -> named "Int64"
		       ModernWord8Type -> named "Word8"
		       ModernWord16Type -> named "Word16"
		       ModernWord32Type -> named "Word32"
		       ModernWord64Type -> named "Word64"
		       ModernFloatType -> named "Float"
		       ModernDoubleType -> named "Double"
		       ModernUTF8Type -> named "UTF8"
		       ModernBlobType -> named "Blob"
		       (ModernListType contentType) -> nodesSoFar
		       (ModernTupleType contentTypes) -> nodesSoFar
                         -- (Maybe (Array Word64 ModernType))
		       (ModernUnionType possibilities) -> nodesSoFar
		         -- (Maybe (Word8, (Array Word64 ModernType)))
		       (ModernStructureType fieldTypes) -> nodesSoFar
                         -- (Maybe (Array Word64 (ModernFieldName, ModernType)))
		       (ModernNamedType name contentType) -> nodesSoFar)
	       Map.empty
	       (Map.toList $ modernContextTypes context)
      outputNodes indentation nodes =
	intercalate (outputIndentation indentation ++ "\n")
		    (map (outputNode indentation) nodes)
      outputNode indentation node =
	case nodeBody node of
	  Nothing -> outputIndentation indentation ++ nodeName node ++ ";\n"
	  Just (bodyType, children) ->
	    outputIndentation indentation ++ nodeName node ++ " "
	    ++ (case bodyType of
                  ListBodyType -> "list"
                  TupleBodyType -> "tuple"
                  UnionBodyType -> "union"
                  StructureBodyType -> "structure")
	    ++ " {\n"
	    ++ outputNodes (indentation + 1) children
	    ++ outputIndentation indentation ++ nodeName node ++ "}\n"
      outputIndentation indentation =
	take (indentation * 4) (repeat ' ')
  in outputNodes 0 (Map.elems allNodes)

