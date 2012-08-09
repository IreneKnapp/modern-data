module Data.Modern.Documentation
  (documentSchema)
  where

import qualified Data.Array as Array
import qualified Data.ByteString.UTF8 as UTF8
import Data.Function
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Modern.Context
import Data.Modern.Hash
import Data.Modern.Initial
import Data.Modern.Types


data Node = Node [String] [NodePart]


data NodePart
  = SymbolNodePart String
  | BlockNodePart [Node]


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
  let (allNodes, allRedundantTypeHashes) =
	foldl' (\(nodesSoFar, redundantTypeHashesSoFar)
		 (theTypeHash, theType) ->
		   if typeInContext theType initialContext
		     then (nodesSoFar, redundantTypeHashesSoFar)
		     else (Map.insert theTypeHash (nodeFor theType) nodesSoFar,
	                   Set.union (Set.fromList
				       $ map computeTypeHash
				       $ catMaybes
				       $ map (\contentType ->
					        case contentType of
					          ModernNamedType _ _ ->
						   Nothing
						  _ -> Just contentType)
				       $ Set.toList
				       $ typeContentTypes theType)
				     redundantTypeHashesSoFar))
	       (Map.empty, Set.empty)
	       (Map.toList $ modernContextTypes context)
      interestingNodes =
	sortBy (on compare (\(Node names _) -> names))
	       (Map.foldlWithKey (\interestingNodesSoFar typeHash node ->
			            if Set.member typeHash
					          allRedundantTypeHashes
				      then interestingNodesSoFar
				      else node : interestingNodesSoFar)
			         []
				 allNodes)
      nodeFor ModernInt8Type = Node [] [SymbolNodePart "int8"]
      nodeFor ModernInt16Type = Node [] [SymbolNodePart "int16"]
      nodeFor ModernInt32Type = Node [] [SymbolNodePart "int32"]
      nodeFor ModernInt64Type = Node [] [SymbolNodePart "int64"]
      nodeFor ModernNat8Type = Node [] [SymbolNodePart "nat8"]
      nodeFor ModernNat16Type = Node [] [SymbolNodePart "nat16"]
      nodeFor ModernNat32Type = Node [] [SymbolNodePart "nat32"]
      nodeFor ModernNat64Type = Node [] [SymbolNodePart "nat64"]
      nodeFor ModernFloat32Type = Node [] [SymbolNodePart "float32"]
      nodeFor ModernFloat64Type = Node [] [SymbolNodePart "float64"]
      nodeFor ModernUTF8Type = Node [] [SymbolNodePart "utf8"]
      nodeFor ModernBlobType = Node [] [SymbolNodePart "blob"]
      nodeFor (ModernListType contentType) =
	let (Node _ childNodeParts) = childNodeFor contentType
	in Node [] (SymbolNodePart "list" : childNodeParts)
      nodeFor (ModernTupleType maybeContentTypes) =
	let contentTypes = maybe [] Array.elems maybeContentTypes
	in Node []
		[SymbolNodePart "tuple",
                 BlockNodePart $ map childNodeFor contentTypes]
      nodeFor (ModernUnionType maybePossibilities) =
	let possibilities = maybe [] (Array.elems . snd) maybePossibilities
	in Node []
		[SymbolNodePart "union",
		 BlockNodePart $ map childNodeFor possibilities]
      nodeFor (ModernStructureType maybeFields) =
	let fields = maybe [] Array.elems maybeFields
	in Node []
	        [SymbolNodePart "structure",
	         BlockNodePart
	          $ map (\(ModernFieldName fieldName, fieldType) ->
			   Node []
			        ((case childNodeFor fieldType of
				    Node _ childNodeParts ->
				      childNodeParts)
			         ++ [SymbolNodePart
				      $ UTF8.toString fieldName]))
		        fields]
      nodeFor (ModernNamedType (ModernTypeName name) contentType) =
	let (Node childNames childNodeParts) = nodeFor contentType
	in Node (UTF8.toString name : childNames)
		(childNodeParts ++ [SymbolNodePart $ UTF8.toString name])
      childNodeFor (ModernNamedType (ModernTypeName name) contentType) =
	Node [] [SymbolNodePart "see", SymbolNodePart $ UTF8.toString name]
      childNodeFor otherType = nodeFor otherType
      buildLine words = intercalate " " words
      linesForNode (Node _ parts) =
	let (lines, words) =
	      foldl' (\(lines, words) part ->
			 case part of
			   SymbolNodePart symbol ->
			     (lines, words ++ [symbol])
			   BlockNodePart subnodes ->
			     (lines
			      ++ [buildLine $ words ++ ["{"]]
			      ++ (map (\line -> "    " ++ line)
				      (concat $ map linesForNode subnodes)),
			      ["}"]))
		     ([], [])
		     parts
        in lines ++ [buildLine words ++ ";"]
  in concat $ map (\line -> line ++ "\n")
		  $ intercalate [""] $ map linesForNode interestingNodes

