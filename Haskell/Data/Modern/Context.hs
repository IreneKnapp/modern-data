module Data.Modern.Context
  (typeContentTypes,
   typeInContext,
   typesNotInContext,
   ensureTypesInContext)
  where

import qualified Data.Array as Array
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Modern.Hash
import Data.Modern.Types


typeContentTypes
  :: ModernType
  -> Set ModernType
typeContentTypes (ModernListType contentType) =
  Set.singleton contentType
typeContentTypes (ModernTupleType maybeContentTypes) =
  Set.fromList $ maybe [] Array.elems maybeContentTypes
typeContentTypes (ModernUnionType maybePossibilities) =
  Set.fromList $ maybe [] (Array.elems . snd) maybePossibilities
typeContentTypes (ModernStructureType maybeFields) =
  Set.fromList $ map snd $ maybe [] Array.elems maybeFields
typeContentTypes (ModernNamedType _ contentType) =
  Set.singleton contentType
typeContentTypes _ = Set.empty


typeInContext
  :: ModernType
  -> ModernContext
  -> Bool
typeInContext theType context =
  Map.member (computeTypeHash theType) (modernContextTypes context)


typesNotInContext
  :: ModernContext
  -> Set ModernType
  -> Set ModernType
typesNotInContext context theTypes = 
  let visit resultSoFar theType =
	let typeHash = computeTypeHash theType
	in case Map.lookup typeHash (modernContextTypes context) of
	     Just _ -> resultSoFar
	     Nothing -> foldl' visit
			       (Set.insert theType resultSoFar)
			       (Set.toList $ typeContentTypes theType)
  in foldl' visit Set.empty (Set.toList theTypes)


ensureTypesInContext
  :: Set ModernType
  -> ModernContext
  -> ModernContext
ensureTypesInContext theTypes context =
  let visit context theType =
	let typeHash = computeTypeHash theType
	in case Map.lookup typeHash (modernContextTypes context) of
	     Just _ -> context
	     Nothing ->
	       insert typeHash
		      theType
		      (foldl' visit
		              context 
			      (Set.toList $ typeContentTypes theType))
      insert theTypeHash theType oldContext =
        let oldTypes = modernContextTypes oldContext
            newTypes = Map.insert theTypeHash theType oldTypes
            newContext = oldContext {
		             modernContextTypes = newTypes
		           }
        in newContext
  in foldl' visit context (Set.toList theTypes)

