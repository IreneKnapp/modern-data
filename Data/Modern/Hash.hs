module Data.Modern.Hash
  (computeTypeHash)
  where

import qualified Data.Array as Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Digest.Murmur3 as Hash

import Data.Modern.Types


computeTypeHash :: ModernType -> ModernHash
computeTypeHash modernType =
  ModernHash
   $ Hash.asByteString
      $ Hash.hash
         $ computeTypeByteString modernType


computeTypeByteString :: ModernType -> ByteString
computeTypeByteString modernType =
  let typeHelper modernType =
        case modernType of
          ModernInt8Type -> UTF8.fromString "Int8"
          ModernInt16Type -> UTF8.fromString "Int16"
          ModernInt32Type -> UTF8.fromString "Int32"
          ModernInt64Type -> UTF8.fromString "Int64"
          ModernWord8Type -> UTF8.fromString "Word8"
          ModernWord16Type -> UTF8.fromString "Word16"
          ModernWord32Type -> UTF8.fromString "Word32"
          ModernWord64Type -> UTF8.fromString "Word64"
          ModernFloatType -> UTF8.fromString "Float"
          ModernDoubleType -> UTF8.fromString "Double"
          ModernUTF8Type -> UTF8.fromString "UTF8"
          ModernBlobType -> UTF8.fromString "Blob"
          ModernListType contentType ->
            BS.concat [UTF8.fromString "List(",
                       typeHelper contentType,
                       UTF8.fromString ")"]
          ModernTupleType maybeContentTypes ->
            BS.concat [UTF8.fromString "Tuple(",
                       BS.intercalate
                        (UTF8.fromString ",")
                        (case maybeContentTypes of
                          Nothing -> []
                          Just contentTypes ->
                            map typeHelper $ Array.elems contentTypes),
                       UTF8.fromString ")"]
          ModernUnionType maybePossibilities ->
            BS.concat [UTF8.fromString "Union(",
                       BS.intercalate
                        (UTF8.fromString ",")
                        (case maybePossibilities of
                           Nothing -> []
                           Just (_, possibilities) ->
                             map typeHelper $ Array.elems possibilities),
                       UTF8.fromString ")"]
          ModernStructureType maybeFields ->
            BS.concat [UTF8.fromString "Structure(",
                       BS.intercalate
                        (UTF8.fromString ",")
                        $ map (\(ModernFieldName fieldName, fieldType) ->
                                  BS.concat [fieldName,
                                             BS.pack [0x00],
                                             typeHelper fieldType])
                              (case maybeFields of
                                 Nothing -> []
                                 Just fields -> Array.elems fields),
                       UTF8.fromString ")"]
          ModernNamedType (ModernTypeName name) contentType ->
            BS.concat [UTF8.fromString "Named(",
                       name,
                       BS.pack [0x00],
                       typeHelper contentType,
                       UTF8.fromString ")"]
  in BS.concat [UTF8.fromString "Type(",
                typeHelper modernType,
                UTF8.fromString ")"]

