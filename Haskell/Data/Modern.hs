module Data.Modern
  (ModernNode)
  where

import qualified Data.Array as Array
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text

import Data.Int
import Data.Word



data ModernNode
  = ModernNodeBoolValueFalse {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeBoolValueTrue {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeOrderingValueLess {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeOrderingValueEqual {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeOrderingValueGreater {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeMaybeValueNothing {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeMaybeValueJust {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeMaybeValueJust :: ModernNode
      }
  | ModernNodeInt8Value {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeInt8ValueValue :: Int8
      }
  | ModernNodeInt16Value {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeInt16ValueValue :: Int16
      }
  | ModernNodeInt32Value {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeInt32ValueValue :: Int32
      }
  | ModernNodeInt64Value {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeInt64ValueValue :: Int64
      }
  | ModernNodeNat8Value {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeNat8ValueValue :: Word8
      }
  | ModernNodeNat16Value {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeNat16ValueValue :: Word16
      }
  | ModernNodeNat32Value {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeNat32ValueValue :: Word32
      }
  | ModernNodeNat64Value {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeNat64ValueValue :: Word64
      }
  | ModernNodeFloat32Value {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeFloat32ValueValue :: Float
      }
  | ModernNodeFloat64Value {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeFloat64ValueValue :: Double
      }
  | ModernNodeUtf8Value {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeUtf8ValueValue :: Text.Text
      }
  | ModernNodeBlobValue {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeBlobValueValue :: ByteString.ByteString
      }
  | ModernNodeSigmaValue {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeSigmaValueFieldValue :: ModernNode,
        modernNodeSigmaValueSuccessor :: ModernNode
      }
  | ModernNodeNameValue {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeNameValueHash :: ModernHash
      }
  | ModernNodeNamedValue {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeNamedValueValue :: ModernHash
      }
  | ModernNodeBoolType {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeOrderingType {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeMaybeType {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeMaybeTypeContentTYpe :: ModernNode
      }
  | ModernNodeInt8Type {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeInt16Type {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeInt32Type {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeInt64Type {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeNat8Type {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeNat16Type {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeNat32Type {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeNat64Type {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeFloat32Type {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeFloat64Type {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeUtf8Type {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeBlobType {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode)
      }
  | ModernNodeFunctionType {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeFunctionTypeParameter :: ModernNode,
        modernNodeFunctionTypeResult :: ModernNode
      }
  | ModernNodeSigmaType {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeSigmaTypeFieldType :: ModernNode,
        modernNodeSigmaTypeSuccessor :: ModernNode
      }
  | ModernNodeNameType {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
      }
  | ModernNodeNamedType {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeNamedTypeName :: ModernHash,
        modernNodeNamedTypeContentType :: ModernNode
      }
  | ModernNodeUniverseType {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeUniverseTypeLevel :: Word64
      }
  | ModernNodeLambda {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeLambdaContent :: ModernNode
      }
  | ModernNodeApply {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeApplyFunction :: ModernNode,
        modernNodeApplyParameter :: ModernNode
      }
  | ModernNodeTypeFamily {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeTypeFamilyMembers :: Array.IArray Word64 ModernNode
      }
  | ModernNodeLet {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeLetMembers :: Array.IArray Word64 ModernNode,
        modernNodeLetContent :: ModernNode
      }
  | ModernNodeBackreference {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeBackreferenceIndex :: Word64
      }
  | ModernNodeBuiltin {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeBuiltinBuiltin :: Word16
      }
  | ModernNodeSatisfies {
        modernNodeCanonicalHash :: MVar (Maybe ModernHash),
        modernNodeValueType :: MVar (Maybe ModernNode),
        modernNodeSatisfiesType :: ModernNode,
        modernNodeSatisfiesPredicate :: ModernNode
      }


-- TODO make IO more polymorphic


-- in the wrapper
-- check if we have already computed this and if so just return True
-- validate that the type typechecks against universe 0
--   do we need to do this?  probably for error reporting!


typecheck :: ModernNode -> ModernNode -> IO Bool
typecheck type'@(ModernNodeSigmaType { }) value@(ModernNodeSigmaValue { }) = do
  let expectedField = modernNodeSigmaTypeFieldType type'
      successorFunction = modernNodeSigmaTypeSuccessor type'
      actualField = modernNodeSigmaValueFieldValue value
      actualSuccessor = modernNodeSigmaValueSuccessor value
  fieldPasses <- typecheck expectedField actualField
  if not fieldPasses
    then return False
    else do
      successorExpression <- getApplication successorFunction actualField
      expectedSuccessor <- evaluate successorExpression
      typecheck expectedSuccessor actualSuccessor
typecheck type'@(ModernNodeSatisfies { }) value = do
  let expectedBase = modernNodeSatisfiesType type'
      predicateFunction = modernNodeSatisfiesPredicate type'
  basePasses <- typecheck expectedBase value
  if not basePasses
    then return False
    else do
      predicateExpression <- getApplication predicateFunction value
      evaluate predicateExpression
typecheck type'@(ModernNodeFunctionType { }) value@(ModernNodeFunctionValue {})
    = do
  -- if the top node is not Apply, then we know right away whether the atomic
  -- type matches the return type (although we have to unify, of course)
  -- if it IS Apply:
  --   we have to check that the function of the Apply is a function type;
  --   that the parameter of the Apply is of the function's paramater type;
  --   that the result of the Apply is (unifies with) the overall return type
typecheck type'@(ModernNodeUniverseType { }) value@{ModernNodeSigmaType {})
   = do
  -- TODO
typecheck _ _ = return False


getApplication :: ModernNode -> ModernNode -> IO ModernNode


evaluate :: ModernNode -> IO ModernNode
