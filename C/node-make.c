#include <math.h>
#include <string.h>
#include "modern.h"
#include "internal.h"


HELPER void helper_bool_false_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_bool_true_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_ordering_less_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_ordering_equal_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_ordering_greater_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_maybe_nothing_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_maybe_just_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_int8_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_int16_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_int32_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_int64_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_nat8_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_nat16_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_nat32_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_nat64_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_float32_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_float64_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_utf8_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_blob_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_sigma_value_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_name_value_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_name_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_named_value_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_bool_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_ordering_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_maybe_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_int8_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_int16_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_int32_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_int64_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_nat8_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_nat16_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_nat32_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_nat64_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_float32_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_float64_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_utf8_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_blob_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_function_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_sigma_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_named_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_universe_type_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_lambda_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_apply_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_type_family_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_let_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_backreference_finalize
  (struct modern_library *library, void *retainable);
HELPER void helper_builtin_finalize
  (struct modern_library *library, void *retainable);


INTERNAL modern *default_bool_false_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_bool_false_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_bool_value_false;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_bool_false_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_bool_true_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_bool_true_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_bool_value_true;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_bool_true_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_ordering_less_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_ordering_less_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_ordering_value_less;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_ordering_less_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_ordering_equal_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_ordering_equal_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_ordering_value_equal;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_ordering_equal_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_ordering_greater_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_ordering_greater_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_ordering_value_greater;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_ordering_greater_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_maybe_nothing_make
    (modern_library *library_in,
     void *type)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_maybe_nothing_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_maybe_value_nothing;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_maybe_nothing_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_maybe_just_make
    (modern_library *library_in,
     void *type,
     void *content_value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_maybe_just_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_maybe_value_just;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.maybe_value.content_value = content_value;
    
    return (modern *) result;
}


HELPER void helper_maybe_just_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_finalize(library, node->specifics.maybe_value.content_value);
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_int8_make
    (modern_library *library_in,
     int8_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_int8_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_int8_value;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.int8_value = value;
    
    return (modern *) result;
}


HELPER void helper_int8_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_int16_make
    (modern_library *library_in,
     int16_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_int16_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_int16_value;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.int16_value = value;
    
    return (modern *) result;
}


HELPER void helper_int16_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_int32_make
    (modern_library *library_in,
     int32_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_int32_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_int32_value;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.int32_value = value;
    
    return (modern *) result;
}


HELPER void helper_int32_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_int64_make
    (modern_library *library_in,
     int64_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_int64_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_int64_value;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.int64_value = value;
    
    return (modern *) result;
}


HELPER void helper_int64_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_nat8_make
    (modern_library *library_in,
     uint8_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_nat8_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_nat8_value;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.nat8_value = value;
    
    return (modern *) result;
}


HELPER void helper_nat8_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_nat16_make
    (modern_library *library_in,
     uint16_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_nat16_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_nat16_value;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.nat16_value = value;
    
    return (modern *) result;
}


HELPER void helper_nat16_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_nat32_make
    (modern_library *library_in,
     uint32_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_nat32_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_nat32_value;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.nat32_value = value;
    
    return (modern *) result;
}


HELPER void helper_nat32_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_nat64_make
    (modern_library *library_in,
     uint64_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_nat64_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_nat64_value;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.nat64_value = value;
    
    return (modern *) result;
}


HELPER void helper_nat64_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_float32_make
    (modern_library *library_in,
     float value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    switch(fpclassify(value)) {
    case FP_NORMAL:
        break;
    case FP_ZERO:
        value = 0.0;
        break;
    case FP_SUBNORMAL:
    {
        float mantissa;
        int exponent;
        mantissa = frexpf(value, &exponent);
        value = scalbnf(mantissa, exponent);
        break;
    }
    default:
        library->error_handler->non_numeric_float
            (library->client_state);
        return NULL;
    }
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_float32_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_float32_value;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.float32_value = value;
    
    return (modern *) result;
}


HELPER void helper_float32_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_float64_make
    (modern_library *library_in,
     double value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    switch(fpclassify(value)) {
    case FP_NORMAL:
        break;
    case FP_ZERO:
        value = 0.0;
        break;
    case FP_SUBNORMAL:
    {
        double mantissa;
        int exponent;
        mantissa = frexp(value, &exponent);
        value = scalbn(mantissa, exponent);
        break;
    }
    default:
        library->error_handler->non_numeric_float
            (library->client_state);
        return NULL;
    }
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_float64_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_float64_value;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.float64_value = value;
    
    return (modern *) result;
}


HELPER void helper_float64_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_utf8_make
    (modern_library *library_in,
     uint8_t *data)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    size_t bytes = strlen((char *) data) + 1;
    uint8_t *copied_data =
        library->allocator->alloc
            (library->client_state, bytes);
    if(!copied_data) {
        library->allocator->free
            (library->client_state, result);
        library->error_handler->memory
            (library->client_state, bytes);
        return NULL;
    }
    memcpy(copied_data, data, bytes);
    
    result->memory.finalizer = helper_utf8_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_utf8_value;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.utf8_value.bytes = bytes;
    result->specifics.utf8_value.data = copied_data;
    
    return (modern *) result;
}


HELPER void helper_utf8_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    library->allocator->free
        (library->client_state, node->specifics.utf8_value.data);
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_blob_make
    (modern_library *library_in,
     uint8_t *data, size_t bytes)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    uint8_t *copied_data =
        library->allocator->alloc
            (library->client_state, bytes);
    if(!copied_data) {
        library->allocator->free
            (library->client_state, result);
        library->error_handler->memory
            (library->client_state, bytes);
        return NULL;
    }
    memcpy(copied_data, data, bytes);
    
    result->memory.finalizer = helper_blob_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_blob_value;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.blob_value.bytes = bytes;
    result->specifics.blob_value.data = copied_data;
    
    return (modern *) result;
}


HELPER void helper_blob_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    library->allocator->free
        (library->client_state, node->specifics.blob_value.data);
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_sigma_make
    (modern_library *library_in,
     modern *type_in,
     modern *field_value_in,
     modern *successor_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *type = (struct modern *) type_in;
    struct modern *field_value = (struct modern *) field_value_in;
    struct modern *successor = (struct modern *) successor_in;
    
    // TODO
    // type must be a sigma type
    // field value must be a value of type's field type
    // successor must be a value of the appropriate type
    // any of these conditions failing to hold should be reported as
    // a type mismatch (three different ones, though)
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_sigma_value_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_sigma_value;
    result->canonical_hash_valid = 0;
    result->value_type = type;
    result->specifics.sigma_value.field_value = field_value;
    result->specifics.sigma_value.successor = successor;
    
    return (modern *) result;
}


HELPER void helper_sigma_value_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_finalize(library, node->specifics.sigma_value.field_value);
    modern_finalize(library, node->specifics.sigma_value.successor);
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_name_value_make
    (modern_library *library_in,
     struct modern_hash name)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_name_value_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_name_value;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.name_value.hash = name;
    
    return (modern *) result;
}


HELPER void helper_name_value_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_named_value_make
    (modern_library *library_in,
     modern *type_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *type = (struct modern *) type_in;
    struct modern *value = (struct modern *) value_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_named_value_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_named_value;
    result->canonical_hash_valid = 0;
    result->value_type = type;
    result->specifics.named_value.value = value;
    
    return (modern *) result;
}


HELPER void helper_named_value_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_finalize(library, node->specifics.named_value.value);
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_bool_type_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_bool_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_bool_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_bool_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_ordering_type_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_ordering_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_ordering_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_ordering_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_maybe_type_make
    (modern_library *library_in,
     modern *content_type)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    if(!content_type) {
        library->error_handler->usage
            (library->client_state);
    }
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_maybe_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_maybe_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.maybe_type.content_type = content_type;
    
    return (modern *) result;
}


HELPER void helper_maybe_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
    
    modern_finalize(library, node->specifics.maybe_type.content_type);
}


INTERNAL modern *default_int8_type_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_int8_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_int8_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_int8_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_int16_type_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_int16_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_int16_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_int16_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_int32_type_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_int32_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_int32_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_int32_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_int64_type_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_int64_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_int64_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_int64_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_nat8_type_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_nat8_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_nat8_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_nat8_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_nat16_type_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_nat16_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_nat16_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_nat16_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_nat32_type_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_nat32_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_nat32_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_nat32_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_nat64_type_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_nat64_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_nat64_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_nat64_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_float32_type_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_float32_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_float32_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_float32_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_float64_type_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_float64_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_float64_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_float64_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_utf8_type_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_utf8_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_utf8_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_utf8_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_blob_type_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_blob_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_blob_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_blob_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_function_type_make
    (modern_library *library_in,
     modern *parameter_in, modern *result_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *parameter = (struct modern *) parameter_in;
    struct modern *result = (struct modern *) result_in;
    
    size_t function_type_size = sizeof(struct modern);
    struct modern *function_type =
        library->allocator->alloc
            (library->client_state, function_type_size);
    if(!function_type) {
        library->error_handler->memory
            (library->client_state, function_type_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_function_type_finalize;
    
    function_type->mutable = 1;
    function_type->node_type = modern_node_type_function_type;
    function_type->canonical_hash_valid = 0;
    function_type->value_type = NULL;
    function_type->specifics.function_type.parameter = parameter;
    function_type->specifics.function_type.result = result;
    
    return (modern *) function_type;
}



HELPER void helper_function_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_finalize(library, node->specifics.function_type.parameter);
    modern_finalize(library, node->specifics.function_type.result);
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_sigma_type_make
    (modern_library *library_in,
     modern *field_type, modern *successor)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_sigma_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_sigma_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.sigma_type.field_type = field_type;
    result->specifics.sigma_type.successor = successor;
    
    return (modern *) result;
}



HELPER void helper_sigma_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_finalize(library, node->specifics.sigma_type.field_type);
    modern_finalize(library, node->specifics.sigma_type.successor);
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_name_type_make
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_name_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_name_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_name_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_named_type_make
    (modern_library *library_in,
     struct modern_hash name, modern *content_type)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_named_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_named_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.named_type.name = name;
    result->specifics.named_type.content_type = content_type;
    
    return (modern *) result;
}


HELPER void helper_named_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_finalize(library, node->specifics.named_type.content_type);
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_universe_type_make
    (modern_library *library_in,
     uint64_t level)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_universe_type_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_universe_type;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.universe_type.level = level;
    
    return (modern *) result;
}


HELPER void helper_universe_type_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_lambda_make
    (modern_library *library_in,
     modern *content_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *content = (struct modern *) content_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_lambda_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_lambda;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.lambda.content = content;
    
    return (modern *) result;
}


HELPER void helper_lambda_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_finalize(library, node->specifics.lambda.content);
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_apply_make
    (modern_library *library_in,
     modern *function_in, modern *parameter_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *function = (struct modern *) function_in;
    struct modern *parameter = (struct modern *) parameter_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_apply_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_apply;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.apply.function = function;
    result->specifics.apply.parameter = parameter;
    
    return (modern *) result;
}


HELPER void helper_apply_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_finalize(library, node->specifics.apply.function);
    modern_finalize(library, node->specifics.apply.parameter);
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_type_family_make
    (modern_library *library_in,
     uint64_t n_items, modern **types_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    size_t bytes = sizeof(modern **) * n_items;
    modern **copied_data = library->allocator->alloc
        (library->client_state, bytes);
    if(!copied_data) {
        library->allocator->free
            (library->client_state, result);
        library->error_handler->memory
            (library->client_state, bytes);
        return NULL;
    }
    memcpy(copied_data, types_in, bytes);
    
    result->memory.finalizer = helper_type_family_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_type_family;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.type_family.n_items = n_items;
    result->specifics.type_family.members = (struct modern **) copied_data;
    
    return (modern *) result;
}


HELPER void helper_type_family_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    for(size_t i = 0; i < node->specifics.type_family.n_items; i++) {
        modern_finalize(library, node->specifics.type_family.members[i]);
    }
    
    library->allocator->free
        (library->client_state, node->specifics.type_family.members);
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_let_make
    (modern_library *library_in,
     uint64_t n_items, modern **values_in, modern *content_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    size_t bytes = sizeof(modern **) * n_items;
    modern **copied_data = library->allocator->alloc
        (library->client_state, bytes);
    if(!copied_data) {
        library->allocator->free
            (library->client_state, result);
        library->error_handler->memory
            (library->client_state, bytes);
        return NULL;
    }
    memcpy(copied_data, values_in, bytes);
    
    result->memory.finalizer = helper_let_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_type_family;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.let.n_items = n_items;
    result->specifics.let.members = (struct modern **) copied_data;
    result->specifics.let.content = (struct modern *) content_in;
    
    return (modern *) result;
}


HELPER void helper_let_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    for(size_t i = 0; i < node->specifics.let.n_items; i++) {
        modern_finalize(library, node->specifics.let.members[i]);
    }
    
    modern_finalize(library, node->specifics.let.content);
    
    library->allocator->free
        (library->client_state, node->specifics.type_family.members);
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_backreference_make
    (modern_library *library_in,
     uint64_t index)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_backreference_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_backreference;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.backreference.index = index;
    
    return (modern *) result;
}


HELPER void helper_backreference_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}


INTERNAL modern *default_builtin_make
    (modern_library *library_in,
     uint16_t identifier)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.finalizer = helper_builtin_finalize;
    
    result->mutable = 1;
    result->node_type = modern_node_type_backreference;
    result->canonical_hash_valid = 0;
    result->value_type = NULL;
    result->specifics.builtin = identifier;
    
    return (modern *) result;
}


HELPER void helper_builtin_finalize
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_finalize(library, node->value_type);
}

