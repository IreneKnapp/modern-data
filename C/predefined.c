#include <math.h>
#include <string.h>
#include "modern.h"
#include "internal.h"


HELPER void helper_finalize_int8
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_int16
    (modern_library *library_in,
     int16_t value);
HELPER void helper_finalize_int16
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_int32
    (modern_library *library_in,
     int32_t value);
HELPER void helper_finalize_int32
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_int64
    (modern_library *library_in,
     int64_t value);
HELPER void helper_finalize_int64
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_nat8
    (modern_library *library_in,
     uint8_t value);
HELPER void helper_finalize_nat8
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_nat16
    (modern_library *library_in,
     uint16_t value);
HELPER void helper_finalize_nat16
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_nat32
    (modern_library *library_in,
     uint32_t value);
HELPER void helper_finalize_nat32
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_nat64
    (modern_library *library_in,
     uint64_t value);
HELPER void helper_finalize_nat64
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_float32
    (modern_library *library_in,
     float value);
HELPER void helper_finalize_float32
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_float64
    (modern_library *library_in,
     double value);
HELPER void helper_finalize_float64
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_utf8
    (modern_library *library_in,
     uint8_t *data);
HELPER void helper_finalize_utf8
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_blob
    (modern_library *library_in,
     uint8_t *data, size_t bytes);
HELPER void helper_finalize_blob
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_sigma
    (modern_library *library_in,
     modern *type_in,
     modern *field_value_in,
     modern *successor_in);
HELPER void helper_finalize_sigma_value
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_named_value
    (modern_library *library_in,
     modern *type_in, modern *value_in);
HELPER void helper_finalize_named_value
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_bool_type
    (modern_library *library_in);
HELPER void helper_finalize_bool_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_ordering_type
    (modern_library *library_in);
HELPER void helper_finalize_ordering_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_maybe_type
    (modern_library *library_in,
     modern *content_type);
HELPER void helper_finalize_maybe_type
  (struct modern_library *library, void *retainable);
HELPER void helper_finalize_maybe_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_int8_type
    (modern_library *library_in);
HELPER void helper_finalize_int8_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_int16_type
    (modern_library *library_in);
HELPER void helper_finalize_int16_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_int32_type
    (modern_library *library_in);
HELPER void helper_finalize_int32_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_int64_type
    (modern_library *library_in);
HELPER void helper_finalize_int64_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_nat8_type
    (modern_library *library_in);
HELPER void helper_finalize_nat8_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_nat16_type
    (modern_library *library_in);
HELPER void helper_finalize_nat16_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_nat32_type
    (modern_library *library_in);
HELPER void helper_finalize_nat32_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_nat64_type
    (modern_library *library_in);
HELPER void helper_finalize_nat64_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_float32_type
    (modern_library *library_in);
HELPER void helper_finalize_float32_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_float64_type
    (modern_library *library_in);
HELPER void helper_finalize_float64_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_utf8_type
    (modern_library *library_in);
HELPER void helper_finalize_utf8_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_blob_type
    (modern_library *library_in);
HELPER void helper_finalize_blob_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_function_type
    (modern_library *library_in,
     modern *left_in, modern *right_in);
HELPER void helper_finalize_function_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_sigma_type
    (modern_library *library_in,
     modern *field_type, modern *successor);
HELPER void helper_finalize_sigma_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_name_type
    (modern_library *library_in);
HELPER void helper_finalize_name_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_named_type
    (modern_library *library_in,
     struct modern_hash name, modern *content_type);
HELPER void helper_finalize_named_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_universe_type
    (modern_library *library_in,
     uint64_t level);
HELPER void helper_finalize_universe_type
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_lambda
    (modern_library *library_in,
     modern *content_in);
HELPER void helper_finalize_lambda
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_apply
    (modern_library *library_in,
     modern *left_in, modern *right_in);
HELPER void helper_finalize_apply
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_type_family
    (modern_library *library_in,
     uint64_t n_items, modern **types_in);
HELPER void helper_finalize_type_family
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_let
    (modern_library *library_in,
     uint64_t n_items, modern **values_in, modern *content_in);
HELPER void helper_finalize_let
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_backreference
    (modern_library *library_in,
     uint64_t index);
HELPER void helper_finalize_backreference
  (struct modern_library *library, void *retainable);
HELPER modern *modern_node_make_builtin
    (modern_library *library_in,
     uint16_t identifier);
HELPER void helper_finalize_builtin
  (struct modern_library *library, void *retainable);


struct modern_node *modern_node_make
  (struct modern_allocator *allocator,
   void *client_state)
{
    struct modern_node *result =
        allocator->modern_allocator_alloc
            (client_state, sizeof(struct modern_node));
    if(!result) return NULL;
    
    result->modern_node_node_type_get =
        modern_node_node_type_get;
    result->modern_node_value_type_get =
        modern_node_value_type_get;
    result->modern_node_mutable_get =
        modern_node_mutable_get;
    result->modern_node_canonical_hash_get =
        modern_node_canonical_hash_get;
    result->modern_node_maybe_just_content_get =
        modern_node_maybe_just_content_get;
    result->modern_node_int8_get =
        modern_node_int8_get;
    result->modern_node_int16_get =
        modern_node_int16_get;
    result->modern_node_int32_get =
        modern_node_int32_get;
    result->modern_node_int64_get =
        modern_node_int64_get;
    result->modern_node_nat8_get =
        modern_node_nat8_get;
    result->modern_node_nat16_get =
        modern_node_nat16_get;
    result->modern_node_nat32_get =
        modern_node_nat32_get;
    result->modern_node_nat64_get =
        modern_node_nat64_get;
    result->modern_node_float32_get =
        modern_node_float32_get;
    result->modern_node_float64_get =
        modern_node_float64_get;
    result->modern_node_utf8_bytes_get =
        modern_node_utf8_bytes_get;
    result->modern_node_utf8_data_piece_get =
        modern_node_utf8_data_piece_get;
    result->modern_node_blob_bytes_get =
        modern_node_blob_bytes_get;
    result->modern_node_blob_data_piece_get =
        modern_node_blob_data_piece_get;
    result->modern_node_sigma_field_value_get =
        modern_node_sigma_field_value_get;
    result->modern_node_sigma_successor_get =
        modern_node_sigma_successor_get;
    result->modern_node_get_name_hash =
        modern_node_get_name_hash;
    result->modern_node_named_value_get =
        modern_node_named_value_get;
    result->modern_node_maybe_type_content_type_get =
        modern_node_maybe_type_content_type_get;
    result->modern_node_function_type_left_get =
        modern_node_function_type_left_get;
    result->modern_node_function_type_right_get =
        modern_node_function_type_right_get;
    result->modern_node_sigma_type_field_type_get =
        modern_node_sigma_type_field_type_get;
    result->modern_node_sigma_type_successor_get =
        modern_node_sigma_type_successor_get;
    result->modern_node_named_type_name_get =
        modern_node_named_type_name_get;
    result->modern_node_named_type_content_type_get =
        modern_node_named_type_content_type_get;
    result->modern_node_universe_type_level_get =
        modern_node_universe_type_level_get;
    result->modern_node_lambda_content_get =
        modern_node_lambda_content_get;
    result->modern_node_apply_left_get =
        modern_node_apply_left_get;
    result->modern_node_apply_right_get =
        modern_node_apply_right_get;
    result->modern_node_type_family_count_get =
        modern_node_type_family_count_get;
    result->modern_node_type_family_item_get =
        modern_node_type_family_item_get;
    result->modern_node_let_count_get =
        modern_node_let_count_get;
    result->modern_node_let_item_get =
        modern_node_let_item_get;
    result->modern_node_let_content_get =
        modern_node_let_content_get;
    result->modern_node_backreference_index_get =
        modern_node_backreference_index_get;
    result->modern_node_builtin_identifier_get =
        modern_node_builtin_identifier_get;
    result->modern_node_bool_false_make =
        modern_node_bool_false_make;
    result->modern_node_bool_true_make =
        modern_node_bool_true_make;
    result->modern_node_ordering_less_make =
        modern_node_ordering_less_make;
    result->modern_node_ordering_equal_make =
        modern_node_ordering_equal_make;
    result->modern_node_ordering_greater_make =
        modern_node_ordering_greater_make;
    result->modern_node_maybe_nothing_make =
        modern_node_maybe_nothing_make;
    result->modern_node_maybe_just_make =
        modern_node_maybe_just_make;
    result->modern_node_int8_make =
        modern_node_int8_make;
    result->modern_node_int16_make =
        modern_node_int16_make;
    result->modern_node_int32_make =
        modern_node_int32_make;
    result->modern_node_int64_make =
        modern_node_int64_make;
    result->modern_node_nat8_make =
        modern_node_nat8_make;
    result->modern_node_nat16_make =
        modern_node_nat16_make;
    result->modern_node_nat32_make =
        modern_node_nat32_make;
    result->modern_node_nat64_make =
        modern_node_nat64_make;
    result->modern_node_float32_make =
        modern_node_float32_make;
    result->modern_node_float64_make =
        modern_node_float64_make;
    result->modern_node_utf8_make =
        modern_node_utf8_make;
    result->modern_node_blob_make =
        modern_node_blob_make;
    result->modern_node_sigma_make =
        modern_node_sigma_make;
    result->modern_node_name_make =
        modern_node_name_make;
    result->modern_node_named_value_make =
        modern_node_named_value_make;
    result->modern_node_bool_type_make =
        modern_node_bool_type_make;
    result->modern_node_ordering_type_make =
        modern_node_ordering_type_make;
    result->modern_node_maybe_type_make =
        modern_node_maybe_type_make;
    result->modern_node_int8_type_make =
        modern_node_int8_type_make;
    result->modern_node_int16_type_make =
        modern_node_int16_type_make;
    result->modern_node_int32_type_make =
        modern_node_int32_type_make;
    result->modern_node_int64_type_make =
        modern_node_int64_type_make;
    result->modern_node_nat8_type_make =
        modern_node_nat8_type_make;
    result->modern_node_nat16_type_make =
        modern_node_nat16_type_make;
    result->modern_node_nat32_type_make =
        modern_node_nat32_type_make;
    result->modern_node_nat64_type_make =
        modern_node_nat64_type_make;
    result->modern_node_float32_type_make =
        modern_node_float32_type_make;
    result->modern_node_float64_type_make =
        modern_node_float64_type_make;
    result->modern_node_utf8_type_make =
        modern_node_utf8_type_make;
    result->modern_node_blob_type_make =
        modern_node_blob_type_make;
    result->modern_node_function_type_make =
        modern_node_function_type_make;
    result->modern_node_sigma_type_make =
        modern_node_sigma_type_make;
    result->modern_node_name_type_make =
        modern_node_name_type_make;
    result->modern_node_named_type_make =
        modern_node_named_type_make;
    result->modern_node_universe_type_make =
        modern_node_universe_type_make;
    result->modern_node_lambda_make =
        modern_node_lambda_make;
    result->modern_node_apply_make =
        modern_node_apply_make;
    result->modern_node_type_family_make =
        modern_node_type_family_make;
    result->modern_node_let_make =
        modern_node_let_make;
    result->modern_node_backreference_make =
        modern_node_backreference_make;
    result->modern_node_builtin_make =
        modern_node_builtin_make;
    result->modern_node_immutable_set =
        modern_node_immutable_set;
    result->modern_node_canonical_hash_set =
        modern_node_canonical_hash_set;
    result->modern_node_maybe_just_content_set =
        modern_node_maybe_just_content_set;
    result->modern_node_int8_set =
        modern_node_int8_set;
    result->modern_node_int16_set =
        modern_node_int16_set;
    result->modern_node_int32_set =
        modern_node_int32_set;
    result->modern_node_int64_set =
        modern_node_int64_set;
    result->modern_node_nat8_set =
        modern_node_nat8_set;
    result->modern_node_nat16_set =
        modern_node_nat16_set;
    result->modern_node_nat32_set =
        modern_node_nat32_set;
    result->modern_node_nat64_set =
        modern_node_nat64_set;
    result->modern_node_float32_set =
        modern_node_float32_set;
    result->modern_node_float64_set =
        modern_node_float64_set;
    result->modern_node_utf8_data_piece_set =
        modern_node_utf8_data_piece_set;
    result->modern_node_blob_data_piece_set =
        modern_node_blob_data_piece_set;
    result->modern_node_sigma_set =
        modern_node_sigma_set;
    result->modern_node_named_value_set =
        modern_node_named_value_set;
    result->modern_node_maybe_type_content_type_set =
        modern_node_maybe_type_content_type_set;
    result->modern_node_function_type_left_set =
        modern_node_function_type_left_set;
    result->modern_node_function_type_right_set =
        modern_node_function_type_right_set;
    result->modern_node_sigma_type_field_type_set =
        modern_node_sigma_type_field_type_set;
    result->modern_node_sigma_type_successor_set =
        modern_node_sigma_type_successor_set;
    result->modern_node_named_type_name_set =
        modern_node_named_type_name_set;
    result->modern_node_named_type_content_type_set =
        modern_node_named_type_content_type_set;
    result->modern_node_universe_type_level_set =
        modern_node_universe_type_level_set;
    result->modern_node_lambda_content_set =
        modern_node_lambda_content_set;
    result->modern_node_apply_left_set =
        modern_node_apply_left_set;
    result->modern_node_apply_right_set =
        modern_node_apply_right_set;
    result->modern_node_set_type_family_item_add =
        modern_node_set_type_family_item_add;
    result->modern_node_set_type_family_item_remove =
        modern_node_set_type_family_item_remove;
    result->modern_node_set_let_item_add =
        modern_node_set_let_item_add;
    result->modern_node_set_let_item_remove =
        modern_node_set_let_item_remove;
    result->modern_node_let_content_set =
        modern_node_let_content_set;
    result->modern_node_backreference_index_set =
        modern_node_backreference_index_set;
    result->modern_node_builtin_identifier_set =
        modern_node_builtin_identifier_set;
    
    return result;
}


HELPER void helper_finalize_int8
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_int16
    (modern_library *library_in,
     int16_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_int16;
    
    result->node_type = modern_node_type_int16_value;
    result->value_type = NULL;
    result->specifics.int16_value = value;
    
    return (modern *) result;
}


HELPER void helper_finalize_int16
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_int32
    (modern_library *library_in,
     int32_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_int32;
    
    result->node_type = modern_node_type_int32_value;
    result->value_type = NULL;
    result->specifics.int32_value = value;
    
    return (modern *) result;
}


HELPER void helper_finalize_int32
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_int64
    (modern_library *library_in,
     int64_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_int64;
    
    result->node_type = modern_node_type_int64_value;
    result->value_type = NULL;
    result->specifics.int64_value = value;
    
    return (modern *) result;
}


HELPER void helper_finalize_int64
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_nat8
    (modern_library *library_in,
     uint8_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_nat8;
    
    result->node_type = modern_node_type_nat8_value;
    result->value_type = NULL;
    result->specifics.nat8_value = value;
    
    return (modern *) result;
}


HELPER void helper_finalize_nat8
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_nat16
    (modern_library *library_in,
     uint16_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_nat16;
    
    result->node_type = modern_node_type_nat16_value;
    result->value_type = NULL;
    result->specifics.nat16_value = value;
    
    return (modern *) result;
}


HELPER void helper_finalize_nat16
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_nat32
    (modern_library *library_in,
     uint32_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_nat32;
    
    result->node_type = modern_node_type_nat32_value;
    result->value_type = NULL;
    result->specifics.nat32_value = value;
    
    return (modern *) result;
}


HELPER void helper_finalize_nat32
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_nat64
    (modern_library *library_in,
     uint64_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_nat64;
    
    result->node_type = modern_node_type_nat64_value;
    result->value_type = NULL;
    result->specifics.nat64_value = value;
    
    return (modern *) result;
}


HELPER void helper_finalize_nat64
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_float32
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
        library->error_handler->modern_error_handler_non_numeric_float
            (library->client_state);
        return NULL;
    }
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_float32;
    
    result->node_type = modern_node_type_float32_value;
    result->value_type = NULL;
    result->specifics.float32_value = value;
    
    return (modern *) result;
}


HELPER void helper_finalize_float32
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_float64
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
        library->error_handler->modern_error_handler_non_numeric_float
            (library->client_state);
        return NULL;
    }
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_float64;
    
    result->node_type = modern_node_type_float64_value;
    result->value_type = NULL;
    result->specifics.float64_value = value;
    
    return (modern *) result;
}


HELPER void helper_finalize_float64
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_utf8
    (modern_library *library_in,
     uint8_t *data)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    size_t bytes = strlen((char *) data) + 1;
    uint8_t *copied_data =
        library->allocator->modern_allocator_alloc
            (library->client_state, bytes);
    if(!copied_data) {
        library->allocator->modern_allocator_free
            (library->client_state, result);
        library->error_handler->modern_error_handler_memory
            (library->client_state, bytes);
        return NULL;
    }
    memcpy(copied_data, data, bytes);
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_utf8;
    
    result->node_type = modern_node_type_utf8_value;
    result->value_type = NULL;
    result->specifics.utf8_value.bytes = bytes;
    result->specifics.utf8_value.data = copied_data;
    
    return (modern *) result;
}


HELPER void helper_finalize_utf8
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    library->allocator->modern_allocator_free
        (library->client_state, node->specifics.utf8_value.data);
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_blob
    (modern_library *library_in,
     uint8_t *data, size_t bytes)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    uint8_t *copied_data =
        library->allocator->modern_allocator_alloc
            (library->client_state, bytes);
    if(!copied_data) {
        library->allocator->modern_allocator_free
            (library->client_state, result);
        library->error_handler->modern_error_handler_memory
            (library->client_state, bytes);
        return NULL;
    }
    memcpy(copied_data, data, bytes);
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_blob;
    
    result->node_type = modern_node_type_blob_value;
    result->value_type = NULL;
    result->specifics.blob_value.bytes = bytes;
    result->specifics.blob_value.data = copied_data;
    
    return (modern *) result;
}


HELPER void helper_finalize_blob
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    library->allocator->modern_allocator_free
        (library->client_state, node->specifics.blob_value.data);
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_sigma
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
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_sigma_value;
    
    result->node_type = modern_node_type_sigma_value;
    result->value_type = type;
    result->specifics.sigma_value.field_value = field_value;
    result->specifics.sigma_value.successor = successor;
    
    modern_retain(library_in, field_value_in);
    modern_retain(library_in, successor_in);
    
    return (modern *) result;
}


HELPER void helper_finalize_sigma_value
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_release(library, node->specifics.sigma_value.field_value);
    modern_release(library, node->specifics.sigma_value.successor);
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_named_value
    (modern_library *library_in,
     modern *type_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *type = (struct modern *) type_in;
    struct modern *value = (struct modern *) value_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_named_value;
    
    result->node_type = modern_node_type_named_value;
    result->value_type = type;
    result->specifics.named_value.value = value;
    
    modern_retain(library_in, type_in);
    modern_retain(library_in, value_in);
    
    return (modern *) result;
}


HELPER void helper_finalize_named_value
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_release(library, node->specifics.named_value.value);
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_bool_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_bool_type;
    
    result->node_type = modern_node_type_bool_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_finalize_bool_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_ordering_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_ordering_type;
    
    result->node_type = modern_node_type_ordering_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_finalize_ordering_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_maybe_type
    (modern_library *library_in,
     modern *content_type)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    if(!content_type) {
        library->error_handler->modern_error_handler_usage
            (library->client_state);
    }
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    modern_retain(library, content_type);
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_maybe_type;
    
    result->node_type = modern_node_type_maybe_type;
    result->value_type = NULL;
    result->specifics.maybe_type.content_type = content_type;
    
    return (modern *) result;
}


HELPER void helper_finalize_maybe_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
    
    modern_release(library, node->specifics.maybe_type.content_type);
}


HELPER modern *modern_node_make_int8_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_int8_type;
    
    result->node_type = modern_node_type_int8_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_finalize_int8_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_int16_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_int16_type;
    
    result->node_type = modern_node_type_int16_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_finalize_int16_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_int32_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_int32_type;
    
    result->node_type = modern_node_type_int32_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_finalize_int32_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_int64_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_int64_type;
    
    result->node_type = modern_node_type_int64_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_finalize_int64_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_nat8_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_nat8_type;
    
    result->node_type = modern_node_type_nat8_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_finalize_nat8_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_nat16_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_nat16_type;
    
    result->node_type = modern_node_type_nat16_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_finalize_nat16_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_nat32_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_nat32_type;
    
    result->node_type = modern_node_type_nat32_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_finalize_nat32_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_nat64_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_nat64_type;
    
    result->node_type = modern_node_type_nat64_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_finalize_nat64_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_float32_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_float32_type;
    
    result->node_type = modern_node_type_float32_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_finalize_float32_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_float64_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_float64_type;
    
    result->node_type = modern_node_type_float64_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_finalize_float64_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_utf8_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_utf8_type;
    
    result->node_type = modern_node_type_utf8_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_finalize_utf8_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_blob_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_blob_type;
    
    result->node_type = modern_node_type_blob_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_finalize_blob_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_function_type
    (modern_library *library_in,
     modern *left_in, modern *right_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *left = (struct modern *) left_in;
    struct modern *right = (struct modern *) right_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_function_type;
    
    result->node_type = modern_node_type_function_type;
    result->value_type = NULL;
    result->specifics.function_type.left = left;
    result->specifics.function_type.right = right;
    
    modern_retain(library, left_in);
    modern_retain(library, right_in);
    
    return (modern *) result;
}



HELPER void helper_finalize_function_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_release(library, node->specifics.function_type.left);
    modern_release(library, node->specifics.function_type.right);
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_sigma_type
    (modern_library *library_in,
     modern *field_type, modern *successor)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_sigma_type;
    
    result->node_type = modern_node_type_sigma_type;
    result->value_type = NULL;
    result->specifics.sigma_type.field_type = field_type;
    result->specifics.sigma_type.successor = successor;
    
    modern_retain(library, field_type);
    modern_retain(library, successor);
    
    return (modern *) result;
}



HELPER void helper_finalize_sigma_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_release(library, node->specifics.sigma_type.field_type);
    modern_release(library, node->specifics.sigma_type.successor);
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_name_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_name_type;
    
    result->node_type = modern_node_type_name_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


HELPER void helper_finalize_name_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_named_type
    (modern_library *library_in,
     struct modern_hash name, modern *content_type)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_named_type;
    
    result->node_type = modern_node_type_named_type;
    result->value_type = NULL;
    result->specifics.named_type.name = name;
    result->specifics.named_type.content_type = content_type;
    
    return (modern *) result;
}


HELPER void helper_finalize_named_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_release(library, node->specifics.named_type.content_type);
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_universe_type
    (modern_library *library_in,
     uint64_t level)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_universe_type;
    
    result->node_type = modern_node_type_universe_type;
    result->value_type = NULL;
    result->specifics.universe_type.level = level;
    
    return (modern *) result;
}


HELPER void helper_finalize_universe_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_lambda
    (modern_library *library_in,
     modern *content_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *content = (struct modern *) content_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_lambda;
    
    result->node_type = modern_node_type_lambda;
    result->value_type = NULL;
    result->specifics.lambda.content = content;
    
    return (modern *) result;
}


HELPER void helper_finalize_lambda
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_release(library, node->specifics.lambda.content);
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_apply
    (modern_library *library_in,
     modern *left_in, modern *right_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *left = (struct modern *) left_in;
    struct modern *right = (struct modern *) right_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_apply;
    
    result->node_type = modern_node_type_apply;
    result->value_type = NULL;
    result->specifics.apply.left = left;
    result->specifics.apply.right = right;
    
    return (modern *) result;
}


HELPER void helper_finalize_apply
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_release(library, node->specifics.apply.left);
    modern_release(library, node->specifics.apply.right);
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_type_family
    (modern_library *library_in,
     uint64_t n_items, modern **types_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    size_t bytes = sizeof(modern **) * n_items;
    modern **copied_data = library->allocator->modern_allocator_alloc
        (library->client_state, bytes);
    if(!copied_data) {
        library->allocator->modern_allocator_free
            (library->client_state, result);
        library->error_handler->modern_error_handler_memory
            (library->client_state, bytes);
        return NULL;
    }
    memcpy(copied_data, types_in, bytes);
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_type_family;
    
    result->node_type = modern_node_type_type_family;
    result->value_type = NULL;
    result->specifics.type_family.n_items = n_items;
    result->specifics.type_family.members = (struct modern **) copied_data;
    
    for(size_t i = 0; i < n_items; i++) {
        modern_retain(library, types_in[i]);
    }
    
    return (modern *) result;
}


HELPER void helper_finalize_type_family
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    for(size_t i = 0; i < node->specifics.type_family.n_items; i++) {
        modern_release(library, node->specifics.type_family.members[i]);
    }
    
    library->allocator->modern_allocator_free
        (library->client_state, node->specifics.type_family.members);
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_let
    (modern_library *library_in,
     uint64_t n_items, modern **values_in, modern *content_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    size_t bytes = sizeof(modern **) * n_items;
    modern **copied_data = library->allocator->modern_allocator_alloc
        (library->client_state, bytes);
    if(!copied_data) {
        library->allocator->modern_allocator_free
            (library->client_state, result);
        library->error_handler->modern_error_handler_memory
            (library->client_state, bytes);
        return NULL;
    }
    memcpy(copied_data, values_in, bytes);
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_let;
    
    result->node_type = modern_node_type_type_family;
    result->value_type = NULL;
    result->specifics.let.n_items = n_items;
    result->specifics.let.members = (struct modern **) copied_data;
    result->specifics.let.content = (struct modern *) content_in;
    
    for(size_t i = 0; i < n_items; i++) {
        modern_retain(library, values_in[i]);
    }
    
    modern_retain(library, content_in);
    
    return (modern *) result;
}


HELPER void helper_finalize_let
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    for(size_t i = 0; i < node->specifics.let.n_items; i++) {
        modern_release(library, node->specifics.let.members[i]);
    }
    
    modern_release(library, node->specifics.let.content);
    
    library->allocator->modern_allocator_free
        (library->client_state, node->specifics.type_family.members);
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_backreference
    (modern_library *library_in,
     uint64_t index)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_backreference;
    
    result->node_type = modern_node_type_backreference;
    result->value_type = NULL;
    result->specifics.backreference.index = index;
    
    return (modern *) result;
}


HELPER void helper_finalize_backreference
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}


HELPER modern *modern_node_make_builtin
    (modern_library *library_in,
     uint16_t identifier)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_builtin;
    
    result->node_type = modern_node_type_backreference;
    result->value_type = NULL;
    result->specifics.builtin = identifier;
    
    return (modern *) result;
}


HELPER void helper_finalize_builtin
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    if(node->value_type) modern_release(library, node->value_type);
}
