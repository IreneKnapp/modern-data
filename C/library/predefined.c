#include <math.h>
#include <string.h>
#include "modern.h"
#include "internal.h"


PUBLIC struct modern_node_representation *
    modern_node_representation_default_make
    (struct modern_allocator *allocator,
     void *client_state)
{
    struct modern_node_representation *result =
        allocator->alloc
            (client_state, sizeof(struct modern_node_representation));
    if(!result) return NULL;
    
    result->node_type_get = default_node_type_get;
    result->value_type_get = default_value_type_get;
    result->canonical_hash_valid_get = default_canonical_hash_valid_get;
    result->canonical_hash_get = default_canonical_hash_get;
    result->maybe_just_content_get = default_maybe_just_content_get;
    result->int8_get = default_int8_get;
    result->int16_get = default_int16_get;
    result->int32_get = default_int32_get;
    result->int64_get = default_int64_get;
    result->nat8_get = default_nat8_get;
    result->nat16_get = default_nat16_get;
    result->nat32_get = default_nat32_get;
    result->nat64_get = default_nat64_get;
    result->float32_get = default_float32_get;
    result->float64_get = default_float64_get;
    result->utf8_bytes_get = default_utf8_bytes_get;
    result->utf8_data_piece_get = default_utf8_data_piece_get;
    result->blob_bytes_get = default_blob_bytes_get;
    result->blob_data_piece_get = default_blob_data_piece_get;
    result->sigma_field_value_get = default_sigma_field_value_get;
    result->sigma_successor_get = default_sigma_successor_get;
    result->name_value_hash_get = default_name_value_hash_get;
    result->named_value_get = default_named_value_get;
    result->maybe_type_content_type_get = default_maybe_type_content_type_get;
    result->function_type_parameter_get = default_function_type_parameter_get;
    result->function_type_result_get = default_function_type_result_get;
    result->sigma_type_field_type_get = default_sigma_type_field_type_get;
    result->sigma_type_successor_get = default_sigma_type_successor_get;
    result->named_type_name_get = default_named_type_name_get;
    result->named_type_content_type_get = default_named_type_content_type_get;
    result->universe_type_level_get = default_universe_type_level_get;
    result->lambda_content_get = default_lambda_content_get;
    result->apply_function_get = default_apply_function_get;
    result->apply_parameter_get = default_apply_parameter_get;
    result->type_family_count_get = default_type_family_count_get;
    result->type_family_item_get = default_type_family_item_get;
    result->let_count_get = default_let_count_get;
    result->let_item_get = default_let_item_get;
    result->let_content_get = default_let_content_get;
    result->backreference_index_get = default_backreference_index_get;
    result->builtin_identifier_get = default_builtin_identifier_get;
    result->bool_false_make = default_bool_false_make;
    result->bool_true_make = default_bool_true_make;
    result->ordering_less_make = default_ordering_less_make;
    result->ordering_equal_make = default_ordering_equal_make;
    result->ordering_greater_make = default_ordering_greater_make;
    result->maybe_nothing_make = default_maybe_nothing_make;
    result->maybe_just_make = default_maybe_just_make;
    result->int8_make = default_int8_make;
    result->int16_make = default_int16_make;
    result->int32_make = default_int32_make;
    result->int64_make = default_int64_make;
    result->nat8_make = default_nat8_make;
    result->nat16_make = default_nat16_make;
    result->nat32_make = default_nat32_make;
    result->nat64_make = default_nat64_make;
    result->float32_make = default_float32_make;
    result->float64_make = default_float64_make;
    result->utf8_make = default_utf8_make;
    result->blob_make = default_blob_make;
    result->sigma_make = default_sigma_make;
    result->name_value_make = default_name_value_make;
    result->named_value_make = default_named_value_make;
    result->bool_type_make = default_bool_type_make;
    result->ordering_type_make = default_ordering_type_make;
    result->maybe_type_make = default_maybe_type_make;
    result->int8_type_make = default_int8_type_make;
    result->int16_type_make = default_int16_type_make;
    result->int32_type_make = default_int32_type_make;
    result->int64_type_make = default_int64_type_make;
    result->nat8_type_make = default_nat8_type_make;
    result->nat16_type_make = default_nat16_type_make;
    result->nat32_type_make = default_nat32_type_make;
    result->nat64_type_make = default_nat64_type_make;
    result->float32_type_make = default_float32_type_make;
    result->float64_type_make = default_float64_type_make;
    result->utf8_type_make = default_utf8_type_make;
    result->blob_type_make = default_blob_type_make;
    result->function_type_make = default_function_type_make;
    result->sigma_type_make = default_sigma_type_make;
    result->name_type_make = default_name_type_make;
    result->named_type_make = default_named_type_make;
    result->universe_type_make = default_universe_type_make;
    result->lambda_make = default_lambda_make;
    result->apply_make = default_apply_make;
    result->type_family_make = default_type_family_make;
    result->let_make = default_let_make;
    result->backreference_make = default_backreference_make;
    result->builtin_make = default_builtin_make;
    result->canonical_hash_set = default_canonical_hash_set;
    result->finalize = default_finalize;
    
    return result;
}


PUBLIC void
    modern_node_representation_default_finalize
    (struct modern_allocator *allocator,
     void *client_state,
     struct modern_node_representation *node_representation)
{
    allocator->free(client_state, node_representation);
}

