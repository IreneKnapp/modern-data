#include <math.h>
#include <string.h>
#include "modern.h"
#include "internal.h"


struct modern_node *modern_node_make
  (struct modern_allocator *allocator,
   void *client_state)
{
    struct modern_node *result =
        allocator->modern_allocator_alloc
            (client_state, sizeof(struct modern_node));
    if(!result) return NULL;
    
    result->modern_node_node_type_get =
        default_modern_node_node_type_get;
    result->modern_node_value_type_get =
        default_modern_node_value_type_get;
    result->modern_node_mutable_get =
        default_modern_node_mutable_get;
    result->modern_node_canonical_hash_get =
        default_modern_node_canonical_hash_get;
    result->modern_node_maybe_just_content_get =
        default_modern_node_maybe_just_content_get;
    result->modern_node_int8_get =
        default_modern_node_int8_get;
    result->modern_node_int16_get =
        default_modern_node_int16_get;
    result->modern_node_int32_get =
        default_modern_node_int32_get;
    result->modern_node_int64_get =
        default_modern_node_int64_get;
    result->modern_node_nat8_get =
        default_modern_node_nat8_get;
    result->modern_node_nat16_get =
        default_modern_node_nat16_get;
    result->modern_node_nat32_get =
        default_modern_node_nat32_get;
    result->modern_node_nat64_get =
        default_modern_node_nat64_get;
    result->modern_node_float32_get =
        default_modern_node_float32_get;
    result->modern_node_float64_get =
        default_modern_node_float64_get;
    result->modern_node_utf8_bytes_get =
        default_modern_node_utf8_bytes_get;
    result->modern_node_utf8_data_piece_get =
        default_modern_node_utf8_data_piece_get;
    result->modern_node_blob_bytes_get =
        default_modern_node_blob_bytes_get;
    result->modern_node_blob_data_piece_get =
        default_modern_node_blob_data_piece_get;
    result->modern_node_sigma_field_value_get =
        default_modern_node_sigma_field_value_get;
    result->modern_node_sigma_successor_get =
        default_modern_node_sigma_successor_get;
    result->modern_node_get_name_hash =
        default_modern_node_get_name_hash;
    result->modern_node_named_value_get =
        default_modern_node_named_value_get;
    result->modern_node_maybe_type_content_type_get =
        default_modern_node_maybe_type_content_type_get;
    result->modern_node_function_type_left_get =
        default_modern_node_function_type_left_get;
    result->modern_node_function_type_right_get =
        default_modern_node_function_type_right_get;
    result->modern_node_sigma_type_field_type_get =
        default_modern_node_sigma_type_field_type_get;
    result->modern_node_sigma_type_successor_get =
        default_modern_node_sigma_type_successor_get;
    result->modern_node_named_type_name_get =
        default_modern_node_named_type_name_get;
    result->modern_node_named_type_content_type_get =
        default_modern_node_named_type_content_type_get;
    result->modern_node_universe_type_level_get =
        default_modern_node_universe_type_level_get;
    result->modern_node_lambda_content_get =
        default_modern_node_lambda_content_get;
    result->modern_node_apply_left_get =
        default_modern_node_apply_left_get;
    result->modern_node_apply_right_get =
        default_modern_node_apply_right_get;
    result->modern_node_type_family_count_get =
        default_modern_node_type_family_count_get;
    result->modern_node_type_family_item_get =
        default_modern_node_type_family_item_get;
    result->modern_node_let_count_get =
        default_modern_node_let_count_get;
    result->modern_node_let_item_get =
        default_modern_node_let_item_get;
    result->modern_node_let_content_get =
        default_modern_node_let_content_get;
    result->modern_node_backreference_index_get =
        default_modern_node_backreference_index_get;
    result->modern_node_builtin_identifier_get =
        default_modern_node_builtin_identifier_get;
    result->modern_node_bool_false_make =
        default_modern_node_bool_false_make;
    result->modern_node_bool_true_make =
        default_modern_node_bool_true_make;
    result->modern_node_ordering_less_make =
        default_modern_node_ordering_less_make;
    result->modern_node_ordering_equal_make =
        default_modern_node_ordering_equal_make;
    result->modern_node_ordering_greater_make =
        default_modern_node_ordering_greater_make;
    result->modern_node_maybe_nothing_make =
        default_modern_node_maybe_nothing_make;
    result->modern_node_maybe_just_make =
        default_modern_node_maybe_just_make;
    result->modern_node_int8_make =
        default_modern_node_int8_make;
    result->modern_node_int16_make =
        default_modern_node_int16_make;
    result->modern_node_int32_make =
        default_modern_node_int32_make;
    result->modern_node_int64_make =
        default_modern_node_int64_make;
    result->modern_node_nat8_make =
        default_modern_node_nat8_make;
    result->modern_node_nat16_make =
        default_modern_node_nat16_make;
    result->modern_node_nat32_make =
        default_modern_node_nat32_make;
    result->modern_node_nat64_make =
        default_modern_node_nat64_make;
    result->modern_node_float32_make =
        default_modern_node_float32_make;
    result->modern_node_float64_make =
        default_modern_node_float64_make;
    result->modern_node_utf8_make =
        default_modern_node_utf8_make;
    result->modern_node_blob_make =
        default_modern_node_blob_make;
    result->modern_node_sigma_make =
        default_modern_node_sigma_make;
    result->modern_node_name_make =
        default_modern_node_name_make;
    result->modern_node_named_value_make =
        default_modern_node_named_value_make;
    result->modern_node_bool_type_make =
        default_modern_node_bool_type_make;
    result->modern_node_ordering_type_make =
        default_modern_node_ordering_type_make;
    result->modern_node_maybe_type_make =
        default_modern_node_maybe_type_make;
    result->modern_node_int8_type_make =
        default_modern_node_int8_type_make;
    result->modern_node_int16_type_make =
        default_modern_node_int16_type_make;
    result->modern_node_int32_type_make =
        default_modern_node_int32_type_make;
    result->modern_node_int64_type_make =
        default_modern_node_int64_type_make;
    result->modern_node_nat8_type_make =
        default_modern_node_nat8_type_make;
    result->modern_node_nat16_type_make =
        default_modern_node_nat16_type_make;
    result->modern_node_nat32_type_make =
        default_modern_node_nat32_type_make;
    result->modern_node_nat64_type_make =
        default_modern_node_nat64_type_make;
    result->modern_node_float32_type_make =
        default_modern_node_float32_type_make;
    result->modern_node_float64_type_make =
        default_modern_node_float64_type_make;
    result->modern_node_utf8_type_make =
        default_modern_node_utf8_type_make;
    result->modern_node_blob_type_make =
        default_modern_node_blob_type_make;
    result->modern_node_function_type_make =
        default_modern_node_function_type_make;
    result->modern_node_sigma_type_make =
        default_modern_node_sigma_type_make;
    result->modern_node_name_type_make =
        default_modern_node_name_type_make;
    result->modern_node_named_type_make =
        default_modern_node_named_type_make;
    result->modern_node_universe_type_make =
        default_modern_node_universe_type_make;
    result->modern_node_lambda_make =
        default_modern_node_lambda_make;
    result->modern_node_apply_make =
        default_modern_node_apply_make;
    result->modern_node_type_family_make =
        default_modern_node_type_family_make;
    result->modern_node_let_make =
        default_modern_node_let_make;
    result->modern_node_backreference_make =
        default_modern_node_backreference_make;
    result->modern_node_builtin_make =
        default_modern_node_builtin_make;
    result->modern_node_immutable_set =
        default_modern_node_immutable_set;
    result->modern_node_canonical_hash_set =
        default_modern_node_canonical_hash_set;
    result->modern_node_maybe_just_content_set =
        default_modern_node_maybe_just_content_set;
    result->modern_node_int8_set =
        default_modern_node_int8_set;
    result->modern_node_int16_set =
        default_modern_node_int16_set;
    result->modern_node_int32_set =
        default_modern_node_int32_set;
    result->modern_node_int64_set =
        default_modern_node_int64_set;
    result->modern_node_nat8_set =
        default_modern_node_nat8_set;
    result->modern_node_nat16_set =
        default_modern_node_nat16_set;
    result->modern_node_nat32_set =
        default_modern_node_nat32_set;
    result->modern_node_nat64_set =
        default_modern_node_nat64_set;
    result->modern_node_float32_set =
        default_modern_node_float32_set;
    result->modern_node_float64_set =
        default_modern_node_float64_set;
    result->modern_node_utf8_data_piece_set =
        default_modern_node_utf8_data_piece_set;
    result->modern_node_blob_data_piece_set =
        default_modern_node_blob_data_piece_set;
    result->modern_node_sigma_set =
        default_modern_node_sigma_set;
    result->modern_node_named_value_set =
        default_modern_node_named_value_set;
    result->modern_node_maybe_type_content_type_set =
        default_modern_node_maybe_type_content_type_set;
    result->modern_node_function_type_left_set =
        default_modern_node_function_type_left_set;
    result->modern_node_function_type_right_set =
        default_modern_node_function_type_right_set;
    result->modern_node_sigma_type_field_type_set =
        default_modern_node_sigma_type_field_type_set;
    result->modern_node_sigma_type_successor_set =
        default_modern_node_sigma_type_successor_set;
    result->modern_node_named_type_name_set =
        default_modern_node_named_type_name_set;
    result->modern_node_named_type_content_type_set =
        default_modern_node_named_type_content_type_set;
    result->modern_node_universe_type_level_set =
        default_modern_node_universe_type_level_set;
    result->modern_node_lambda_content_set =
        default_modern_node_lambda_content_set;
    result->modern_node_apply_left_set =
        default_modern_node_apply_left_set;
    result->modern_node_apply_right_set =
        default_modern_node_apply_right_set;
    result->modern_node_type_family_item_add =
        default_modern_node_type_family_item_add;
    result->modern_node_type_family_item_remove =
        default_modern_node_type_family_item_remove;
    result->modern_node_let_item_add =
        default_modern_node_let_item_add;
    result->modern_node_let_item_remove =
        default_modern_node_let_item_remove;
    result->modern_node_let_content_set =
        default_modern_node_let_content_set;
    result->modern_node_backreference_index_set =
        default_modern_node_backreference_index_set;
    result->modern_node_builtin_identifier_set =
        default_modern_node_builtin_identifier_set;
    
    return result;
}

