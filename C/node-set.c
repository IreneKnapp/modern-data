#include <math.h>
#include <string.h>
#include "modern.h"
#include "internal.h"


INTERNAL void default_immutable_set
    (modern_library *library,
     void *value)
{
    // TODO
}


INTERNAL void default_canonical_hash_set
    (modern_library *library_in,
     void *value_in,
     struct modern_hash hash)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    
    if(!value->mutable) {
        library->error_handler->immutable
            (library->client_state, value_in);
    }
    
    value->canonical_hash_valid = 1;
    value->canonical_hash = hash;
}


INTERNAL void default_maybe_just_content_set
    (modern_library *library,
     void *value,
     void *content_value)
{
    // TODO
}


INTERNAL void default_int8_set
    (modern_library *library,
     void *node,
     int8_t value)
{
    // TODO
}


INTERNAL void default_int16_set
    (modern_library *library,
     void *node,
     int16_t value)
{
    // TODO
}


INTERNAL void default_int32_set
    (modern_library *library,
     void *node,
     int32_t value)
{
    // TODO
}


INTERNAL void default_int64_set
    (modern_library *library,
     void *node,
     int64_t value)
{
    // TODO
}


INTERNAL void default_nat8_set
    (modern_library *library,
     void *node,
     uint8_t value)
{
    // TODO
}


INTERNAL void default_nat16_set
    (modern_library *library,
     void *node,
     uint16_t value)
{
    // TODO
}


INTERNAL void default_nat32_set
    (modern_library *library,
     void *node,
     uint32_t value)
{
    // TODO
}


INTERNAL void default_nat64_set
    (modern_library *library,
     void *node,
     uint64_t value)
{
    // TODO
}


INTERNAL void default_float32_set
    (modern_library *library,
     void *node,
     float value)
{
    // TODO
}


INTERNAL void default_float64_set
    (modern_library *library,
     void *node,
     double value)
{
    // TODO
}


INTERNAL void default_utf8_data_piece_set
    (modern_library *library,
     void *value,
     uint8_t *data,
     size_t offset,
     size_t old_bytes,
     size_t new_bytes)
{
    // TODO
}


INTERNAL void default_blob_data_piece_set
    (modern_library *library,
     void *value,
     uint8_t *data,
     size_t offset,
     size_t old_bytes,
     size_t new_bytes)
{
    // TODO
}


INTERNAL void default_sigma_set
    (modern_library *library,
     void *value,
     void *field_value,
     void *successor)
{
    // TODO
}


INTERNAL void default_named_value_set
    (modern_library *library,
     void *node,
     void *type,
     void *value)
{
    // TODO
}


INTERNAL void default_maybe_type_content_type_set
    (modern_library *library,
     void *value,
     void *content_type)
{
    // TODO
}


INTERNAL void default_function_type_parameter_set
    (modern_library *library,
     void *value,
     void *parameter)
{
    // TODO
}


INTERNAL void default_function_type_result_set
    (modern_library *library,
     void *value,
     void *result)
{
    // TODO
}


INTERNAL void default_sigma_type_field_type_set
    (modern_library *library,
     void *value,
     void *field_type)
{
    // TODO
}


INTERNAL void default_sigma_type_successor_set
    (modern_library *library,
     void *value,
     void *successor)
{
    // TODO
}


INTERNAL void default_named_type_name_set
    (modern_library *library,
     void *value,
     struct modern_hash name)
{
    // TODO
}


INTERNAL void default_named_type_content_type_set
    (modern_library *library,
     void *value,
     void *content_type)
{
    // TODO
}


INTERNAL void default_universe_type_level_set
    (modern_library *library,
     void *value,
     uint64_t level)
{
    // TODO
}


INTERNAL void default_lambda_content_set
    (modern_library *library,
     void *value,
     void *content)
{
    // TODO
}


INTERNAL void default_apply_function_set
    (modern_library *library,
     void *value,
     void *function)
{
    // TODO
}


INTERNAL void default_apply_parameter_set
    (modern_library *library,
     void *value,
     void *parameter)
{
    // TODO
}


INTERNAL void default_type_family_item_add
    (modern_library *library,
     void *value,
     void *item,
     uint64_t index)
{
    // TODO
}


INTERNAL void default_type_family_item_remove
    (modern_library *library,
     void *value,
     uint64_t index)
{
    // TODO
}


INTERNAL void default_let_item_add
    (modern_library *library,
     void *value,
     void *item,
     uint64_t index)
{
    // TODO
}


INTERNAL void default_let_item_remove
    (modern_library *library,
     void *value,
     uint64_t index)
{
    // TODO
}


INTERNAL void default_let_content_set
    (modern_library *library,
     void *value,
     void *content)
{
    // TODO
}


INTERNAL void default_backreference_index_set
    (modern_library *library,
     void *value,
     uint64_t index)
{
    // TODO
}


INTERNAL void default_builtin_identifier_set
    (modern_library *library,
     void *value,
     uint16_t identifier)
{
    // TODO
}

