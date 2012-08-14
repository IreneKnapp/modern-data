#include "modern.h"
#include "internal.h"


modern *modern_node_make_int8
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     int8_t value)
{
}


modern *modern_node_make_int16
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     int16_t value)
{
}


modern *modern_node_make_int32
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     int32_t value)
{
}


modern *modern_node_make_int64
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     int64_t value)
{
}


modern *modern_node_make_nat8
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     uint8_t value)
{
}


modern *modern_node_make_nat16
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     uint16_t value)
{
}


modern *modern_node_make_nat32
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     uint32_t value)
{
}


modern *modern_node_make_nat64
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     uint64_t value)
{
}


modern *modern_node_make_float32
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     float value)
{
}


modern *modern_node_make_float64
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     double value)
{
}


modern *modern_node_make_float128
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     long double value)
{
}


modern *modern_node_make_utf8
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     uint8_t *data)
{
}


modern *modern_node_make_blob
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     uint8_t *data, size_t bytes)
{
}


modern *modern_node_make_sigma
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     modern *type_in,
     modern *field_value_in,
     modern *successor_value_in)
{
}


modern *modern_node_make_named_value
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     modern *type_in, modern *value_in)
{
}


modern *modern_node_get_int8_type
    (struct modern_error_handler *error_handler)
{
}


modern *modern_node_get_int16_type
    (struct modern_error_handler *error_handler)
{
}


modern *modern_node_get_int32_type
    (struct modern_error_handler *error_handler)
{
}


modern *modern_node_get_int64_type
    (struct modern_error_handler *error_handler)
{
}


modern *modern_node_get_nat8_type
    (struct modern_error_handler *error_handler)
{
}


modern *modern_node_get_nat16_type
    (struct modern_error_handler *error_handler)
{
}


modern *modern_node_get_nat32_type
    (struct modern_error_handler *error_handler)
{
}


modern *modern_node_get_nat64_type
    (struct modern_error_handler *error_handler)
{
}


modern *modern_node_get_float32_type
    (struct modern_error_handler *error_handler)
{
}


modern *modern_node_get_float64_type
    (struct modern_error_handler *error_handler)
{
}


modern *modern_node_get_float128_type
    (struct modern_error_handler *error_handler)
{
}


modern *modern_node_get_utf8_type
    (struct modern_error_handler *error_handler)
{
}


modern *modern_node_get_blob_type
    (struct modern_error_handler *error_handler)
{
}


modern *modern_node_make_sigma_type
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     modern *field_type, modern *successor)
{
}


modern *modern_node_make_named_type
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     struct modern_hash *name, modern *content_type_in)
{
}


modern *modern_node_make_universe_type
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     uint64_t level)
{
}


modern *modern_node_make_lambda
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     modern *content_in)
{
}


modern *modern_node_make_apply
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     modern *left_in, modern *right_in)
{
}


modern *modern_node_make_type_index
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     uint64_t index)
{
}


modern *modern_node_make_type_family
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     uint64_t n_items, modern **types_in)
{
}
