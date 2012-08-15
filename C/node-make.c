#include "modern.h"
#include "internal.h"


modern *modern_node_make_int8
    (modern_library *library,
     int8_t value)
{
}


modern *modern_node_make_int16
    (modern_library *library,
     int16_t value)
{
}


modern *modern_node_make_int32
    (modern_library *library,
     int32_t value)
{
}


modern *modern_node_make_int64
    (modern_library *library,
     int64_t value)
{
}


modern *modern_node_make_nat8
    (modern_library *library,
     uint8_t value)
{
}


modern *modern_node_make_nat16
    (modern_library *library,
     uint16_t value)
{
}


modern *modern_node_make_nat32
    (modern_library *library,
     uint32_t value)
{
}


modern *modern_node_make_nat64
    (modern_library *library,
     uint64_t value)
{
}


modern *modern_node_make_float32
    (modern_library *library,
     float value)
{
}


modern *modern_node_make_float64
    (modern_library *library,
     double value)
{
}


modern *modern_node_make_float128
    (modern_library *library,
     long double value)
{
}


modern *modern_node_make_utf8
    (modern_library *library,
     uint8_t *data)
{
}


modern *modern_node_make_blob
    (modern_library *library,
     uint8_t *data, size_t bytes)
{
}


modern *modern_node_make_sigma
    (modern_library *library,
     modern *type_in,
     modern *field_value_in,
     modern *successor_value_in)
{
}


modern *modern_node_make_named_value
    (modern_library *library,
     modern *type_in, modern *value_in)
{
}


modern *modern_node_make_int8_type
    (modern_library *library)
{
}


modern *modern_node_make_int16_type
    (modern_library *library)
{
}


modern *modern_node_make_int32_type
    (modern_library *library)
{
}


modern *modern_node_make_int64_type
    (modern_library *library)
{
}


modern *modern_node_make_nat8_type
    (modern_library *library)
{
}


modern *modern_node_make_nat16_type
    (modern_library *library)
{
}


modern *modern_node_make_nat32_type
    (modern_library *library)
{
}


modern *modern_node_make_nat64_type
    (modern_library *library)
{
}


modern *modern_node_make_float32_type
    (modern_library *library)
{
}


modern *modern_node_make_float64_type
    (modern_library *library)
{
}


modern *modern_node_make_float128_type
    (modern_library *library)
{
}


modern *modern_node_make_utf8_type
    (modern_library *library)
{
}


modern *modern_node_make_blob_type
    (modern_library *library)
{
}


modern *modern_node_make_sigma_type
    (modern_library *library,
     modern *field_type, modern *successor)
{
}


modern *modern_node_make_named_type
    (modern_library *library,
     struct modern_hash *name, modern *content_type_in)
{
}


modern *modern_node_make_universe_type
    (modern_library *library,
     uint64_t level)
{
}


modern *modern_node_make_lambda
    (modern_library *library,
     modern *content_in)
{
}


modern *modern_node_make_apply
    (modern_library *library,
     modern *left_in, modern *right_in)
{
}


modern *modern_node_make_type_index
    (modern_library *library,
     uint64_t index)
{
}


modern *modern_node_make_type_family
    (modern_library *library,
     uint64_t n_items, modern **types_in)
{
}
