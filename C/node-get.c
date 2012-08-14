#include "modern.h"
#include "internal.h"


enum modern_node_type modern_node_get_node_type
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
    return value->node_type;
}


modern *modern_node_get_value_type
    (struct modern_error_handler *error_handler,
     struct modern_allocator *allocator,
     modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
    
    if(value->value_type) return value->value_type;
    
    switch(value->node_type) {
    case int8_value_modern_node_type:
        return modern_node_get_int8_type(error_handler);
    
    case int16_value_modern_node_type:
        return modern_node_get_int16_type(error_handler);
    
    case int32_value_modern_node_type:
        return modern_node_get_int32_type(error_handler);
    
    case int64_value_modern_node_type:
        return modern_node_get_int64_type(error_handler);
    
    case nat8_value_modern_node_type:
        return modern_node_get_nat8_type(error_handler);
    
    case nat16_value_modern_node_type:
        return modern_node_get_nat16_type(error_handler);
    
    case nat32_value_modern_node_type:
        return modern_node_get_nat32_type(error_handler);
    
    case nat64_value_modern_node_type:
        return modern_node_get_nat64_type(error_handler);
    
    case float32_value_modern_node_type:
        return modern_node_get_float32_type(error_handler);
    
    case float64_value_modern_node_type:
        return modern_node_get_float64_type(error_handler);
    
    case float128_value_modern_node_type:
        return modern_node_get_float128_type(error_handler);
    
    case utf8_value_modern_node_type:
        return modern_node_get_utf8_type(error_handler);
    
    case blob_value_modern_node_type:
        return modern_node_get_blob_type(error_handler);
    
    case sigma_value_modern_node_type:
    {
        struct modern *sigma_field_value =
            value->specifics.sigma_value.field_value;
        
        modern *sigma_field_type = modern_node_get_value_type
            (error_handler, (modern *) sigma_field_value);
        if(!sigma_field_type) return NULL;
        
        struct modern *sigma_successor =
            value->specifics.sigma_value.successor;
        
        modern *sigma_type = modern_node_make_sigma_type
            (error_handler, sigma_field_type, sigma_successor);
        if(!sigma_type) return NULL;
        
        value->specifics.value_type = sigma_type;
        
        return sigma_type;
    }
    
    case named_value_modern_node_type:
        return NULL;
    
    case int8_type_modern_node_type:
    case int16_type_modern_node_type:
    case int32_type_modern_node_type:
    case int64_type_modern_node_type:
    case nat8_type_modern_node_type:
    case nat16_type_modern_node_type:
    case nat32_type_modern_node_type:
    case nat64_type_modern_node_type:
    case float32_type_modern_node_type:
    case float64_type_modern_node_type:
    case float128_type_modern_node_type:
    case utf8_type_modern_node_type:
    case blob_type_modern_node_type:
    case sigma_type_modern_node_type:
    case named_type_modern_node_type:
        // TODO cache this
        return modern_node_make_universe_type
            (error_handler, allocator, 0);
    
    case universe_type_modern_node_type:
        // TODO
    
    case lambda_modern_node_type:
        // TODO
    
    case apply_modern_node_type:
        // TODO
    }
}


int8_t modern_node_get_int8
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


int16_t modern_node_get_int16
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


int32_t modern_node_get_int32
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


int64_t modern_node_get_int64
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


uint8_t modern_node_get_nat8
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


uint16_t modern_node_get_nat16
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


uint32_t modern_node_get_nat32
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


uint64_t modern_node_get_nat64
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


float modern_node_get_float32
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


double modern_node_get_float64
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


long double modern_node_get_float128
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


size_t modern_node_get_utf8_bytes
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


uint8_t *modern_node_get_utf8_data_piece
  (struct modern_error_handler *error_handler, modern *value_in,
   size_t offset, size_t bytes)
{
    struct modern *value = (struct modern *) value_in;
}


size_t modern_node_get_blob_bytes
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


uint8_t *modern_node_get_blob_data_piece
  (struct modern_error_handler *error_handler, modern *value_in,
   size_t offset, size_t bytes)
{
    struct modern *value = (struct modern *) value_in;
}


modern *modern_node_get_sigma_value
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


modern *modern_node_get_sigma_successor
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


struct modern_hash *modern_node_get_named_type_name
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


modern *modern_node_get_named_type_content_type
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


uint64_t modern_node_get_universe_type_level
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


modern *modern_node_get_lambda_content
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


modern *modern_node_get_apply_left
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}


modern *modern_node_get_apply_right
    (struct modern_error_handler *error_handler, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
}
