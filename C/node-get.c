#include "modern.h"
#include "internal.h"


enum modern_node_type modern_node_get_node_type
    (modern_library *library_in, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
    return value->node_type;
}


modern *modern_node_get_value_type
    (modern_library *library_in,
     modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    
    if(value->value_type) return value->value_type;
    
    switch(value->node_type) {
    case int8_value_modern_node_type:
        return modern_node_make_int8_type(library_in);
    
    case int16_value_modern_node_type:
        return modern_node_make_int16_type(library_in);
    
    case int32_value_modern_node_type:
        return modern_node_make_int32_type(library_in);
    
    case int64_value_modern_node_type:
        return modern_node_make_int64_type(library_in);
    
    case nat8_value_modern_node_type:
        return modern_node_make_nat8_type(library_in);
    
    case nat16_value_modern_node_type:
        return modern_node_make_nat16_type(library_in);
    
    case nat32_value_modern_node_type:
        return modern_node_make_nat32_type(library_in);
    
    case nat64_value_modern_node_type:
        return modern_node_make_nat64_type(library_in);
    
    case float32_value_modern_node_type:
        return modern_node_make_float32_type(library_in);
    
    case float64_value_modern_node_type:
        return modern_node_make_float64_type(library_in);
    
    case utf8_value_modern_node_type:
        return modern_node_make_utf8_type(library_in);
    
    case blob_value_modern_node_type:
        return modern_node_make_blob_type(library_in);
    
    case sigma_value_modern_node_type:
    {
        modern *sigma_field_value =
            (modern *) value->specifics.sigma_value.field_value;
        
        modern *sigma_field_type = modern_node_get_value_type
            (library_in, (modern *) sigma_field_value);
        if(!sigma_field_type) return NULL;
        
        struct modern *sigma_successor =
            value->specifics.sigma_value.successor;
        
        modern *sigma_type = modern_node_make_sigma_type
            (library_in, sigma_field_type, sigma_successor);
        if(!sigma_type) return NULL;
        
        value->value_type = sigma_type;
        
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
    case utf8_type_modern_node_type:
    case blob_type_modern_node_type:
    case sigma_type_modern_node_type:
    case named_type_modern_node_type:
        return modern_node_make_universe_type
            (library_in, 0);
    
    case universe_type_modern_node_type:
    {
        uint64_t level = modern_node_get_universe_type_level
            (library_in, value_in);
        if(level == UINT64_MAX) {
            library->error_handler->
                modern_error_handler_universe_level_overflow();
            return NULL;
        } else {
            modern *universe_type =
                modern_node_make_universe_type(library_in, level + 1);
            if(!universe_type) return NULL;
            
            value->value_type = universe_type;
            
            return universe_type;
        }
    }
    
    case lambda_modern_node_type:
        // TODO
        break;
    
    case apply_modern_node_type:
        // TODO
        break;
    
    case family_modern_node_type:
    	// TODO
    	break;
    }
}


int8_t modern_node_get_int8
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.int8_value;
}


int16_t modern_node_get_int16
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.int16_value;
}


int32_t modern_node_get_int32
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.int32_value;
}


int64_t modern_node_get_int64
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.int64_value;
}


uint8_t modern_node_get_nat8
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.nat8_value;
}


uint16_t modern_node_get_nat16
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.nat16_value;
}


uint32_t modern_node_get_nat32
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.nat32_value;
}


uint64_t modern_node_get_nat64
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.nat64_value;
}


float modern_node_get_float32
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.float32_value;
}


double modern_node_get_float64
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.float64_value;
}


size_t modern_node_get_utf8_bytes
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.utf8_value.bytes;
}


uint8_t *modern_node_get_utf8_data_piece
  (modern_library *library_in, modern *value_in,
   size_t offset, size_t bytes)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    if(offset + bytes < value->specifics.utf8_value.bytes) {
        return value->specifics.utf8_value.data + offset;
    } else {
        library->error_handler->modern_error_handler_buffer_index();
        return NULL;
    }
}


size_t modern_node_get_blob_bytes
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.blob_value.bytes;
}


uint8_t *modern_node_get_blob_data_piece
  (modern_library *library_in, modern *value_in,
   size_t offset, size_t bytes)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    if(offset + bytes < value->specifics.blob_value.bytes) {
        return value->specifics.blob_value.data + offset;
    } else {
        library->error_handler->modern_error_handler_buffer_index();
        return NULL;
    }
}


modern *modern_node_get_sigma_value
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.sigma_value.field_value;
}


modern *modern_node_get_sigma_successor
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.sigma_value.successor;
}


modern *modern_node_get_named_value
    (modern_library *library_in,
     modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.named_value.value;
}


struct modern_hash *modern_node_get_named_type_name
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return &value->specifics.named_type.name;
}


modern *modern_node_get_named_type_content_type
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.named_type.content_type;
}


uint64_t modern_node_get_universe_type_level
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.universe_type.level;
}


modern *modern_node_get_lambda_content
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.lambda.content;
}


modern *modern_node_get_apply_left
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.apply.left;
}


modern *modern_node_get_apply_right
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.apply.right;
}
