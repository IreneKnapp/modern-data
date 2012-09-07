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
        value->value_type = modern_node_make_int8_type(library_in);
        break;
    
    case int16_value_modern_node_type:
        value->value_type = modern_node_make_int16_type(library_in);
        break;
    
    case int32_value_modern_node_type:
        value->value_type = modern_node_make_int32_type(library_in);
        break;
    
    case int64_value_modern_node_type:
        value->value_type = modern_node_make_int64_type(library_in);
        break;
    
    case nat8_value_modern_node_type:
        value->value_type = modern_node_make_nat8_type(library_in);
        break;
    
    case nat16_value_modern_node_type:
        value->value_type = modern_node_make_nat16_type(library_in);
        break;
    
    case nat32_value_modern_node_type:
        value->value_type = modern_node_make_nat32_type(library_in);
        break;
    
    case nat64_value_modern_node_type:
        value->value_type = modern_node_make_nat64_type(library_in);
        break;
    
    case float32_value_modern_node_type:
        value->value_type = modern_node_make_float32_type(library_in);
        break;
    
    case float64_value_modern_node_type:
        value->value_type = modern_node_make_float64_type(library_in);
        break;
    
    case utf8_value_modern_node_type:
        value->value_type = modern_node_make_utf8_type(library_in);
        break;
    
    case blob_value_modern_node_type:
        value->value_type = modern_node_make_blob_type(library_in);
        break;
    
    case sigma_value_modern_node_type:
        break;
    
    case named_value_modern_node_type:
        break;
    
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
    case function_type_modern_node_type:
    case sigma_type_modern_node_type:
    case named_type_modern_node_type:
        value->value_type = modern_node_make_universe_type(library_in, 0);
        break;
    
    case universe_type_modern_node_type:
    {
        uint64_t level = modern_node_get_universe_type_level
            (library_in, value_in);
        if(level == UINT64_MAX) {
            library->error_handler->
                modern_error_handler_universe_level_overflow
                    (library->client_state);
        } else {
            modern *universe_type =
                modern_node_make_universe_type(library_in, level + 1);
            if(!universe_type) return NULL;
            
            value->value_type = universe_type;
        }
        
        break;
    }
    
    case lambda_modern_node_type:
        break;
    
    case apply_modern_node_type:
        break;
    
    case type_family_modern_node_type:
        library->error_handler->modern_error_handler_not_applicable
            (library->client_state);
    	break;
    
    case let_modern_node_type:
        break;
    
    case backreference_modern_node_type:
        break;
    
    case builtin_modern_node_type:
        switch(value->specifics.builtin) {
        case plus_int8_modern_builtin_identifier:
            // TODO
            break;
        default: break;
        }
    }
    
    return value->value_type;
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
        library->error_handler->modern_error_handler_buffer_index
            (library->client_state);
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
        library->error_handler->modern_error_handler_buffer_index
            (library->client_state);
        return NULL;
    }
}


modern *modern_node_get_sigma_field_value
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


modern *modern_node_get_function_type_left
  (modern_library *library_in,
   modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.function_type.left;
}


modern *modern_node_get_function_type_right
  (modern_library *library_in,
   modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.function_type.right;
}


modern *modern_node_get_sigma_type_field_type
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.sigma_type.field_type;
}


modern *modern_node_get_sigma_type_successor
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.sigma_type.successor;
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


uint64_t modern_node_get_type_family_count
  (modern_library *library_in,
   modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.type_family.n_items;
}


modern *modern_node_get_type_family_item
  (modern_library *library_in,
   modern *value_in, uint64_t index)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.type_family.members[index];
}


uint64_t modern_node_get_let_count
  (modern_library *library_in,
   modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.let.n_items;
}


modern *modern_node_get_let_item
  (modern_library *library_in,
   modern *value_in, uint64_t index)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.let.members[index];
}


modern *modern_node_get_let_content
  (modern_library *library_in,
   modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.let.content;
}


uint16_t modern_node_get_builtin_identifier
  (modern_library *library_in,
   modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.builtin;
}
