#include "modern.h"
#include "internal.h"


INTERNAL enum modern_node_type default_modern_node_type_get
    (modern_library *library_in, modern *value_in)
{
    struct modern *value = (struct modern *) value_in;
    return value->node_type;
}


INTERNAL modern *default_modern_value_type_get
    (modern_library *library_in,
     modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    if(!library->node) {
        library->error_handler->modern_error_handler_usage(library->client_state);
        return NULL;
    }
    
    struct modern *value = (struct modern *) value_in;
    
    if(value->value_type) return value->value_type;
    
    switch(value->node_type) {
    case modern_node_type_bool_value_false:
        value->value_type = library->node->modern_node_bool_type_make(library_in);
        break;
    
    case modern_node_type_bool_value_true:
        value->value_type = library->node->modern_node_bool_type_make(library_in);
        break;
    
    case modern_node_type_ordering_value_less:
        value->value_type = library->node->modern_node_ordering_type_make(library_in);
        break;
    
    case modern_node_type_ordering_value_equal:
        value->value_type = library->node->modern_node_ordering_type_make(library_in);
        break;
    
    case modern_node_type_ordering_value_greater:
        value->value_type = library->node->modern_node_ordering_type_make(library_in);
        break;
    
    case modern_node_type_maybe_value_nothing:
        break;
    
    case modern_node_type_maybe_value_just:
        break;
    
    case modern_node_type_int8_value:
        value->value_type = library->node->modern_node_int8_type_make(library_in);
        break;
    
    case modern_node_type_int16_value:
        value->value_type = library->node->modern_node_int16_type_make(library_in);
        break;
    
    case modern_node_type_int32_value:
        value->value_type = library->node->modern_node_int32_type_make(library_in);
        break;
    
    case modern_node_type_int64_value:
        value->value_type = library->node->modern_node_int64_type_make(library_in);
        break;
    
    case modern_node_type_nat8_value:
        value->value_type = library->node->modern_node_nat8_type_make(library_in);
        break;
    
    case modern_node_type_nat16_value:
        value->value_type = library->node->modern_node_nat16_type_make(library_in);
        break;
    
    case modern_node_type_nat32_value:
        value->value_type = library->node->modern_node_nat32_type_make(library_in);
        break;
    
    case modern_node_type_nat64_value:
        value->value_type = library->node->modern_node_nat64_type_make(library_in);
        break;
    
    case modern_node_type_float32_value:
        value->value_type = library->node->modern_node_float32_type_make(library_in);
        break;
    
    case modern_node_type_float64_value:
        value->value_type = library->node->modern_node_float64_type_make(library_in);
        break;
    
    case modern_node_type_utf8_value:
        value->value_type = library->node->modern_node_utf8_type_make(library_in);
        break;
    
    case modern_node_type_blob_value:
        value->value_type = library->node->modern_node_blob_type_make(library_in);
        break;
    
    case modern_node_type_sigma_value:
        break;
    
    case modern_node_type_name_value:
        value->value_type = library->node->modern_node_name_type_make(library_in);
    
    case modern_node_type_named_value:
        break;
    
    case modern_node_type_bool_type:
    case modern_node_type_ordering_type:
    case modern_node_type_maybe_type:
    case modern_node_type_int8_type:
    case modern_node_type_int16_type:
    case modern_node_type_int32_type:
    case modern_node_type_int64_type:
    case modern_node_type_nat8_type:
    case modern_node_type_nat16_type:
    case modern_node_type_nat32_type:
    case modern_node_type_nat64_type:
    case modern_node_type_float32_type:
    case modern_node_type_float64_type:
    case modern_node_type_utf8_type:
    case modern_node_type_blob_type:
    case modern_node_type_function_type:
    case modern_node_type_sigma_type:
    case modern_node_type_name_type:
    case modern_node_type_named_type:
        value->value_type = library->node->modern_node_universe_type_make(library_in, 0);
        break;
    
    case modern_node_type_universe_type:
    {
        uint64_t level = library->node->modern_node_universe_type_level_get
            (library_in, value_in);
        if(level == UINT64_MAX) {
            library->error_handler->
                modern_error_handler_universe_level_overflow
                    (library->client_state);
        } else {
            modern *universe_type =
                library->node->modern_node_universe_type_make(library_in, level + 1);
            if(!universe_type) return NULL;
            
            value->value_type = universe_type;
        }
        
        break;
    }
    
    case modern_node_type_lambda:
        break;
    
    case modern_node_type_apply:
        break;
    
    case modern_node_type_type_family:
        library->error_handler->modern_error_handler_not_applicable
            (library->client_state);
    	break;
    
    case modern_node_type_let:
        break;
    
    case modern_node_type_backreference:
        break;
    
    case modern_node_type_builtin:
        switch(value->specifics.builtin) {
            // TODO
        default: break;
        }
    }
    
    return value->value_type;
}


INTERNAL int8_t default_modern_int8_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    if(!library->node) {
        library->error_handler->modern_error_handler_usage(library->client_state);
        return 0;
    }
    
    struct modern *value = (struct modern *) value_in;
    return value->specifics.int8_value;
}


INTERNAL int16_t default_modern_int16_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.int16_value;
}


INTERNAL int32_t default_modern_int32_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.int32_value;
}


INTERNAL int64_t default_modern_int64_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.int64_value;
}


INTERNAL uint8_t default_modern_nat8_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.nat8_value;
}


INTERNAL uint16_t default_modern_nat16_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.nat16_value;
}


INTERNAL uint32_t default_modern_nat32_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.nat32_value;
}


INTERNAL uint64_t default_modern_nat64_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.nat64_value;
}


INTERNAL float default_modern_float32_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.float32_value;
}


INTERNAL double default_modern_float64_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.float64_value;
}


INTERNAL size_t default_modern_utf8_bytes_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.utf8_value.bytes;
}


INTERNAL uint8_t *default_modern_utf8_data_piece_get
  (modern_library *library_in, modern *value_in,
   size_t offset, size_t bytes)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    if(offset + bytes <= value->specifics.utf8_value.bytes) {
        return value->specifics.utf8_value.data + offset;
    } else {
        library->error_handler->modern_error_handler_buffer_index
            (library->client_state);
        return NULL;
    }
}


INTERNAL size_t default_modern_blob_bytes_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.blob_value.bytes;
}


INTERNAL uint8_t *default_modern_blob_data_piece_get
  (modern_library *library_in, modern *value_in,
   size_t offset, size_t bytes)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    if(offset + bytes <= value->specifics.blob_value.bytes) {
        return value->specifics.blob_value.data + offset;
    } else {
        library->error_handler->modern_error_handler_buffer_index
            (library->client_state);
        return NULL;
    }
}


INTERNAL modern *default_modern_sigma_field_value_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.sigma_value.field_value;
}


INTERNAL modern *default_modern_sigma_successor_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.sigma_value.successor;
}


INTERNAL modern *default_modern_named_value_get
    (modern_library *library_in,
     modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.named_value.value;
}


INTERNAL modern *default_modern_function_type_left_get
  (modern_library *library_in,
   modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.function_type.left;
}


INTERNAL modern *default_modern_function_type_right_get
  (modern_library *library_in,
   modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.function_type.right;
}


INTERNAL modern *default_modern_sigma_type_field_type_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.sigma_type.field_type;
}


INTERNAL modern *default_modern_sigma_type_successor_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.sigma_type.successor;
}


INTERNAL struct modern_hash default_modern_named_type_name_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.named_type.name;
}


INTERNAL modern *default_modern_named_type_content_type_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.named_type.content_type;
}


INTERNAL uint64_t default_modern_universe_type_level_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.universe_type.level;
}


INTERNAL modern *default_modern_lambda_content_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.lambda.content;
}


INTERNAL modern *default_modern_apply_left_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.apply.left;
}


INTERNAL modern *default_modern_apply_right_get
    (modern_library *library_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.apply.right;
}


INTERNAL uint64_t default_modern_type_family_count_get
  (modern_library *library_in,
   modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.type_family.n_items;
}


INTERNAL modern *default_modern_type_family_item_get
  (modern_library *library_in,
   modern *value_in, uint64_t index)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.type_family.members[index];
}


INTERNAL uint64_t default_modern_let_count_get
  (modern_library *library_in,
   modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.let.n_items;
}


INTERNAL modern *default_modern_let_item_get
  (modern_library *library_in,
   modern *value_in, uint64_t index)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.let.members[index];
}


INTERNAL modern *default_modern_let_content_get
  (modern_library *library_in,
   modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.let.content;
}


INTERNAL uint16_t default_modern_builtin_identifier_get
  (modern_library *library_in,
   modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    return value->specifics.builtin;
}
