#include <string.h>
#include "modern.h"
#include "internal.h"


modern *modern_node_make_int8
    (modern_library *library_in,
     int8_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = int8_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.int8_value = value;
    
    return (modern *) result;
}


modern *modern_node_make_int16
    (modern_library *library_in,
     int16_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = int16_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.int16_value = value;
    
    return (modern *) result;
}


modern *modern_node_make_int32
    (modern_library *library_in,
     int32_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = int32_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.int32_value = value;
    
    return (modern *) result;
}


modern *modern_node_make_int64
    (modern_library *library_in,
     int64_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = int64_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.int64_value = value;
    
    return (modern *) result;
}


modern *modern_node_make_nat8
    (modern_library *library_in,
     uint8_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = nat8_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.nat8_value = value;
    
    return (modern *) result;
}


modern *modern_node_make_nat16
    (modern_library *library_in,
     uint16_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = nat16_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.nat16_value = value;
    
    return (modern *) result;
}


modern *modern_node_make_nat32
    (modern_library *library_in,
     uint32_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = nat32_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.nat32_value = value;
    
    return (modern *) result;
}


modern *modern_node_make_nat64
    (modern_library *library_in,
     uint64_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = nat64_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.nat64_value = value;
    
    return (modern *) result;
}


modern *modern_node_make_float32
    (modern_library *library_in,
     float value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = float32_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.float32_value = value;
    
    return (modern *) result;
}


modern *modern_node_make_float64
    (modern_library *library_in,
     double value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = float64_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.float64_value = value;
    
    return (modern *) result;
}


modern *modern_node_make_float128
    (modern_library *library_in,
     long double value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = float128_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.float128_value = value;
    
    return (modern *) result;
}


modern *modern_node_make_utf8
    (modern_library *library_in,
     uint8_t *data)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    size_t bytes = strlen((char *) data);
    uint8_t *copied_data =
        library->allocator->modern_allocator_alloc(bytes);
    if(!copied_data) {
        library->allocator->modern_allocator_free(result);
        library->error_handler->modern_error_handler_memory(bytes);
        return NULL;
    }
    memcpy(copied_data, data, bytes);
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = utf8_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.utf8_value.bytes = bytes;
    result->specifics.utf8_value.data = copied_data;
    
    return (modern *) result;
}


modern *modern_node_make_blob
    (modern_library *library_in,
     uint8_t *data, size_t bytes)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    uint8_t *copied_data =
        library->allocator->modern_allocator_alloc(bytes);
    if(!copied_data) {
        library->allocator->modern_allocator_free(result);
        library->error_handler->modern_error_handler_memory(bytes);
        return NULL;
    }
    memcpy(copied_data, data, bytes);
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = blob_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.blob_value.bytes = bytes;
    result->specifics.blob_value.data = copied_data;
    
    return (modern *) result;
}


modern *modern_node_make_sigma
    (modern_library *library_in,
     modern *type_in,
     modern *field_value_in,
     modern *successor_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *field_value = (struct modern *) field_value_in;
    struct modern *successor = (struct modern *) successor_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = sigma_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.sigma_value.field_value = field_value;
    result->specifics.sigma_value.successor = successor;
    
    return (modern *) result;
}


modern *modern_node_make_named_value
    (modern_library *library_in,
     modern *type_in, modern *value_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *type = (struct modern *) type_in;
    struct modern *value = (struct modern *) value_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = named_value_modern_node_type;
    result->value_type = type;
    result->specifics.named_value.value = value;
    
    return (modern *) result;
}


modern *modern_node_make_int8_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    if(library->cache_context) {
    }
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc(result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory(result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    
    result->node_type = int8_type_modern_node_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


modern *modern_node_make_int16_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_int32_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_int64_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_nat8_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_nat16_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_nat32_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_nat64_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_float32_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_float64_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_float128_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_utf8_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_blob_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_sigma_type
    (modern_library *library_in,
     modern *field_type, modern *successor)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_named_type
    (modern_library *library_in,
     struct modern_hash *name, modern *content_type_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_universe_type
    (modern_library *library_in,
     uint64_t level)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_lambda
    (modern_library *library_in,
     modern *content_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_apply
    (modern_library *library_in,
     modern *left_in, modern *right_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_type_index
    (modern_library *library_in,
     uint64_t index)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}


modern *modern_node_make_type_family
    (modern_library *library_in,
     uint64_t n_items, modern **types_in, modern *content_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
}
