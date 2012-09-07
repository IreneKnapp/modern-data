#include <string.h>
#include "modern.h"
#include "internal.h"


HELPER void helper_finalize_utf8
  (struct modern_library *library, void *retainable);
HELPER void helper_finalize_blob
  (struct modern_library *library, void *retainable);
HELPER void helper_finalize_sigma_value
  (struct modern_library *library, void *retainable);
HELPER void helper_finalize_named_value
  (struct modern_library *library, void *retainable);
HELPER void helper_finalize_function_type
  (struct modern_library *library, void *retainable);
HELPER void helper_finalize_sigma_type
  (struct modern_library *library, void *retainable);
HELPER void helper_finalize_named_type
  (struct modern_library *library, void *retainable);
HELPER void helper_finalize_lambda
  (struct modern_library *library, void *retainable);
HELPER void helper_finalize_apply
  (struct modern_library *library, void *retainable);
HELPER void helper_finalize_type_family
  (struct modern_library *library, void *retainable);
HELPER void helper_finalize_let
  (struct modern_library *library, void *retainable);


modern *modern_node_make_int8
    (modern_library *library_in,
     int8_t value)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
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
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
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
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
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
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
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
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
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
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
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
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
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
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
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
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
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
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = float64_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.float64_value = value;
    
    return (modern *) result;
}


modern *modern_node_make_utf8
    (modern_library *library_in,
     uint8_t *data)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    size_t bytes = strlen((char *) data);
    uint8_t *copied_data =
        library->allocator->modern_allocator_alloc
            (library->client_state, bytes);
    if(!copied_data) {
        library->allocator->modern_allocator_free
            (library->client_state, result);
        library->error_handler->modern_error_handler_memory
            (library->client_state, bytes);
        return NULL;
    }
    memcpy(copied_data, data, bytes);
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_utf8;
    
    result->node_type = utf8_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.utf8_value.bytes = bytes;
    result->specifics.utf8_value.data = copied_data;
    
    return (modern *) result;
}


HELPER void helper_finalize_utf8
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    library->allocator->modern_allocator_free
        (library->client_state, node->specifics.utf8_value.data);
}


modern *modern_node_make_blob
    (modern_library *library_in,
     uint8_t *data, size_t bytes)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    uint8_t *copied_data =
        library->allocator->modern_allocator_alloc
            (library->client_state, bytes);
    if(!copied_data) {
        library->allocator->modern_allocator_free
            (library->client_state, result);
        library->error_handler->modern_error_handler_memory
            (library->client_state, bytes);
        return NULL;
    }
    memcpy(copied_data, data, bytes);
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_blob;
    
    result->node_type = blob_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.blob_value.bytes = bytes;
    result->specifics.blob_value.data = copied_data;
    
    return (modern *) result;
}


HELPER void helper_finalize_blob
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    library->allocator->modern_allocator_free
        (library->client_state, node->specifics.blob_value.data);
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
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_sigma_value;
    
    result->node_type = sigma_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.sigma_value.field_value = field_value;
    result->specifics.sigma_value.successor = successor;
    
    modern_retain(library_in, field_value_in);
    modern_retain(library_in, successor_in);
    
    return (modern *) result;
}


HELPER void helper_finalize_sigma_value
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_release(library, node->specifics.sigma_value.field_value);
    modern_release(library, node->specifics.sigma_value.successor);
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
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_named_value;
    
    result->node_type = named_value_modern_node_type;
    result->value_type = NULL;
    result->specifics.named_value.type = type;
    result->specifics.named_value.value = value;
    
    modern_retain(library_in, type_in);
    modern_retain(library_in, value_in);
    
    return (modern *) result;
}


HELPER void helper_finalize_named_value
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_release(library, node->specifics.named_value.type);
    modern_release(library, node->specifics.named_value.value);
}


modern *modern_node_make_int8_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = int8_type_modern_node_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


modern *modern_node_make_int16_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = int16_type_modern_node_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


modern *modern_node_make_int32_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = int32_type_modern_node_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


modern *modern_node_make_int64_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = int64_type_modern_node_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


modern *modern_node_make_nat8_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = nat8_type_modern_node_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


modern *modern_node_make_nat16_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = nat16_type_modern_node_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


modern *modern_node_make_nat32_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = nat32_type_modern_node_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


modern *modern_node_make_nat64_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = nat64_type_modern_node_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


modern *modern_node_make_float32_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = float32_type_modern_node_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


modern *modern_node_make_float64_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = float64_type_modern_node_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


modern *modern_node_make_utf8_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = utf8_type_modern_node_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


modern *modern_node_make_blob_type
    (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = blob_type_modern_node_type;
    result->value_type = NULL;
    
    return (modern *) result;
}


modern *modern_node_make_function_type
    (modern_library *library_in,
     modern *left_in, modern *right_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *left = (struct modern *) left_in;
    struct modern *right = (struct modern *) right_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_function_type;
    
    result->node_type = function_type_modern_node_type;
    result->value_type = NULL;
    result->specifics.function_type.left = left;
    result->specifics.function_type.right = right;
    
    modern_retain(library, left_in);
    modern_retain(library, right_in);
    
    return (modern *) result;
}



HELPER void helper_finalize_function_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_release(library, node->specifics.function_type.left);
    modern_release(library, node->specifics.function_type.right);
}


modern *modern_node_make_sigma_type
    (modern_library *library_in,
     modern *field_type, modern *successor)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_sigma_type;
    
    result->node_type = sigma_type_modern_node_type;
    result->value_type = NULL;
    result->specifics.sigma_type.field_type = field_type;
    result->specifics.sigma_type.successor = successor;
    
    modern_retain(library, field_type);
    modern_retain(library, successor);
    
    return (modern *) result;
}



HELPER void helper_finalize_sigma_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_release(library, node->specifics.sigma_type.field_type);
    modern_release(library, node->specifics.sigma_type.successor);
}


modern *modern_node_make_named_type
    (modern_library *library_in,
     struct modern_hash *name, modern *content_type)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_named_type;
    
    result->node_type = named_type_modern_node_type;
    result->value_type = NULL;
    result->specifics.named_type.name = *name;
    result->specifics.named_type.content_type = content_type;
    
    return (modern *) result;
}


HELPER void helper_finalize_named_type
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_release(library, node->specifics.named_type.content_type);
}


modern *modern_node_make_universe_type
    (modern_library *library_in,
     uint64_t level)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = universe_type_modern_node_type;
    result->value_type = NULL;
    result->specifics.universe_type.level = level;
    
    return (modern *) result;
}


modern *modern_node_make_lambda
    (modern_library *library_in,
     modern *content_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *content = (struct modern *) content_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_lambda;
    
    result->node_type = lambda_modern_node_type;
    result->value_type = NULL;
    result->specifics.lambda.content = content;
    
    return (modern *) result;
}


HELPER void helper_finalize_lambda
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_release(library, node->specifics.lambda.content);
}


modern *modern_node_make_apply
    (modern_library *library_in,
     modern *left_in, modern *right_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *left = (struct modern *) left_in;
    struct modern *right = (struct modern *) right_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_apply;
    
    result->node_type = apply_modern_node_type;
    result->value_type = NULL;
    result->specifics.apply.left = left;
    result->specifics.apply.right = right;
    
    return (modern *) result;
}


HELPER void helper_finalize_apply
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    modern_release(library, node->specifics.apply.left);
    modern_release(library, node->specifics.apply.right);
}


modern *modern_node_make_type_family
    (modern_library *library_in,
     uint64_t n_items, modern **types_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    size_t bytes = sizeof(modern **) * n_items;
    modern **copied_data = library->allocator->modern_allocator_alloc
        (library->client_state, bytes);
    if(!copied_data) {
        library->allocator->modern_allocator_free
            (library->client_state, result);
        library->error_handler->modern_error_handler_memory
            (library->client_state, bytes);
        return NULL;
    }
    memcpy(copied_data, types_in, bytes);
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_type_family;
    
    result->node_type = type_family_modern_node_type;
    result->value_type = NULL;
    result->specifics.type_family.n_items = n_items;
    result->specifics.type_family.members = (struct modern **) copied_data;
    
    for(size_t i = 0; i < n_items; i++) {
        modern_retain(library, types_in[i]);
    }
    
    return (modern *) result;
}


HELPER void helper_finalize_type_family
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    for(size_t i = 0; i < node->specifics.type_family.n_items; i++) {
        modern_release(library, node->specifics.type_family.members[i]);
    }
    
    library->allocator->modern_allocator_free
        (library->client_state, node->specifics.type_family.members);
}


modern *modern_node_make_let
    (modern_library *library_in,
     uint64_t n_items, modern **values_in, modern *content_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    size_t bytes = sizeof(modern **) * n_items;
    modern **copied_data = library->allocator->modern_allocator_alloc
        (library->client_state, bytes);
    if(!copied_data) {
        library->allocator->modern_allocator_free
            (library->client_state, result);
        library->error_handler->modern_error_handler_memory
            (library->client_state, bytes);
        return NULL;
    }
    memcpy(copied_data, values_in, bytes);
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = helper_finalize_let;
    
    result->node_type = type_family_modern_node_type;
    result->value_type = NULL;
    result->specifics.let.n_items = n_items;
    result->specifics.let.members = (struct modern **) copied_data;
    result->specifics.let.content = (struct modern *) content_in;
    
    for(size_t i = 0; i < n_items; i++) {
        modern_retain(library, values_in[i]);
    }
    
    modern_retain(library, content_in);
    
    return (modern *) result;
}


HELPER void helper_finalize_let
  (struct modern_library *library, void *retainable)
{
    struct modern *node = (struct modern *) retainable;
    
    for(size_t i = 0; i < node->specifics.let.n_items; i++) {
        modern_release(library, node->specifics.let.members[i]);
    }
    
    modern_release(library, node->specifics.let.content);
    
    library->allocator->modern_allocator_free
        (library->client_state, node->specifics.type_family.members);
}


modern *modern_node_make_backreference
    (modern_library *library_in,
     uint64_t index)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = backreference_modern_node_type;
    result->value_type = NULL;
    result->specifics.backreference = index;
    
    return (modern *) result;
}


modern *modern_node_make_builtin
    (modern_library *library_in,
     uint16_t identifier)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t result_size = sizeof(struct modern);
    struct modern *result =
        library->allocator->modern_allocator_alloc
            (library->client_state, result_size);
    if(!result) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, result_size);
        return NULL;
    }
    
    result->memory.retain_count = 1;
    result->memory.is_autoreleased = 0;
    result->memory.finalizer = NULL;
    
    result->node_type = backreference_modern_node_type;
    result->value_type = NULL;
    result->specifics.builtin = identifier;
    
    return (modern *) result;
}
