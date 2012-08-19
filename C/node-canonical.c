#include <math.h>
#include <string.h>
#include "modern.h"
#include "internal.h"


struct helper_byte_buffer {
    size_t count;
    size_t capacity;
    uint8_t *data;
};


struct helper_byte_buffer_buffer {
    size_t count;
    size_t capacity;
    struct helper_byte_buffer **byte_buffers;
};


struct helper_node_buffer {
    size_t count;
    size_t capacity;
    struct modern **nodes;
};


struct helper_node_cons {
    struct modern *node;
    struct helper_node_cons *next;
};


HELPER int helper_byte_buffer_alloc
  (struct modern_library *library,
   struct helper_byte_buffer **out);
HELPER int helper_byte_buffer_free
  (struct modern_library *library,
   struct helper_byte_buffer *buffer);
HELPER int helper_byte_buffer_append
  (struct modern_library *library,
   struct helper_byte_buffer *buffer,
   uint8_t *data,
   size_t length);

HELPER int helper_byte_buffer_buffer_alloc
  (struct modern_library *library,
   struct helper_byte_buffer_buffer **out);
HELPER int helper_byte_buffer_buffer_free
  (struct modern_library *library,
   struct helper_byte_buffer_buffer *buffer);
HELPER int helper_byte_buffer_buffer_append
  (struct modern_library *library,
   struct helper_byte_buffer_buffer *buffer,
   struct helper_byte_buffer *subbuffer);

HELPER int helper_node_buffer_alloc
  (struct modern_library *library,
   struct helper_node_buffer **out);
HELPER int helper_node_buffer_free
  (struct modern_library *library,
   struct helper_node_buffer *buffer);
HELPER int helper_node_buffer_append
  (struct modern_library *library,
   struct helper_node_buffer *buffer,
   struct modern *node);

HELPER int helper_node_list_free
  (struct modern_library *library,
   struct helper_node_cons **cons);
HELPER int helper_node_list_push
  (struct modern_library *library,
   struct helper_node_cons **list,
   struct modern *node);
HELPER int helper_node_list_pop
  (struct modern_library *library,
   struct helper_node_cons **list);

HELPER int helper_visit_node
  (struct modern_library *library,
   struct helper_node_buffer *visited_nodes,
   struct helper_byte_buffer_buffer *canonical_forms,
   struct helper_node_buffer *top_level_nodes,
   struct helper_node_cons **visit_stack,
   struct helper_node_cons **evaluation_stack,
   struct modern *node,
   struct helper_byte_buffer **out);


HELPER int helper_byte_buffer_alloc
  (struct modern_library *library,
   struct helper_byte_buffer **out)
{
    size_t buffer_size = sizeof(struct helper_byte_buffer);
    struct helper_byte_buffer *buffer;
    buffer = library->allocator->modern_allocator_alloc(buffer_size);
    if(!buffer) {
        library->error_handler->modern_error_handler_memory(buffer_size);
        return 0;
    }
    buffer->count = 0;
    buffer->capacity = 8;
    size_t data_size = (sizeof(uint8_t) * buffer->capacity);
    buffer->data = library->allocator->modern_allocator_alloc(data_size);
    if(!buffer->data) {
        library->allocator->modern_allocator_free(buffer);
        library->error_handler->modern_error_handler_memory(data_size);
        return 0;
    }
    *out = buffer;
    return 1;
}


HELPER int helper_byte_buffer_free
  (struct modern_library *library,
   struct helper_byte_buffer *buffer)
{
    library->allocator->modern_allocator_free(buffer->data);
    library->allocator->modern_allocator_free(buffer);
    return 1;
}


HELPER int helper_byte_buffer_append
  (struct modern_library *library,
   struct helper_byte_buffer *buffer,
   uint8_t *data,
   size_t length)
{
    while(buffer->count + length >= buffer->capacity) {
        buffer->capacity *= 2;
        size_t data_size = sizeof(uint8_t) * buffer->capacity;
        uint8_t *new_data =
            library->allocator->modern_allocator_realloc
                (buffer->data, data_size);
        if(!new_data) {
            library->error_handler->modern_error_handler_memory(data_size);
            return 0;
        }
        buffer->data = new_data;
    }
    memcpy(buffer->data + buffer->count, data, sizeof(uint8_t) * length);
    buffer->count += length;
    return 1;
}


HELPER int helper_byte_buffer_buffer_alloc
  (struct modern_library *library,
   struct helper_byte_buffer_buffer **out)
{
    size_t buffer_size = sizeof(struct helper_byte_buffer_buffer);
    struct helper_byte_buffer_buffer *buffer;
    buffer = library->allocator->modern_allocator_alloc(buffer_size);
    if(!buffer) {
        library->error_handler->modern_error_handler_memory(buffer_size);
        return 0;
    }
    buffer->count = 0;
    buffer->capacity = 8;
    size_t byte_buffers_size =
        (sizeof(struct helper_byte_buffer *) * buffer->capacity);
    buffer->byte_buffers =
        library->allocator->modern_allocator_alloc(byte_buffers_size);
    if(!buffer->byte_buffers) {
        library->allocator->modern_allocator_free(buffer);
        library->error_handler->modern_error_handler_memory(byte_buffers_size);
        return 0;
    }
    *out = buffer;
    return 1;
}


HELPER int helper_byte_buffer_buffer_free
  (struct modern_library *library,
   struct helper_byte_buffer_buffer *buffer)
{
    int result = 1;
    
    for(size_t i = 0; i < buffer->count; i++) {
        if(!helper_byte_buffer_free(library, buffer->byte_buffers[i]))
            result = 0;
    }
    
    library->allocator->modern_allocator_free(buffer->byte_buffers);
    library->allocator->modern_allocator_free(buffer);
    
    return result;
}


HELPER int helper_byte_buffer_buffer_append
  (struct modern_library *library,
   struct helper_byte_buffer_buffer *buffer,
   struct helper_byte_buffer *subbuffer)
{
    while(buffer->count + 1 >= buffer->capacity) {
        buffer->capacity *= 2;
        size_t byte_buffers_size =
            sizeof(struct helper_byte_buffer *) * buffer->capacity;
        struct helper_byte_buffer **new_byte_buffers =
            library->allocator->modern_allocator_realloc
                (buffer->byte_buffers, byte_buffers_size);
        if(!new_byte_buffers) {
            library->error_handler->modern_error_handler_memory
                (byte_buffers_size);
            return 0;
        }
        buffer->byte_buffers = new_byte_buffers;
    }
    buffer->byte_buffers[buffer->count] = subbuffer;
    buffer->count++;
    return 1;
}


HELPER int helper_node_buffer_alloc
  (struct modern_library *library,
   struct helper_node_buffer **out)
{
    size_t buffer_size = sizeof(struct helper_node_buffer);
    struct helper_node_buffer *buffer;
    buffer = library->allocator->modern_allocator_alloc(buffer_size);
    if(!buffer) {
        library->error_handler->modern_error_handler_memory(buffer_size);
        return 0;
    }
    buffer->count = 0;
    buffer->capacity = 128;
    size_t nodes_size = (sizeof(struct modern *) * buffer->capacity);
    buffer->nodes = library->allocator->modern_allocator_alloc(nodes_size);
    if(!buffer->nodes) {
        library->allocator->modern_allocator_free(buffer);
        library->error_handler->modern_error_handler_memory(nodes_size);
        return 0;
    }
    *out = buffer;
    return 1;
}


HELPER int helper_node_buffer_free
  (struct modern_library *library,
   struct helper_node_buffer *buffer)
{
    library->allocator->modern_allocator_free(buffer->nodes);
    library->allocator->modern_allocator_free(buffer);
    return 1;
}


HELPER int helper_node_buffer_append
  (struct modern_library *library,
   struct helper_node_buffer *buffer,
   struct modern *node)
{
    while(buffer->count + 1 >= buffer->capacity) {
        buffer->capacity *= 2;
        size_t nodes_size =
            sizeof(struct modern *) * buffer->capacity;
        struct modern **new_nodes =
            library->allocator->modern_allocator_realloc
                (buffer->nodes, nodes_size);
        if(!new_nodes) {
            library->error_handler->modern_error_handler_memory(nodes_size);
            return 0;
        }
        buffer->nodes = new_nodes;
    }
    buffer->nodes[buffer->count] = node;
    buffer->count++;
    return 1;
}


HELPER int helper_node_list_free
  (struct modern_library *library,
   struct helper_node_cons **cons)
{
    int result = 1;
    if(*cons) {
        if(!helper_node_list_free(library, &(*cons)->next)) result = 0;
        library->allocator->modern_allocator_free(*cons);
        *cons = NULL;
    }
    return result;
}


HELPER int helper_node_list_push
  (struct modern_library *library,
   struct helper_node_cons **list,
   struct modern *node)
{
    size_t cons_size = sizeof(struct helper_node_cons);
    struct helper_node_cons *new_head =
        library->allocator->modern_allocator_alloc(cons_size);
    if(!new_head) {
        library->error_handler->modern_error_handler_memory(cons_size);
        return 0;
    }
    new_head->node = node;
    new_head->next = *list;
    
    *list = new_head;
    
    return 0;
}


HELPER int helper_node_list_pop
  (struct modern_library *library,
   struct helper_node_cons **list)
{
    struct helper_node_cons *old_head = *list;
    if(old_head) {
        struct helper_node_cons *new_head = old_head->next;
        library->allocator->modern_allocator_free(old_head);
        *list = new_head;
    }
    return 1;
}


HELPER int helper_visit_node
  (struct modern_library *library,
   struct helper_node_buffer *visited_nodes,
   struct helper_byte_buffer_buffer *canonical_forms,
   struct helper_node_buffer *top_level_nodes,
   struct helper_node_cons **visit_stack,
   struct helper_node_cons **evaluation_stack,
   struct modern *node,
   struct helper_byte_buffer **out)
{
    size_t visited_nodes_index;
    for(visited_nodes_index = 0;
        visited_nodes_index < visited_nodes->count;
        visited_nodes_index++)
    {
        if(visited_nodes->nodes[visited_nodes_index] == node) break;
    }
    
    if(visited_nodes_index < visited_nodes->count) {
    }
    
    if(!helper_node_buffer_append(library, visited_nodes, node))
        return 0;
    
    struct helper_byte_buffer *canonical_form = NULL;
    if(!helper_byte_buffer_alloc(library, &canonical_form))
        return 0;
    
    switch(node->node_type) {
    case int8_value_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        temporary[1] = node->specifics.int8_value;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case int16_value_modern_node_type:
    {
        uint8_t temporary[3];
        
        temporary[0] = int16_value_modern_node_type;
        temporary[1] = ((node->specifics.int16_value >> 0) & 0xFF);
        temporary[2] = ((node->specifics.int16_value >> 8) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case int32_value_modern_node_type:
    {
        uint8_t temporary[5];
        
        temporary[0] = int32_value_modern_node_type;
        temporary[1] = ((node->specifics.int32_value >> 0) & 0xFF);
        temporary[2] = ((node->specifics.int32_value >> 8) & 0xFF);
        temporary[3] = ((node->specifics.int32_value >> 16) & 0xFF);
        temporary[4] = ((node->specifics.int32_value >> 24) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case int64_value_modern_node_type:
    {
        uint8_t temporary[9];
        
        temporary[0] = int64_value_modern_node_type;
        temporary[1] = ((node->specifics.int64_value >> 0) & 0xFF);
        temporary[2] = ((node->specifics.int64_value >> 8) & 0xFF);
        temporary[3] = ((node->specifics.int64_value >> 16) & 0xFF);
        temporary[4] = ((node->specifics.int64_value >> 24) & 0xFF);
        temporary[1] = ((node->specifics.int64_value >> 32) & 0xFF);
        temporary[2] = ((node->specifics.int64_value >> 40) & 0xFF);
        temporary[3] = ((node->specifics.int64_value >> 48) & 0xFF);
        temporary[4] = ((node->specifics.int64_value >> 56) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case nat8_value_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = nat8_value_modern_node_type;
        temporary[1] = node->specifics.nat8_value;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case nat16_value_modern_node_type:
    {
        uint8_t temporary[3];
        
        temporary[0] = nat16_value_modern_node_type;
        temporary[1] = ((node->specifics.nat16_value >> 0) & 0xFF);
        temporary[2] = ((node->specifics.nat16_value >> 8) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case nat32_value_modern_node_type:
    {
        uint8_t temporary[5];
        
        temporary[0] = nat32_value_modern_node_type;
        temporary[1] = ((node->specifics.nat32_value >> 0) & 0xFF);
        temporary[2] = ((node->specifics.nat32_value >> 8) & 0xFF);
        temporary[3] = ((node->specifics.nat32_value >> 16) & 0xFF);
        temporary[4] = ((node->specifics.nat32_value >> 24) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case nat64_value_modern_node_type:
    {
        uint8_t temporary[9];
        
        temporary[0] = nat64_value_modern_node_type;
        temporary[1] = ((node->specifics.nat64_value >> 0) & 0xFF);
        temporary[2] = ((node->specifics.nat64_value >> 8) & 0xFF);
        temporary[3] = ((node->specifics.nat64_value >> 16) & 0xFF);
        temporary[4] = ((node->specifics.nat64_value >> 24) & 0xFF);
        temporary[5] = ((node->specifics.nat64_value >> 32) & 0xFF);
        temporary[6] = ((node->specifics.nat64_value >> 40) & 0xFF);
        temporary[7] = ((node->specifics.nat64_value >> 48) & 0xFF);
        temporary[8] = ((node->specifics.nat64_value >> 56) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case float32_value_modern_node_type:
    {
        uint8_t temporary[5];
        
        temporary[0] = float32_value_modern_node_type;
        temporary[1] = 0x00;
        temporary[2] = 0x00;
        temporary[3] = 0x00;
        temporary[4] = 0x00;
        
        switch(fpclassify(node->specifics.float32_value)) {
        case FP_INFINITE:
        case FP_NAN:
        case FP_ZERO:
            break;
        case FP_NORMAL:
        case FP_SUBNORMAL:
        {
            if(signbit(node->specifics.float32_value)) {
                temporary[1] |= 0x80;
            }
            float value = node->specifics.float32_value;
            int exponent;
            float mantissa = fabsf(frexpf(value, &exponent));
            exponent += 126;
            temporary[1] |= ((exponent >> 1) & 0x7F);
            temporary[2] |= ((exponent << 7) & 0x80);
            temporary[2] |= ((uint8_t) (mantissa * (1 << 8)) & 0x7F);
            temporary[3] |= (uint8_t) (mantissa * (1 << 16));
            temporary[4] |= (uint8_t) (mantissa * (1 << 24));
            break;
        }
        }
                
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case float64_value_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case float128_value_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case utf8_value_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case blob_value_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case sigma_value_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case named_value_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case int8_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case int16_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case int32_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case int64_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case nat8_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case nat16_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case nat32_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case nat64_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case float32_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case float64_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case float128_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case utf8_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case blob_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case sigma_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case named_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case universe_type_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case lambda_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case apply_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    
    case family_modern_node_type:
    {
        uint8_t temporary[2];
        
        temporary[0] = int8_value_modern_node_type;
        // TODO
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
        {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        break;
    }
    }
    
    *out = canonical_form;
    return 1;
}


void modern_node_canonical_hash
  (modern_library *library_in,
   modern *value_in,
   struct modern_hash *out)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    
    struct helper_node_buffer *visited_nodes = NULL;
    if(!helper_node_buffer_alloc(library, &visited_nodes)) {
        return;
    }
    
    struct helper_byte_buffer_buffer *canonical_forms = NULL;
    if(!helper_byte_buffer_buffer_alloc(library, &canonical_forms))
    {
        helper_node_buffer_free(library, visited_nodes);
        return;
    }
    
    struct helper_node_buffer *top_level_nodes = NULL;
    if(!helper_node_buffer_alloc(library, &top_level_nodes)) {
        helper_byte_buffer_buffer_free(library, canonical_forms);
        helper_node_buffer_free(library, visited_nodes);
        return;
    }
    
    struct helper_node_cons *visit_stack = NULL;
    struct helper_node_cons *evaluation_stack = NULL;
    
    struct helper_byte_buffer *canonical_form = NULL;
    
    if(!helper_visit_node(library, visited_nodes, canonical_forms,
                          top_level_nodes, &visit_stack, &evaluation_stack,
                          value, &canonical_form))
    {
        helper_byte_buffer_buffer_free(library, canonical_forms);
        helper_node_list_free(library, &evaluation_stack);
        helper_node_list_free(library, &visit_stack);
        helper_node_buffer_free(library, top_level_nodes);
        helper_node_buffer_free(library, visited_nodes);
        return;
    }
    
    for(size_t i = 0; i < canonical_form->count; i++) {
    	printf(" %02x", canonical_form->data[i]);
    }
    printf("\n");
    
    modern_compute_hash(canonical_form->data, canonical_form->count, out);
    
    helper_byte_buffer_buffer_free(library, canonical_forms);
    helper_node_list_free(library, &evaluation_stack);
    helper_node_list_free(library, &visit_stack);
    helper_node_buffer_free(library, top_level_nodes);
    helper_node_buffer_free(library, visited_nodes);
}
