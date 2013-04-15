#include <math.h>
#include <string.h>
#include "modern.h"
#include "internal.h"


struct helper_byte_buffer {
    size_t count;
    size_t capacity;
    uint8_t *data;
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

HELPER int helper_visit_node_top_level
  (struct modern_library *library,
   struct helper_byte_buffer *canonical_form,
   struct modern *node);
HELPER int helper_visit_node_interior
  (struct modern_library *library,
   struct helper_byte_buffer *canonical_form,
   struct modern *node);


HELPER int helper_byte_buffer_alloc
  (struct modern_library *library,
   struct helper_byte_buffer **out)
{
    size_t buffer_size = sizeof(struct helper_byte_buffer);
    struct helper_byte_buffer *buffer;
    buffer = library->allocator->modern_allocator_alloc
        (library->client_state, buffer_size);
    if(!buffer) {
        library->error_handler->modern_error_handler_memory
            (library->client_state, buffer_size);
        return 0;
    }
    buffer->count = 0;
    buffer->capacity = 8;
    size_t data_size = (sizeof(uint8_t) * buffer->capacity);
    buffer->data = library->allocator->modern_allocator_alloc
        (library->client_state, data_size);
    if(!buffer->data) {
        library->allocator->modern_allocator_free
            (library->client_state, buffer);
        library->error_handler->modern_error_handler_memory
            (library->client_state, data_size);
        return 0;
    }
    *out = buffer;
    return 1;
}


HELPER int helper_byte_buffer_free
  (struct modern_library *library,
   struct helper_byte_buffer *buffer)
{
    library->allocator->modern_allocator_free
        (library->client_state, buffer->data);
    library->allocator->modern_allocator_free
        (library->client_state, buffer);
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
                (library->client_state, buffer->data, data_size);
        if(!new_data) {
            library->error_handler->modern_error_handler_memory
                (library->client_state, data_size);
            return 0;
        }
        buffer->data = new_data;
    }
    memcpy(buffer->data + buffer->count, data, sizeof(uint8_t) * length);
    buffer->count += length;
    return 1;
}


HELPER int helper_visit_node_top_level
  (struct modern_library *library,
   struct helper_byte_buffer *canonical_form,
   struct modern *node)
{
    switch(node->node_type) {
    case modern_node_type_bool_value_false:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_bool_value_false;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_bool_value_true:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_bool_value_true;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_ordering_value_less:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_ordering_value_less;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_ordering_value_equal:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_ordering_value_equal;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_ordering_value_greater:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_ordering_value_greater;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_maybe_value_nothing:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_maybe_value_nothing;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_maybe_value_just:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_maybe_value_just;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        if(!helper_visit_node_interior(library, canonical_form,
                                       node->specifics.maybe_value.content_value))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_int8_value:
    {
        uint8_t temporary[2];
        
        temporary[0] = modern_node_type_int8_value;
        temporary[1] = node->specifics.int8_value;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_int16_value:
    {
        uint8_t temporary[3];
        
        temporary[0] = modern_node_type_int16_value;
        temporary[1] = ((node->specifics.int16_value >> 0) & 0xFF);
        temporary[2] = ((node->specifics.int16_value >> 8) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_int32_value:
    {
        uint8_t temporary[5];
        
        temporary[0] = modern_node_type_int32_value;
        temporary[1] = ((node->specifics.int32_value >> 0) & 0xFF);
        temporary[2] = ((node->specifics.int32_value >> 8) & 0xFF);
        temporary[3] = ((node->specifics.int32_value >> 16) & 0xFF);
        temporary[4] = ((node->specifics.int32_value >> 24) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_int64_value:
    {
        uint8_t temporary[9];
        
        temporary[0] = modern_node_type_int64_value;
        temporary[1] = ((node->specifics.int64_value >> 0) & 0xFF);
        temporary[2] = ((node->specifics.int64_value >> 8) & 0xFF);
        temporary[3] = ((node->specifics.int64_value >> 16) & 0xFF);
        temporary[4] = ((node->specifics.int64_value >> 24) & 0xFF);
        temporary[5] = ((node->specifics.int64_value >> 32) & 0xFF);
        temporary[6] = ((node->specifics.int64_value >> 40) & 0xFF);
        temporary[7] = ((node->specifics.int64_value >> 48) & 0xFF);
        temporary[8] = ((node->specifics.int64_value >> 56) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_nat8_value:
    {
        uint8_t temporary[2];
        
        temporary[0] = modern_node_type_nat8_value;
        temporary[1] = node->specifics.nat8_value;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_nat16_value:
    {
        uint8_t temporary[3];
        
        temporary[0] = modern_node_type_nat16_value;
        temporary[1] = ((node->specifics.nat16_value >> 0) & 0xFF);
        temporary[2] = ((node->specifics.nat16_value >> 8) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_nat32_value:
    {
        uint8_t temporary[5];
        
        temporary[0] = modern_node_type_nat32_value;
        temporary[1] = ((node->specifics.nat32_value >> 0) & 0xFF);
        temporary[2] = ((node->specifics.nat32_value >> 8) & 0xFF);
        temporary[3] = ((node->specifics.nat32_value >> 16) & 0xFF);
        temporary[4] = ((node->specifics.nat32_value >> 24) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_nat64_value:
    {
        uint8_t temporary[9];
        
        temporary[0] = modern_node_type_nat64_value;
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
            return 0;
        
        return 1;
    }
    
    case modern_node_type_float32_value:
    {
        uint8_t temporary[5];
        
        temporary[0] = modern_node_type_float32_value;
        temporary[1] = 0x00;
        temporary[2] = 0x00;
        temporary[3] = 0x00;
        temporary[4] = 0x00;
        
        switch(fpclassify(node->specifics.float32_value)) {
        case FP_INFINITE:
        case FP_NAN:
        case FP_ZERO:
            return 1;
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
            temporary[2] |= (((uint8_t) fmodf(ldexpf(mantissa, 8), 256.0)) & 0x7F);
            temporary[3] |= (uint8_t) fmodf(ldexpf(mantissa, 16), 256.0);
            temporary[4] |= (uint8_t) fmodf(ldexpf(mantissa, 24), 256.0);
            break;
        }
        }
                
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_float64_value:
    {
        uint8_t temporary[9];
        
        temporary[0] = modern_node_type_float64_value;
        temporary[1] = 0x00;
        temporary[2] = 0x00;
        temporary[3] = 0x00;
        temporary[4] = 0x00;
        temporary[5] = 0x00;
        temporary[6] = 0x00;
        temporary[7] = 0x00;
        temporary[8] = 0x00;
        
        switch(fpclassify(node->specifics.float64_value)) {
        case FP_INFINITE:
        case FP_NAN:
        case FP_ZERO:
            break;
        case FP_NORMAL:
        case FP_SUBNORMAL:
        {
            if(signbit(node->specifics.float64_value)) {
                temporary[1] |= 0x80;
            }
            double value = node->specifics.float64_value;
            int exponent;
            double mantissa = fabs(frexp(value, &exponent));
            exponent += 1022;
            temporary[1] |= ((exponent >> 4) & 0x7F);
            temporary[2] |= ((exponent << 4) & 0xF0);
            temporary[2] |= (((uint8_t) ldexp(mantissa, 5)) & 0x0F);
            temporary[3] |= (uint8_t) fmod(ldexp(mantissa, 13), 256.0);
            temporary[4] |= (uint8_t) fmod(ldexp(mantissa, 21), 256.0);
            temporary[5] |= (uint8_t) fmod(ldexp(mantissa, 29), 256.0);
            temporary[6] |= (uint8_t) fmod(ldexp(mantissa, 37), 256.0);
            temporary[7] |= (uint8_t) fmod(ldexp(mantissa, 45), 256.0);
            temporary[8] |= (uint8_t) fmod(ldexp(mantissa, 52), 256.0);
            break;
        }
        }
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_utf8_value:
    {
        uint8_t temporary[9];
        
        temporary[0] = modern_node_type_utf8_value;
        temporary[1] = ((node->specifics.utf8_value.bytes >> 0) & 0xFF);
        temporary[2] = ((node->specifics.utf8_value.bytes >> 8) & 0xFF);
        temporary[3] = ((node->specifics.utf8_value.bytes >> 16) & 0xFF);
        temporary[4] = ((node->specifics.utf8_value.bytes >> 24) & 0xFF);
        temporary[5] = ((node->specifics.utf8_value.bytes >> 32) & 0xFF);
        temporary[6] = ((node->specifics.utf8_value.bytes >> 40) & 0xFF);
        temporary[7] = ((node->specifics.utf8_value.bytes >> 48) & 0xFF);
        temporary[8] = ((node->specifics.utf8_value.bytes >> 56) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        if(!helper_byte_buffer_append(library, canonical_form,
                                      node->specifics.utf8_value.data,
                                      node->specifics.utf8_value.bytes))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_blob_value:
    {
        uint8_t temporary[9];
        
        temporary[0] = modern_node_type_blob_value;
        temporary[1] = ((node->specifics.blob_value.bytes >> 0) & 0xFF);
        temporary[2] = ((node->specifics.blob_value.bytes >> 8) & 0xFF);
        temporary[3] = ((node->specifics.blob_value.bytes >> 16) & 0xFF);
        temporary[4] = ((node->specifics.blob_value.bytes >> 24) & 0xFF);
        temporary[5] = ((node->specifics.blob_value.bytes >> 32) & 0xFF);
        temporary[6] = ((node->specifics.blob_value.bytes >> 40) & 0xFF);
        temporary[7] = ((node->specifics.blob_value.bytes >> 48) & 0xFF);
        temporary[8] = ((node->specifics.blob_value.bytes >> 56) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        if(!helper_byte_buffer_append(library, canonical_form,
                                      node->specifics.blob_value.data,
                                      node->specifics.blob_value.bytes))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_sigma_value:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_sigma_value;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        if(!helper_visit_node_interior
            (library, canonical_form, node->specifics.sigma_value.field_value))
            return 0;
        
        if(!helper_visit_node_interior
            (library, canonical_form, node->specifics.sigma_value.successor))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_name_value:
    {
        uint8_t temporary[17];
        
        temporary[0] = modern_node_type_name_value;
        temporary[1] = ((node->specifics.name_value.hash.a >> 0) & 0xFF);
        temporary[2] = ((node->specifics.name_value.hash.a >> 8) & 0xFF);
        temporary[3] = ((node->specifics.name_value.hash.a >> 16) & 0xFF);
        temporary[4] = ((node->specifics.name_value.hash.a >> 24) & 0xFF);
        temporary[5] = ((node->specifics.name_value.hash.a >> 32) & 0xFF);
        temporary[6] = ((node->specifics.name_value.hash.a >> 40) & 0xFF);
        temporary[7] = ((node->specifics.name_value.hash.a >> 48) & 0xFF);
        temporary[8] = ((node->specifics.name_value.hash.a >> 56) & 0xFF);
        temporary[9] = ((node->specifics.name_value.hash.b >> 0) & 0xFF);
        temporary[10] = ((node->specifics.name_value.hash.b >> 8) & 0xFF);
        temporary[11] = ((node->specifics.name_value.hash.b >> 16) & 0xFF);
        temporary[12] = ((node->specifics.name_value.hash.b >> 24) & 0xFF);
        temporary[13] = ((node->specifics.name_value.hash.b >> 32) & 0xFF);
        temporary[14] = ((node->specifics.name_value.hash.b >> 40) & 0xFF);
        temporary[15] = ((node->specifics.name_value.hash.b >> 48) & 0xFF);
        temporary[16] = ((node->specifics.name_value.hash.b >> 56) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_named_value:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_named_value;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        if(!helper_visit_node_interior
            (library, canonical_form, node->specifics.named_value.value))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_bool_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_bool_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_ordering_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_ordering_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_maybe_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_maybe_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        if(!helper_visit_node_interior
            (library, canonical_form, node->specifics.maybe_type.content_type))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_int8_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_int8_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_int16_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_int16_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_int32_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_int32_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_int64_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_int64_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_nat8_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_nat8_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_nat16_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_nat16_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_nat32_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_nat32_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_nat64_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_nat64_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_float32_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_float32_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_float64_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_float64_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_utf8_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_utf8_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_blob_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_blob_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_function_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_function_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        if(!helper_visit_node_interior
            (library, canonical_form, node->specifics.function_type.left))
            return 0;
        
        if(!helper_visit_node_interior
            (library, canonical_form, node->specifics.function_type.right))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_sigma_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_sigma_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        if(!helper_visit_node_interior
            (library, canonical_form, node->specifics.sigma_type.field_type))
            return 0;
        
        if(!helper_visit_node_interior
            (library, canonical_form, node->specifics.sigma_type.successor))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_name_type:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_name_type;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_named_type:
    {
        uint8_t temporary[17];
        
        temporary[0] = modern_node_type_named_type;
        temporary[1] = ((node->specifics.named_type.name.a >> 0) & 0xFF);
        temporary[2] = ((node->specifics.named_type.name.a >> 8) & 0xFF);
        temporary[3] = ((node->specifics.named_type.name.a >> 16) & 0xFF);
        temporary[4] = ((node->specifics.named_type.name.a >> 24) & 0xFF);
        temporary[5] = ((node->specifics.named_type.name.a >> 32) & 0xFF);
        temporary[6] = ((node->specifics.named_type.name.a >> 40) & 0xFF);
        temporary[7] = ((node->specifics.named_type.name.a >> 48) & 0xFF);
        temporary[8] = ((node->specifics.named_type.name.a >> 56) & 0xFF);
        temporary[9] = ((node->specifics.named_type.name.b >> 0) & 0xFF);
        temporary[10] = ((node->specifics.named_type.name.b >> 8) & 0xFF);
        temporary[11] = ((node->specifics.named_type.name.b >> 16) & 0xFF);
        temporary[12] = ((node->specifics.named_type.name.b >> 24) & 0xFF);
        temporary[13] = ((node->specifics.named_type.name.b >> 32) & 0xFF);
        temporary[14] = ((node->specifics.named_type.name.b >> 40) & 0xFF);
        temporary[15] = ((node->specifics.named_type.name.b >> 48) & 0xFF);
        temporary[16] = ((node->specifics.named_type.name.b >> 56) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        if(!helper_visit_node_interior
            (library, canonical_form, node->specifics.named_type.content_type))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_universe_type:
    {
        uint8_t temporary[9];
        
        temporary[0] = modern_node_type_universe_type;
        temporary[1] = ((node->specifics.universe_type.level >> 0) & 0xFF);
        temporary[2] = ((node->specifics.universe_type.level >> 8) & 0xFF);
        temporary[3] = ((node->specifics.universe_type.level >> 16) & 0xFF);
        temporary[4] = ((node->specifics.universe_type.level >> 24) & 0xFF);
        temporary[5] = ((node->specifics.universe_type.level >> 32) & 0xFF);
        temporary[6] = ((node->specifics.universe_type.level >> 40) & 0xFF);
        temporary[7] = ((node->specifics.universe_type.level >> 48) & 0xFF);
        temporary[8] = ((node->specifics.universe_type.level >> 56) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_lambda:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_lambda;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        if(!helper_visit_node_interior
            (library, canonical_form, node->specifics.lambda.content))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_apply:
    {
        uint8_t temporary[1];
        
        temporary[0] = modern_node_type_apply;
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        if(!helper_visit_node_interior
            (library, canonical_form, node->specifics.apply.left))
            return 0;
        
        if(!helper_visit_node_interior
            (library, canonical_form, node->specifics.apply.right))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_type_family:
    {
        uint8_t temporary[9];
        
        temporary[0] = modern_node_type_type_family;
        temporary[1] = ((node->specifics.type_family.n_items >> 0) & 0xFF);
        temporary[2] = ((node->specifics.type_family.n_items >> 8) & 0xFF);
        temporary[3] = ((node->specifics.type_family.n_items >> 16) & 0xFF);
        temporary[4] = ((node->specifics.type_family.n_items >> 24) & 0xFF);
        temporary[5] = ((node->specifics.type_family.n_items >> 32) & 0xFF);
        temporary[6] = ((node->specifics.type_family.n_items >> 40) & 0xFF);
        temporary[7] = ((node->specifics.type_family.n_items >> 48) & 0xFF);
        temporary[8] = ((node->specifics.type_family.n_items >> 56) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        for(size_t i = 0; i < node->specifics.type_family.n_items; i++) {
            if(!helper_visit_node_interior
                (library, canonical_form, node->specifics.type_family.members[i]))
                return 0;
        }
        
        return 1;
    }
    
    case modern_node_type_let:
    {
        uint8_t temporary[9];
        
        temporary[0] = modern_node_type_let;
        temporary[1] = ((node->specifics.let.n_items >> 0) & 0xFF);
        temporary[2] = ((node->specifics.let.n_items >> 8) & 0xFF);
        temporary[3] = ((node->specifics.let.n_items >> 16) & 0xFF);
        temporary[4] = ((node->specifics.let.n_items >> 24) & 0xFF);
        temporary[5] = ((node->specifics.let.n_items >> 32) & 0xFF);
        temporary[6] = ((node->specifics.let.n_items >> 40) & 0xFF);
        temporary[7] = ((node->specifics.let.n_items >> 48) & 0xFF);
        temporary[8] = ((node->specifics.let.n_items >> 56) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        for(size_t i = 0; i < node->specifics.let.n_items; i++) {
            if(!helper_visit_node_interior
                (library, canonical_form, node->specifics.let.members[i]))
                return 0;
        }
        
        if(!helper_visit_node_interior
            (library, canonical_form, node->specifics.let.content))
            return 0;
        
        return 1;
    }
        
    case modern_node_type_backreference:
    {
        uint8_t temporary[9];
        
        temporary[0] = modern_node_type_backreference;
        temporary[1] = ((node->specifics.backreference.index >> 0) & 0xFF);
        temporary[2] = ((node->specifics.backreference.index >> 8) & 0xFF);
        temporary[3] = ((node->specifics.backreference.index >> 16) & 0xFF);
        temporary[4] = ((node->specifics.backreference.index >> 24) & 0xFF);
        temporary[5] = ((node->specifics.backreference.index >> 32) & 0xFF);
        temporary[6] = ((node->specifics.backreference.index >> 40) & 0xFF);
        temporary[7] = ((node->specifics.backreference.index >> 48) & 0xFF);
        temporary[8] = ((node->specifics.backreference.index >> 56) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }
    
    case modern_node_type_builtin:
    {
        uint8_t temporary[3];
        
        temporary[0] = modern_node_type_builtin;
        temporary[1] = ((node->specifics.builtin >> 0) & 0xFF);
        temporary[2] = ((node->specifics.builtin >> 8) & 0xFF);
        
        if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                      sizeof(temporary)))
            return 0;
        
        return 1;
    }

    default:
        return 0;
    }
}


HELPER int helper_visit_node_interior
  (struct modern_library *library,
   struct helper_byte_buffer *canonical_form,
   struct modern *node)
{
    struct modern_hash hash;
    
    if(!modern_node_canonical_hash_compute
        ((modern_library *) library, (modern *) node, &hash))
        return 0;
    
    uint8_t temporary[16];
    
    temporary[0] = ((hash.a >> 0) & 0xFF);
    temporary[1] = ((hash.a >> 8) & 0xFF);
    temporary[2] = ((hash.a >> 16) & 0xFF);
    temporary[3] = ((hash.a >> 24) & 0xFF);
    temporary[4] = ((hash.a >> 32) & 0xFF);
    temporary[5] = ((hash.a >> 40) & 0xFF);
    temporary[6] = ((hash.a >> 48) & 0xFF);
    temporary[7] = ((hash.a >> 56) & 0xFF);
    temporary[8] = ((hash.b >> 0) & 0xFF);
    temporary[9] = ((hash.b >> 8) & 0xFF);
    temporary[10] = ((hash.b >> 16) & 0xFF);
    temporary[11] = ((hash.b >> 24) & 0xFF);
    temporary[12] = ((hash.b >> 32) & 0xFF);
    temporary[13] = ((hash.b >> 40) & 0xFF);
    temporary[14] = ((hash.b >> 48) & 0xFF);
    temporary[15] = ((hash.b >> 56) & 0xFF);
    
    if(!helper_byte_buffer_append(library, canonical_form, temporary,
                                  sizeof(temporary)))
        return 0;
    
    return 1;
}


INTERNAL int modern_node_canonical_hash_compute
  (modern_library *library_in,
   modern *value_in,
   struct modern_hash *out)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    if(!library->node_representation) {
        library->error_handler->modern_error_handler_usage
            (library->client_state);
        return 0;
    }
    
    if(!library->node_representation
        ->modern_node_representation_canonical_hash_valid_get
        (library_in, value_in))
    {
        struct helper_byte_buffer *canonical_form = NULL;
        if(!helper_byte_buffer_alloc(library, &canonical_form)) {
            return 0;
        }
        
        if(!helper_visit_node_top_level(library, canonical_form, value_in)) {
            helper_byte_buffer_free(library, canonical_form);
            return 0;
        }
        
        for(size_t i = 0; i < canonical_form->count; i++) {
            printf(" %02x", canonical_form->data[i]);
        }
        printf("\n");
        
        struct modern_hash hash;
        modern_hash_compute
            (canonical_form->data, canonical_form->count, &hash);
        
        library->node_representation
            ->modern_node_representation_canonical_hash_set
            (library_in, value_in, hash);
        
        helper_byte_buffer_free(library, canonical_form);
    }
    
    struct modern_hash hash =
        library->node_representation
        ->modern_node_representation_canonical_hash_get
        (library_in, value_in);
    memcpy(out, &hash, sizeof(struct modern_hash));
    
    return 1;
}
