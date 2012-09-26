#include "modern.h"
#include "internal.h"


struct helper_target_stack_frame {
    size_t count;
    struct modern **members;
    struct helper_target_stack_frame *parent;
};


struct helper_parameter_stack_frame {
    struct modern *parameter;
    struct helper_parameter_stack_frame *parent;
};



HELPER struct helper_target_stack_frame *helper_allocate_target_stack_frame
  (struct modern_library *library, size_t count);
HELPER void helper_free_target_stack_frame
  (struct modern_library *library, struct helper_target_stack_frame *frame);

HELPER struct helper_parameter_stack_frame *helper_allocate_parameter_stack_frame
  (struct modern_library *library);
HELPER void helper_free_parameter_stack_frame
  (struct modern_library *library, struct helper_parameter_stack_frame *frame);

HELPER struct modern *helper_evaluate_node
  (struct modern_library *library,
   struct modern *node,
   struct helper_target_stack_frame *target_stack);
HELPER struct modern *helper_evaluate_application
  (struct modern_library *library,
   struct modern *left,
   struct helper_target_stack_frame *target_stack,
   struct helper_parameter_stack_frame *parameter_stack);

HELPER struct modern *helper_resolve_node
  (struct modern_library *library,
   struct modern *node,
   struct helper_target_stack_frame *target_stack);

HELPER uint64_t helper_builtin_static_arity(uint16_t index);
HELPER struct modern *helper_builtin_apply
  (struct modern_library *library, uint16_t index, struct modern **parameters);


HELPER struct helper_target_stack_frame *helper_allocate_target_stack_frame
  (struct modern_library *library, size_t count)
{
    struct helper_target_stack_frame *frame;
    {
        size_t size = sizeof(struct helper_target_stack_frame);
        frame = library->allocator->modern_allocator_alloc
            (library->client_state, size);
        if(!frame) {
            library->error_handler->modern_error_handler_memory
                (library->client_state, size);
            return NULL;
        }
    }
    
    {
        size_t size = sizeof(struct modern *) * count;
        frame->members =
            library->allocator->modern_allocator_alloc
                (library->client_state, size);
        if(!frame->members) {
            library->error_handler->modern_error_handler_memory
                (library->client_state, size);
            return NULL;
        }
    }
    
    frame->count = count;
    
    return frame;
}


HELPER void helper_free_target_stack_frame
  (struct modern_library *library, struct helper_target_stack_frame *frame)
{
    library->allocator->modern_allocator_free
        (library->client_state, frame->members);
    library->allocator->modern_allocator_free
        (library->client_state, frame);
}


HELPER struct helper_parameter_stack_frame *helper_allocate_parameter_stack_frame
  (struct modern_library *library)
{
    struct helper_parameter_stack_frame *frame;
    {
        size_t size = sizeof(struct helper_parameter_stack_frame);
        frame = library->allocator->modern_allocator_alloc
            (library->client_state, size);
        if(!frame) {
            library->error_handler->modern_error_handler_memory
                (library->client_state, size);
            return NULL;
        }
    }
    
    return frame;
}


HELPER void helper_free_parameter_stack_frame
  (struct modern_library *library, struct helper_parameter_stack_frame *frame)
{
    library->allocator->modern_allocator_free
        (library->client_state, frame);
}


HELPER struct modern *helper_evaluate_node
  (struct modern_library *library,
   struct modern *node,
   struct helper_target_stack_frame *target_stack)
{
    if(node->node_type == modern_node_type_backreference) {
        uint64_t index = node->specifics.backreference.index;
        struct helper_target_stack_frame *frame = target_stack;
        while(frame) {
            if(index < frame->count) return frame->members[index];
            index -= frame->count;
            frame = frame->parent;
        }
        return NULL;
    } else if(node->node_type == modern_node_type_apply) {
        struct helper_parameter_stack_frame *frame =
            helper_allocate_parameter_stack_frame(library);
        if(!frame) return NULL;
        
        frame->parameter = node->specifics.apply.right;
        frame->parent = NULL;
        
        struct modern *result = helper_evaluate_application
            (library,
             node->specifics.apply.left,
             target_stack,
             frame);
        
        helper_free_parameter_stack_frame(library, frame);
        
        return result;
    } else {
        return node;
    }
}


HELPER struct modern *helper_evaluate_application
  (struct modern_library *library,
   struct modern *left,
   struct helper_target_stack_frame *target_stack,
   struct helper_parameter_stack_frame *parameter_stack)
{
    struct modern *intermediate_result = left;
    while(1) {
        if(intermediate_result->node_type == modern_node_type_lambda) {
            if(!parameter_stack) break;
            
            struct modern *right = (struct modern *)
                helper_evaluate_node
                    (library, parameter_stack->parameter, target_stack);
            if(!right) return NULL;
            
            {
                struct helper_parameter_stack_frame *frame = parameter_stack;
                parameter_stack = frame->parent;
                helper_free_parameter_stack_frame(library, frame);
            }
            
            struct helper_target_stack_frame *target_stack_copy =
                helper_allocate_target_stack_frame(library, 1);
            if(!target_stack_copy) return 0;
            
            target_stack_copy->members[0] = right;
            target_stack_copy->parent = target_stack;
            
            intermediate_result = helper_evaluate_node
                (library, intermediate_result, target_stack_copy);
            if(!intermediate_result) return NULL;
            
            helper_free_target_stack_frame(library, target_stack_copy);
        } else if(intermediate_result->node_type == modern_node_type_apply) {
            struct helper_parameter_stack_frame *frame =
                helper_allocate_parameter_stack_frame(library);
            if(!frame) return NULL;
            
            frame->parameter = intermediate_result->specifics.apply.right;
            frame->parent = parameter_stack;
            
            parameter_stack = frame;
        } else if(intermediate_result->node_type == modern_node_type_builtin) {
            uint64_t static_arity = helper_builtin_static_arity
                (intermediate_result->specifics.builtin);
            
            struct modern *parameters[3];
            int okay = 1;
            {
                struct helper_parameter_stack_frame *parameter_point =
                    parameter_stack;
                
                for(uint64_t i = 0; i < static_arity; i++) {
                    if(!parameter_point) {
                        okay = 0;
                        break;
                    }
                    
                    parameters[i] = parameter_point->parameter;
                    parameter_point = parameter_point->parent;
                }
            }
            
            if(!okay) break;
            
            for(uint64_t i = 0; i < static_arity; i++) {
                struct helper_parameter_stack_frame *frame = parameter_stack;
                parameter_stack = frame->parent;
                helper_free_parameter_stack_frame(library, frame);        
            }
            
            intermediate_result = helper_builtin_apply
                (library, intermediate_result->specifics.builtin, parameters);
        } else {
            break;
        }
    }
    
    while(parameter_stack) {
        struct helper_parameter_stack_frame *frame = parameter_stack;
        intermediate_result = modern_node_make_apply
            (library, intermediate_result, frame->parameter);
        parameter_stack = frame->parent;
        helper_free_parameter_stack_frame(library, frame);        
    }
    
    return intermediate_result;
}


HELPER struct modern *helper_resolve_node
  (struct modern_library *library,
   struct modern *node,
   struct helper_target_stack_frame *target_stack)
{
    // TODO
    return NULL;
}


HELPER uint64_t helper_builtin_static_arity(uint16_t index) {
    switch(index) {
    case modern_builtin_identifier_if_bool: return 3;
    case modern_builtin_identifier_and_bool: return 2;
    case modern_builtin_identifier_or_bool: return 2;
    case modern_builtin_identifier_not_bool: return 1;
    case modern_builtin_identifier_equal_to_bool: return 2;
    case modern_builtin_identifier_equal_to_int8: return 2;
    case modern_builtin_identifier_equal_to_int16: return 2;
    case modern_builtin_identifier_equal_to_int32: return 2;
    case modern_builtin_identifier_equal_to_int64: return 2;
    case modern_builtin_identifier_equal_to_nat8: return 2;
    case modern_builtin_identifier_equal_to_nat16: return 2;
    case modern_builtin_identifier_equal_to_nat32: return 2;
    case modern_builtin_identifier_equal_to_nat64: return 2;
    case modern_builtin_identifier_equal_to_float32: return 2;
    case modern_builtin_identifier_equal_to_float64: return 2;
    case modern_builtin_identifier_equal_to_name: return 2;
    case modern_builtin_identifier_equal_to_utf8: return 2;
    case modern_builtin_identifier_equal_to_blob: return 2;
    case modern_builtin_identifier_equal_to_ordering: return 2;
    case modern_builtin_identifier_compare_int8: return 2;
    case modern_builtin_identifier_compare_int16: return 2;
    case modern_builtin_identifier_compare_int32: return 2;
    case modern_builtin_identifier_compare_int64: return 2;
    case modern_builtin_identifier_compare_nat8: return 2;
    case modern_builtin_identifier_compare_nat16: return 2;
    case modern_builtin_identifier_compare_nat32: return 2;
    case modern_builtin_identifier_compare_nat64: return 2;
    case modern_builtin_identifier_compare_float32: return 2;
    case modern_builtin_identifier_compare_float64: return 2;
    case modern_builtin_identifier_add_int8: return 2;
    case modern_builtin_identifier_add_int16: return 2;
    case modern_builtin_identifier_add_int32: return 2;
    case modern_builtin_identifier_add_int64: return 2;
    case modern_builtin_identifier_add_nat8: return 2;
    case modern_builtin_identifier_add_nat16: return 2;
    case modern_builtin_identifier_add_nat32: return 2;
    case modern_builtin_identifier_add_nat64: return 2;
    case modern_builtin_identifier_add_float32: return 2;
    case modern_builtin_identifier_add_float64: return 2;
    case modern_builtin_identifier_subtract_int8: return 2;
    case modern_builtin_identifier_subtract_int16: return 2;
    case modern_builtin_identifier_subtract_int32: return 2;
    case modern_builtin_identifier_subtract_int64: return 2;
    case modern_builtin_identifier_subtract_nat8: return 2;
    case modern_builtin_identifier_subtract_nat16: return 2;
    case modern_builtin_identifier_subtract_nat32: return 2;
    case modern_builtin_identifier_subtract_nat64: return 2;
    case modern_builtin_identifier_subtract_float32: return 2;
    case modern_builtin_identifier_subtract_float64: return 2;
    case modern_builtin_identifier_multiply_int8: return 2;
    case modern_builtin_identifier_multiply_int16: return 2;
    case modern_builtin_identifier_multiply_int32: return 2;
    case modern_builtin_identifier_multiply_int64: return 2;
    case modern_builtin_identifier_multiply_nat8: return 2;
    case modern_builtin_identifier_multiply_nat16: return 2;
    case modern_builtin_identifier_multiply_nat32: return 2;
    case modern_builtin_identifier_multiply_nat64: return 2;
    case modern_builtin_identifier_multiply_float32: return 2;
    case modern_builtin_identifier_multiply_float64: return 2;
    case modern_builtin_identifier_divide_towards_zero_int8: return 2;
    case modern_builtin_identifier_divide_towards_zero_int16: return 2;
    case modern_builtin_identifier_divide_towards_zero_int32: return 2;
    case modern_builtin_identifier_divide_towards_zero_int64: return 2;
    case modern_builtin_identifier_divide_towards_zero_nat8: return 2;
    case modern_builtin_identifier_divide_towards_zero_nat16: return 2;
    case modern_builtin_identifier_divide_towards_zero_nat32: return 2;
    case modern_builtin_identifier_divide_towards_zero_nat64: return 2;
    case modern_builtin_identifier_divide_towards_negative_infinity_int8: return 2;
    case modern_builtin_identifier_divide_towards_negative_infinity_int16: return 2;
    case modern_builtin_identifier_divide_towards_negative_infinity_int32: return 2;
    case modern_builtin_identifier_divide_towards_negative_infinity_int64: return 2;
    case modern_builtin_identifier_divide_towards_negative_infinity_nat8: return 2;
    case modern_builtin_identifier_divide_towards_negative_infinity_nat16: return 2;
    case modern_builtin_identifier_divide_towards_negative_infinity_nat32: return 2;
    case modern_builtin_identifier_divide_towards_negative_infinity_nat64: return 2;
    case modern_builtin_identifier_divide_float32: return 2;
    case modern_builtin_identifier_divide_float64: return 2;
    case modern_builtin_identifier_modulus_towards_zero_int8: return 2;
    case modern_builtin_identifier_modulus_towards_zero_int16: return 2;
    case modern_builtin_identifier_modulus_towards_zero_int32: return 2;
    case modern_builtin_identifier_modulus_towards_zero_int64: return 2;
    case modern_builtin_identifier_modulus_towards_zero_nat8: return 2;
    case modern_builtin_identifier_modulus_towards_zero_nat16: return 2;
    case modern_builtin_identifier_modulus_towards_zero_nat32: return 2;
    case modern_builtin_identifier_modulus_towards_zero_nat64: return 2;
    case modern_builtin_identifier_modulus_towards_negative_infinity_int8: return 2;
    case modern_builtin_identifier_modulus_towards_negative_infinity_int16: return 2;
    case modern_builtin_identifier_modulus_towards_negative_infinity_int32: return 2;
    case modern_builtin_identifier_modulus_towards_negative_infinity_int64: return 2;
    case modern_builtin_identifier_modulus_towards_negative_infinity_nat8: return 2;
    case modern_builtin_identifier_modulus_towards_negative_infinity_nat16: return 2;
    case modern_builtin_identifier_modulus_towards_negative_infinity_nat32: return 2;
    case modern_builtin_identifier_modulus_towards_negative_infinity_nat64: return 2;
    case modern_builtin_identifier_negate_int8: return 1;
    case modern_builtin_identifier_negate_int16: return 1;
    case modern_builtin_identifier_negate_int32: return 1;
    case modern_builtin_identifier_negate_int64: return 1;
    case modern_builtin_identifier_negate_float32: return 1;
    case modern_builtin_identifier_negate_float64: return 1;
    case modern_builtin_identifier_absolute_value_int8: return 1;
    case modern_builtin_identifier_absolute_value_int16: return 1;
    case modern_builtin_identifier_absolute_value_int32: return 1;
    case modern_builtin_identifier_absolute_value_int64: return 1;
    case modern_builtin_identifier_absolute_value_float32: return 1;
    case modern_builtin_identifier_absolute_value_float64: return 1;
    case modern_builtin_identifier_sign_int8: return 1;
    case modern_builtin_identifier_sign_int16: return 1;
    case modern_builtin_identifier_sign_int32: return 1;
    case modern_builtin_identifier_sign_int64: return 1;
    case modern_builtin_identifier_sign_float32: return 1;
    case modern_builtin_identifier_sign_float64: return 1;
    case modern_builtin_identifier_pi_float32: return 1;
    case modern_builtin_identifier_pi_float64: return 1;
    case modern_builtin_identifier_square_root_float32: return 1;
    case modern_builtin_identifier_square_root_float64: return 1;
    case modern_builtin_identifier_natural_logarithm_float32: return 1;
    case modern_builtin_identifier_natural_logarithm_float64: return 1;
    case modern_builtin_identifier_e_to_the_x_float32: return 1;
    case modern_builtin_identifier_e_to_the_x_float64: return 1;
    case modern_builtin_identifier_two_to_the_x_float32: return 1;
    case modern_builtin_identifier_two_to_the_x_float64: return 1;
    case modern_builtin_identifier_x_to_the_y_float32: return 2;
    case modern_builtin_identifier_x_to_the_y_float64: return 2;
    case modern_builtin_identifier_logarithm_base_x_float32: return 2;
    case modern_builtin_identifier_logarithm_base_x_float64: return 2;
    case modern_builtin_identifier_sine_float32: return 1;
    case modern_builtin_identifier_sine_float64: return 1;
    case modern_builtin_identifier_cosine_float32: return 1;
    case modern_builtin_identifier_cosine_float64: return 1;
    case modern_builtin_identifier_tangent_float32: return 1;
    case modern_builtin_identifier_tangent_float64: return 1;
    case modern_builtin_identifier_arcsine_float32: return 1;
    case modern_builtin_identifier_arcsine_float64: return 1;
    case modern_builtin_identifier_arccosine_float32: return 1;
    case modern_builtin_identifier_arccosine_float64: return 1;
    case modern_builtin_identifier_arctangent_float32: return 1;
    case modern_builtin_identifier_arctangent_float64: return 1;
    case modern_builtin_identifier_arctangent_fraction_float32: return 1;
    case modern_builtin_identifier_arctangent_fraction_float64: return 1;
    case modern_builtin_identifier_hyperbolic_sine_float32: return 1;
    case modern_builtin_identifier_hyperbolic_sine_float64: return 1;
    case modern_builtin_identifier_hyperbolic_cosine_float32: return 1;
    case modern_builtin_identifier_hyperbolic_cosine_float64: return 1;
    case modern_builtin_identifier_hyperbolic_tangent_float32: return 1;
    case modern_builtin_identifier_hyperbolic_tangent_float64: return 1;
    case modern_builtin_identifier_hyperbolic_arcsine_float32: return 1;
    case modern_builtin_identifier_hyperbolic_arcsine_float64: return 1;
    case modern_builtin_identifier_hyperbolic_arccosine_float32: return 1;
    case modern_builtin_identifier_hyperbolic_arccosine_float64: return 1;
    case modern_builtin_identifier_hyperbolic_arctangent_float32: return 1;
    case modern_builtin_identifier_hyperbolic_arctangent_float64: return 1;
    case modern_builtin_identifier_round_towards_zero_float32_int8: return 1;
    case modern_builtin_identifier_round_towards_zero_float64_int8: return 1;
    case modern_builtin_identifier_round_towards_zero_float32_int16: return 1;
    case modern_builtin_identifier_round_towards_zero_float64_int16: return 1;
    case modern_builtin_identifier_round_towards_zero_float32_int32: return 1;
    case modern_builtin_identifier_round_towards_zero_float64_int32: return 1;
    case modern_builtin_identifier_round_towards_zero_float32_int64: return 1;
    case modern_builtin_identifier_round_towards_zero_float64_int64: return 1;
    case modern_builtin_identifier_round_towards_zero_float32_nat8: return 1;
    case modern_builtin_identifier_round_towards_zero_float64_nat8: return 1;
    case modern_builtin_identifier_round_towards_zero_float32_nat16: return 1;
    case modern_builtin_identifier_round_towards_zero_float64_nat16: return 1;
    case modern_builtin_identifier_round_towards_zero_float32_nat32: return 1;
    case modern_builtin_identifier_round_towards_zero_float64_nat32: return 1;
    case modern_builtin_identifier_round_towards_zero_float32_nat64: return 1;
    case modern_builtin_identifier_round_towards_zero_float64_nat64: return 1;
    case modern_builtin_identifier_round_towards_zero_float32_float32: return 1;
    case modern_builtin_identifier_round_towards_zero_float64_float64: return 1;
    case modern_builtin_identifier_round_away_from_zero_float32_int8: return 1;
    case modern_builtin_identifier_round_away_from_zero_float64_int8: return 1;
    case modern_builtin_identifier_round_away_from_zero_float32_int16: return 1;
    case modern_builtin_identifier_round_away_from_zero_float64_int16: return 1;
    case modern_builtin_identifier_round_away_from_zero_float32_int32: return 1;
    case modern_builtin_identifier_round_away_from_zero_float64_int32: return 1;
    case modern_builtin_identifier_round_away_from_zero_float32_int64: return 1;
    case modern_builtin_identifier_round_away_from_zero_float64_int64: return 1;
    case modern_builtin_identifier_round_away_from_zero_float32_nat8: return 1;
    case modern_builtin_identifier_round_away_from_zero_float64_nat8: return 1;
    case modern_builtin_identifier_round_away_from_zero_float32_nat16: return 1;
    case modern_builtin_identifier_round_away_from_zero_float64_nat16: return 1;
    case modern_builtin_identifier_round_away_from_zero_float32_nat32: return 1;
    case modern_builtin_identifier_round_away_from_zero_float64_nat32: return 1;
    case modern_builtin_identifier_round_away_from_zero_float32_nat64: return 1;
    case modern_builtin_identifier_round_away_from_zero_float64_nat64: return 1;
    case modern_builtin_identifier_round_away_from_zero_float32_float32: return 1;
    case modern_builtin_identifier_round_away_from_zero_float64_float64: return 1;
    case modern_builtin_identifier_round_towards_even_float32_int8: return 1;
    case modern_builtin_identifier_round_towards_even_float64_int8: return 1;
    case modern_builtin_identifier_round_towards_even_float32_int16: return 1;
    case modern_builtin_identifier_round_towards_even_float64_int16: return 1;
    case modern_builtin_identifier_round_towards_even_float32_int32: return 1;
    case modern_builtin_identifier_round_towards_even_float64_int32: return 1;
    case modern_builtin_identifier_round_towards_even_float32_int64: return 1;
    case modern_builtin_identifier_round_towards_even_float64_int64: return 1;
    case modern_builtin_identifier_round_towards_even_float32_nat8: return 1;
    case modern_builtin_identifier_round_towards_even_float64_nat8: return 1;
    case modern_builtin_identifier_round_towards_even_float32_nat16: return 1;
    case modern_builtin_identifier_round_towards_even_float64_nat16: return 1;
    case modern_builtin_identifier_round_towards_even_float32_nat32: return 1;
    case modern_builtin_identifier_round_towards_even_float64_nat32: return 1;
    case modern_builtin_identifier_round_towards_even_float32_nat64: return 1;
    case modern_builtin_identifier_round_towards_even_float64_nat64: return 1;
    case modern_builtin_identifier_round_towards_even_float32_float32: return 1;
    case modern_builtin_identifier_round_towards_even_float64_float64: return 1;
    case modern_builtin_identifier_round_towards_odd_float32_int8: return 1;
    case modern_builtin_identifier_round_towards_odd_float64_int8: return 1;
    case modern_builtin_identifier_round_towards_odd_float32_int16: return 1;
    case modern_builtin_identifier_round_towards_odd_float64_int16: return 1;
    case modern_builtin_identifier_round_towards_odd_float32_int32: return 1;
    case modern_builtin_identifier_round_towards_odd_float64_int32: return 1;
    case modern_builtin_identifier_round_towards_odd_float32_int64: return 1;
    case modern_builtin_identifier_round_towards_odd_float64_int64: return 1;
    case modern_builtin_identifier_round_towards_odd_float32_nat8: return 1;
    case modern_builtin_identifier_round_towards_odd_float64_nat8: return 1;
    case modern_builtin_identifier_round_towards_odd_float32_nat16: return 1;
    case modern_builtin_identifier_round_towards_odd_float64_nat16: return 1;
    case modern_builtin_identifier_round_towards_odd_float32_nat32: return 1;
    case modern_builtin_identifier_round_towards_odd_float64_nat32: return 1;
    case modern_builtin_identifier_round_towards_odd_float32_nat64: return 1;
    case modern_builtin_identifier_round_towards_odd_float64_nat64: return 1;
    case modern_builtin_identifier_round_towards_odd_float32_float32: return 1;
    case modern_builtin_identifier_round_towards_odd_float64_float64: return 1;
    case modern_builtin_identifier_ceiling_float32_int8: return 1;
    case modern_builtin_identifier_ceiling_float64_int8: return 1;
    case modern_builtin_identifier_ceiling_float32_int16: return 1;
    case modern_builtin_identifier_ceiling_float64_int16: return 1;
    case modern_builtin_identifier_ceiling_float32_int32: return 1;
    case modern_builtin_identifier_ceiling_float64_int32: return 1;
    case modern_builtin_identifier_ceiling_float32_int64: return 1;
    case modern_builtin_identifier_ceiling_float64_int64: return 1;
    case modern_builtin_identifier_ceiling_float32_nat8: return 1;
    case modern_builtin_identifier_ceiling_float64_nat8: return 1;
    case modern_builtin_identifier_ceiling_float32_nat16: return 1;
    case modern_builtin_identifier_ceiling_float64_nat16: return 1;
    case modern_builtin_identifier_ceiling_float32_nat32: return 1;
    case modern_builtin_identifier_ceiling_float64_nat32: return 1;
    case modern_builtin_identifier_ceiling_float32_nat64: return 1;
    case modern_builtin_identifier_ceiling_float64_nat64: return 1;
    case modern_builtin_identifier_ceiling_float32_float32: return 1;
    case modern_builtin_identifier_ceiling_float64_float64: return 1;
    case modern_builtin_identifier_floor_float32_int8: return 1;
    case modern_builtin_identifier_floor_float64_int8: return 1;
    case modern_builtin_identifier_floor_float32_int16: return 1;
    case modern_builtin_identifier_floor_float64_int16: return 1;
    case modern_builtin_identifier_floor_float32_int32: return 1;
    case modern_builtin_identifier_floor_float64_int32: return 1;
    case modern_builtin_identifier_floor_float32_int64: return 1;
    case modern_builtin_identifier_floor_float64_int64: return 1;
    case modern_builtin_identifier_floor_float32_nat8: return 1;
    case modern_builtin_identifier_floor_float64_nat8: return 1;
    case modern_builtin_identifier_floor_float32_nat16: return 1;
    case modern_builtin_identifier_floor_float64_nat16: return 1;
    case modern_builtin_identifier_floor_float32_nat32: return 1;
    case modern_builtin_identifier_floor_float64_nat32: return 1;
    case modern_builtin_identifier_floor_float32_nat64: return 1;
    case modern_builtin_identifier_floor_float64_nat64: return 1;
    case modern_builtin_identifier_floor_float32_float32: return 1;
    case modern_builtin_identifier_floor_float64_float64: return 1;
    case modern_builtin_identifier_minimum_bound_int8: return 0;
    case modern_builtin_identifier_minimum_bound_int16: return 0;
    case modern_builtin_identifier_minimum_bound_int32: return 0;
    case modern_builtin_identifier_minimum_bound_int64: return 0;
    case modern_builtin_identifier_minimum_bound_nat8: return 0;
    case modern_builtin_identifier_minimum_bound_nat16: return 0;
    case modern_builtin_identifier_minimum_bound_nat32: return 0;
    case modern_builtin_identifier_minimum_bound_nat64: return 0;
    case modern_builtin_identifier_maximum_bound_int8: return 0;
    case modern_builtin_identifier_maximum_bound_int16: return 0;
    case modern_builtin_identifier_maximum_bound_int32: return 0;
    case modern_builtin_identifier_maximum_bound_int64: return 0;
    case modern_builtin_identifier_maximum_bound_nat8: return 0;
    case modern_builtin_identifier_maximum_bound_nat16: return 0;
    case modern_builtin_identifier_maximum_bound_nat32: return 0;
    case modern_builtin_identifier_maximum_bound_nat64: return 0;
    case modern_builtin_identifier_shift_left_int8: return 2;
    case modern_builtin_identifier_shift_left_int16: return 2;
    case modern_builtin_identifier_shift_left_int32: return 2;
    case modern_builtin_identifier_shift_left_int64: return 2;
    case modern_builtin_identifier_shift_left_nat8: return 2;
    case modern_builtin_identifier_shift_left_nat16: return 2;
    case modern_builtin_identifier_shift_left_nat32: return 2;
    case modern_builtin_identifier_shift_left_nat64: return 2;
    case modern_builtin_identifier_shift_right_int8: return 2;
    case modern_builtin_identifier_shift_right_int16: return 2;
    case modern_builtin_identifier_shift_right_int32: return 2;
    case modern_builtin_identifier_shift_right_int64: return 2;
    case modern_builtin_identifier_shift_right_nat8: return 2;
    case modern_builtin_identifier_shift_right_nat16: return 2;
    case modern_builtin_identifier_shift_right_nat32: return 2;
    case modern_builtin_identifier_shift_right_nat64: return 2;
    case modern_builtin_identifier_rotate_left_int8: return 2;
    case modern_builtin_identifier_rotate_left_int16: return 2;
    case modern_builtin_identifier_rotate_left_int32: return 2;
    case modern_builtin_identifier_rotate_left_int64: return 2;
    case modern_builtin_identifier_rotate_left_nat8: return 2;
    case modern_builtin_identifier_rotate_left_nat16: return 2;
    case modern_builtin_identifier_rotate_left_nat32: return 2;
    case modern_builtin_identifier_rotate_left_nat64: return 2;
    case modern_builtin_identifier_rotate_right_int8: return 2;
    case modern_builtin_identifier_rotate_right_int16: return 2;
    case modern_builtin_identifier_rotate_right_int32: return 2;
    case modern_builtin_identifier_rotate_right_int64: return 2;
    case modern_builtin_identifier_rotate_right_nat8: return 2;
    case modern_builtin_identifier_rotate_right_nat16: return 2;
    case modern_builtin_identifier_rotate_right_nat32: return 2;
    case modern_builtin_identifier_rotate_right_nat64: return 2;
    case modern_builtin_identifier_bit_and_int8: return 2;
    case modern_builtin_identifier_bit_and_int16: return 2;
    case modern_builtin_identifier_bit_and_int32: return 2;
    case modern_builtin_identifier_bit_and_int64: return 2;
    case modern_builtin_identifier_bit_and_nat8: return 2;
    case modern_builtin_identifier_bit_and_nat16: return 2;
    case modern_builtin_identifier_bit_and_nat32: return 2;
    case modern_builtin_identifier_bit_and_nat64: return 2;
    case modern_builtin_identifier_bit_or_int8: return 2;
    case modern_builtin_identifier_bit_or_int16: return 2;
    case modern_builtin_identifier_bit_or_int32: return 2;
    case modern_builtin_identifier_bit_or_int64: return 2;
    case modern_builtin_identifier_bit_or_nat8: return 2;
    case modern_builtin_identifier_bit_or_nat16: return 2;
    case modern_builtin_identifier_bit_or_nat32: return 2;
    case modern_builtin_identifier_bit_or_nat64: return 2;
    case modern_builtin_identifier_bit_xor_int8: return 2;
    case modern_builtin_identifier_bit_xor_int16: return 2;
    case modern_builtin_identifier_bit_xor_int32: return 2;
    case modern_builtin_identifier_bit_xor_int64: return 2;
    case modern_builtin_identifier_bit_xor_nat8: return 2;
    case modern_builtin_identifier_bit_xor_nat16: return 2;
    case modern_builtin_identifier_bit_xor_nat32: return 2;
    case modern_builtin_identifier_bit_xor_nat64: return 2;
    case modern_builtin_identifier_bit_not_int8: return 1;
    case modern_builtin_identifier_bit_not_int16: return 1;
    case modern_builtin_identifier_bit_not_int32: return 1;
    case modern_builtin_identifier_bit_not_int64: return 1;
    case modern_builtin_identifier_bit_not_nat8: return 1;
    case modern_builtin_identifier_bit_not_nat16: return 1;
    case modern_builtin_identifier_bit_not_nat32: return 1;
    case modern_builtin_identifier_bit_not_nat64: return 1;
    case modern_builtin_identifier_decode_utf8: return 1;
    case modern_builtin_identifier_encode_utf8: return 1;
    case modern_builtin_identifier_character_offset_to_byte_offset_utf8: return 2;
    case modern_builtin_identifier_length_bytes_utf8: return 1;
    case modern_builtin_identifier_length_bytes_blob: return 1;
    case modern_builtin_identifier_get_byte_blob: return 2;
    case modern_builtin_identifier_replace_byte_blob: return 3;
    case modern_builtin_identifier_get_data_piece_utf8: return 3;
    case modern_builtin_identifier_get_data_piece_blob: return 3;
    case modern_builtin_identifier_replace_data_piece_utf8: return 4;
    case modern_builtin_identifier_replace_data_piece_blob: return 4;
    case modern_builtin_identifier_empty_utf8: return 0;
    case modern_builtin_identifier_empty_blob: return 0;
    case modern_builtin_identifier_get_sigma_field_value: return 1;
    case modern_builtin_identifier_get_sigma_successor: return 1;
    case modern_builtin_identifier_get_named_value: return 1;
    case modern_builtin_identifier_get_function_type_left: return 1;
    case modern_builtin_identifier_get_function_type_right: return 1;
    case modern_builtin_identifier_get_sigma_type_field_type: return 1;
    case modern_builtin_identifier_get_sigma_type_successor: return 1;
    case modern_builtin_identifier_get_named_type_content_type: return 1;
    case modern_builtin_identifier_get_universe_type_level: return 1;
    case modern_builtin_identifier_make_sigma: return 2;
    case modern_builtin_identifier_make_name: return 1;
    case modern_builtin_identifier_make_named_value: return 2;
    case modern_builtin_identifier_make_function_type: return 2;
    case modern_builtin_identifier_make_sigma_type: return 2;
    case modern_builtin_identifier_make_named_type: return 1;
    case modern_builtin_identifier_make_universe_type: return 1;
    case modern_builtin_identifier_make_maybe_type: return 1;
    case modern_builtin_identifier_get_maybe_type_content_type: return 1;
    case modern_builtin_identifier_maybe_is_just: return 1;
    case modern_builtin_identifier_fmap_maybe: return 2;
    case modern_builtin_identifier_from_maybe: return 2;
    case modern_builtin_identifier_cast_int8_int16: return 1;
    case modern_builtin_identifier_cast_int8_int32: return 1;
    case modern_builtin_identifier_cast_int8_int64: return 1;
    case modern_builtin_identifier_cast_int8_nat8: return 1;
    case modern_builtin_identifier_cast_int8_nat16: return 1;
    case modern_builtin_identifier_cast_int8_nat32: return 1;
    case modern_builtin_identifier_cast_int8_nat64: return 1;
    case modern_builtin_identifier_cast_int16_int8: return 1;
    case modern_builtin_identifier_cast_int16_int32: return 1;
    case modern_builtin_identifier_cast_int16_int64: return 1;
    case modern_builtin_identifier_cast_int16_nat8: return 1;
    case modern_builtin_identifier_cast_int16_nat16: return 1;
    case modern_builtin_identifier_cast_int16_nat32: return 1;
    case modern_builtin_identifier_cast_int16_nat64: return 1;
    case modern_builtin_identifier_cast_int32_int8: return 1;
    case modern_builtin_identifier_cast_int32_int16: return 1;
    case modern_builtin_identifier_cast_int32_int64: return 1;
    case modern_builtin_identifier_cast_int32_nat8: return 1;
    case modern_builtin_identifier_cast_int32_nat16: return 1;
    case modern_builtin_identifier_cast_int32_nat32: return 1;
    case modern_builtin_identifier_cast_int32_nat64: return 1;
    case modern_builtin_identifier_cast_int64_int8: return 1;
    case modern_builtin_identifier_cast_int64_int16: return 1;
    case modern_builtin_identifier_cast_int64_int32: return 1;
    case modern_builtin_identifier_cast_int64_nat8: return 1;
    case modern_builtin_identifier_cast_int64_nat16: return 1;
    case modern_builtin_identifier_cast_int64_nat32: return 1;
    case modern_builtin_identifier_cast_int64_nat64: return 1;
    case modern_builtin_identifier_cast_nat8_int8: return 1;
    case modern_builtin_identifier_cast_nat8_int16: return 1;
    case modern_builtin_identifier_cast_nat8_int32: return 1;
    case modern_builtin_identifier_cast_nat8_int64: return 1;
    case modern_builtin_identifier_cast_nat8_nat16: return 1;
    case modern_builtin_identifier_cast_nat8_nat32: return 1;
    case modern_builtin_identifier_cast_nat8_nat64: return 1;
    case modern_builtin_identifier_cast_nat16_int8: return 1;
    case modern_builtin_identifier_cast_nat16_int16: return 1;
    case modern_builtin_identifier_cast_nat16_int32: return 1;
    case modern_builtin_identifier_cast_nat16_int64: return 1;
    case modern_builtin_identifier_cast_nat16_nat8: return 1;
    case modern_builtin_identifier_cast_nat16_nat32: return 1;
    case modern_builtin_identifier_cast_nat16_nat64: return 1;
    case modern_builtin_identifier_cast_nat32_int8: return 1;
    case modern_builtin_identifier_cast_nat32_int16: return 1;
    case modern_builtin_identifier_cast_nat32_int32: return 1;
    case modern_builtin_identifier_cast_nat32_int64: return 1;
    case modern_builtin_identifier_cast_nat32_nat8: return 1;
    case modern_builtin_identifier_cast_nat32_nat16: return 1;
    case modern_builtin_identifier_cast_nat32_nat64: return 1;
    case modern_builtin_identifier_cast_nat64_int8: return 1;
    case modern_builtin_identifier_cast_nat64_int16: return 1;
    case modern_builtin_identifier_cast_nat64_int32: return 1;
    case modern_builtin_identifier_cast_nat64_int64: return 1;
    case modern_builtin_identifier_cast_nat64_nat8: return 1;
    case modern_builtin_identifier_cast_nat64_nat16: return 1;
    case modern_builtin_identifier_cast_nat64_nat32: return 1;
    case modern_builtin_identifier_cast_utf8_blob: return 1;
    case modern_builtin_identifier_cast_blob_utf8: return 1;
    case modern_builtin_identifier_cast_name_blob: return 1;
    case modern_builtin_identifier_cast_blob_name: return 1;
    case modern_builtin_identifier_cast_float32_float64: return 1;
    case modern_builtin_identifier_cast_float64_float32: return 1;
    case modern_builtin_identifier_cast_blob_int8: return 1;
    case modern_builtin_identifier_cast_blob_int16: return 1;
    case modern_builtin_identifier_cast_blob_int32: return 1;
    case modern_builtin_identifier_cast_blob_int64: return 1;
    case modern_builtin_identifier_cast_blob_nat8: return 1;
    case modern_builtin_identifier_cast_blob_nat16: return 1;
    case modern_builtin_identifier_cast_blob_nat32: return 1;
    case modern_builtin_identifier_cast_blob_nat64: return 1;
    case modern_builtin_identifier_cast_blob_float32: return 1;
    case modern_builtin_identifier_cast_blob_float64: return 1;
    case modern_builtin_identifier_cast_int8_blob: return 1;
    case modern_builtin_identifier_cast_int16_blob: return 1;
    case modern_builtin_identifier_cast_int32_blob: return 1;
    case modern_builtin_identifier_cast_int64_blob: return 1;
    case modern_builtin_identifier_cast_nat8_blob: return 1;
    case modern_builtin_identifier_cast_nat16_blob: return 1;
    case modern_builtin_identifier_cast_nat32_blob: return 1;
    case modern_builtin_identifier_cast_nat64_blob: return 1;
    case modern_builtin_identifier_cast_float32_blob: return 1;
    case modern_builtin_identifier_cast_float64_blob: return 1;
    default: return 0;
    }
}


HELPER struct modern *helper_builtin_apply
  (struct modern_library *library, uint16_t index, struct modern **parameters)
{
    switch(index) {
        // TODO
    }
}


modern *modern_evaluate
  (modern_library *library_in, modern *node_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *node = (struct modern *) node_in;
    
    struct modern *result = helper_evaluate_node(library, node, NULL);
    
    return (modern *) result;
}


modern *modern_resolve_backreferences
  (modern_library *library_in, modern *node_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *node = (struct modern *) node_in;
    
    struct modern *result = helper_resolve_node(library, node, NULL);
    
    return (modern *) result;
}
