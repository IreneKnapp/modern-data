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
    if(node->node_type == backreference_modern_node_type) {
        uint64_t index = node->specifics.backreference.index;
        struct helper_target_stack_frame *frame = target_stack;
        while(frame) {
            if(index < frame->count) return frame->members[index];
            index -= frame->count;
            frame = frame->parent;
        }
        return NULL;
    } else if(node->node_type == apply_modern_node_type) {
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
        if(intermediate_result->node_type == lambda_modern_node_type) {
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
        } else if(intermediate_result->node_type == apply_modern_node_type) {
            struct helper_parameter_stack_frame *frame =
                helper_allocate_parameter_stack_frame(library);
            if(!frame) return NULL;
            
            frame->parameter = intermediate_result->specifics.apply.right;
            frame->parent = parameter_stack;
            
            parameter_stack = frame;
        } else if(intermediate_result->node_type == builtin_modern_node_type) {
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
    case if_bool_modern_builtin_identifier: return 3;
    case and_bool_modern_builtin_identifier: return 2;
    case or_bool_modern_builtin_identifier: return 2;
    case not_bool_modern_builtin_identifier: return 1;
    case equal_to_bool_modern_builtin_identifier: return 2;
    case equal_to_int8_modern_builtin_identifier: return 2;
    case equal_to_int16_modern_builtin_identifier: return 2;
    case equal_to_int32_modern_builtin_identifier: return 2;
    case equal_to_int64_modern_builtin_identifier: return 2;
    case equal_to_nat8_modern_builtin_identifier: return 2;
    case equal_to_nat16_modern_builtin_identifier: return 2;
    case equal_to_nat32_modern_builtin_identifier: return 2;
    case equal_to_nat64_modern_builtin_identifier: return 2;
    case equal_to_float32_modern_builtin_identifier: return 2;
    case equal_to_float64_modern_builtin_identifier: return 2;
    case equal_to_name_modern_builtin_identifier: return 2;
    case equal_to_utf8_modern_builtin_identifier: return 2;
    case equal_to_blob_modern_builtin_identifier: return 2;
    case equal_to_ordering_modern_builtin_identifier: return 2;
    case compare_int8_modern_builtin_identifier: return 2;
    case compare_int16_modern_builtin_identifier: return 2;
    case compare_int32_modern_builtin_identifier: return 2;
    case compare_int64_modern_builtin_identifier: return 2;
    case compare_nat8_modern_builtin_identifier: return 2;
    case compare_nat16_modern_builtin_identifier: return 2;
    case compare_nat32_modern_builtin_identifier: return 2;
    case compare_nat64_modern_builtin_identifier: return 2;
    case compare_float32_modern_builtin_identifier: return 2;
    case compare_float64_modern_builtin_identifier: return 2;
    case add_int8_modern_builtin_identifier: return 2;
    case add_int16_modern_builtin_identifier: return 2;
    case add_int32_modern_builtin_identifier: return 2;
    case add_int64_modern_builtin_identifier: return 2;
    case add_nat8_modern_builtin_identifier: return 2;
    case add_nat16_modern_builtin_identifier: return 2;
    case add_nat32_modern_builtin_identifier: return 2;
    case add_nat64_modern_builtin_identifier: return 2;
    case add_float32_modern_builtin_identifier: return 2;
    case add_float64_modern_builtin_identifier: return 2;
    case subtract_int8_modern_builtin_identifier: return 2;
    case subtract_int16_modern_builtin_identifier: return 2;
    case subtract_int32_modern_builtin_identifier: return 2;
    case subtract_int64_modern_builtin_identifier: return 2;
    case subtract_nat8_modern_builtin_identifier: return 2;
    case subtract_nat16_modern_builtin_identifier: return 2;
    case subtract_nat32_modern_builtin_identifier: return 2;
    case subtract_nat64_modern_builtin_identifier: return 2;
    case subtract_float32_modern_builtin_identifier: return 2;
    case subtract_float64_modern_builtin_identifier: return 2;
    case multiply_int8_modern_builtin_identifier: return 2;
    case multiply_int16_modern_builtin_identifier: return 2;
    case multiply_int32_modern_builtin_identifier: return 2;
    case multiply_int64_modern_builtin_identifier: return 2;
    case multiply_nat8_modern_builtin_identifier: return 2;
    case multiply_nat16_modern_builtin_identifier: return 2;
    case multiply_nat32_modern_builtin_identifier: return 2;
    case multiply_nat64_modern_builtin_identifier: return 2;
    case multiply_float32_modern_builtin_identifier: return 2;
    case multiply_float64_modern_builtin_identifier: return 2;
    case divide_towards_zero_int8_modern_builtin_identifier: return 2;
    case divide_towards_zero_int16_modern_builtin_identifier: return 2;
    case divide_towards_zero_int32_modern_builtin_identifier: return 2;
    case divide_towards_zero_int64_modern_builtin_identifier: return 2;
    case divide_towards_zero_nat8_modern_builtin_identifier: return 2;
    case divide_towards_zero_nat16_modern_builtin_identifier: return 2;
    case divide_towards_zero_nat32_modern_builtin_identifier: return 2;
    case divide_towards_zero_nat64_modern_builtin_identifier: return 2;
    case divide_towards_negative_infinity_int8_modern_builtin_identifier: return 2;
    case divide_towards_negative_infinity_int16_modern_builtin_identifier: return 2;
    case divide_towards_negative_infinity_int32_modern_builtin_identifier: return 2;
    case divide_towards_negative_infinity_int64_modern_builtin_identifier: return 2;
    case divide_towards_negative_infinity_nat8_modern_builtin_identifier: return 2;
    case divide_towards_negative_infinity_nat16_modern_builtin_identifier: return 2;
    case divide_towards_negative_infinity_nat32_modern_builtin_identifier: return 2;
    case divide_towards_negative_infinity_nat64_modern_builtin_identifier: return 2;
    case divide_float32_modern_builtin_identifier: return 2;
    case divide_float64_modern_builtin_identifier: return 2;
    case modulus_towards_zero_int8_modern_builtin_identifier: return 2;
    case modulus_towards_zero_int16_modern_builtin_identifier: return 2;
    case modulus_towards_zero_int32_modern_builtin_identifier: return 2;
    case modulus_towards_zero_int64_modern_builtin_identifier: return 2;
    case modulus_towards_zero_nat8_modern_builtin_identifier: return 2;
    case modulus_towards_zero_nat16_modern_builtin_identifier: return 2;
    case modulus_towards_zero_nat32_modern_builtin_identifier: return 2;
    case modulus_towards_zero_nat64_modern_builtin_identifier: return 2;
    case modulus_towards_negative_infinity_int8_modern_builtin_identifier: return 2;
    case modulus_towards_negative_infinity_int16_modern_builtin_identifier: return 2;
    case modulus_towards_negative_infinity_int32_modern_builtin_identifier: return 2;
    case modulus_towards_negative_infinity_int64_modern_builtin_identifier: return 2;
    case modulus_towards_negative_infinity_nat8_modern_builtin_identifier: return 2;
    case modulus_towards_negative_infinity_nat16_modern_builtin_identifier: return 2;
    case modulus_towards_negative_infinity_nat32_modern_builtin_identifier: return 2;
    case modulus_towards_negative_infinity_nat64_modern_builtin_identifier: return 2;
    case negate_int8_modern_builtin_identifier: return 1;
    case negate_int16_modern_builtin_identifier: return 1;
    case negate_int32_modern_builtin_identifier: return 1;
    case negate_int64_modern_builtin_identifier: return 1;
    case negate_float32_modern_builtin_identifier: return 1;
    case negate_float64_modern_builtin_identifier: return 1;
    case absolute_value_int8_modern_builtin_identifier: return 1;
    case absolute_value_int16_modern_builtin_identifier: return 1;
    case absolute_value_int32_modern_builtin_identifier: return 1;
    case absolute_value_int64_modern_builtin_identifier: return 1;
    case absolute_value_float32_modern_builtin_identifier: return 1;
    case absolute_value_float64_modern_builtin_identifier: return 1;
    case sign_int8_modern_builtin_identifier: return 1;
    case sign_int16_modern_builtin_identifier: return 1;
    case sign_int32_modern_builtin_identifier: return 1;
    case sign_int64_modern_builtin_identifier: return 1;
    case sign_float32_modern_builtin_identifier: return 1;
    case sign_float64_modern_builtin_identifier: return 1;
    case pi_float32_modern_builtin_identifier: return 1;
    case pi_float64_modern_builtin_identifier: return 1;
    case square_root_float32_modern_builtin_identifier: return 1;
    case square_root_float64_modern_builtin_identifier: return 1;
    case natural_logarithm_float32_modern_builtin_identifier: return 1;
    case natural_logarithm_float64_modern_builtin_identifier: return 1;
    case e_to_the_x_float32_modern_builtin_identifier: return 1;
    case e_to_the_x_float64_modern_builtin_identifier: return 1;
    case two_to_the_x_float32_modern_builtin_identifier: return 1;
    case two_to_the_x_float64_modern_builtin_identifier: return 1;
    case x_to_the_y_float32_modern_builtin_identifier: return 2;
    case x_to_the_y_float64_modern_builtin_identifier: return 2;
    case logarithm_base_x_float32_modern_builtin_identifier: return 2;
    case logarithm_base_x_float64_modern_builtin_identifier: return 2;
    case sine_float32_modern_builtin_identifier: return 1;
    case sine_float64_modern_builtin_identifier: return 1;
    case cosine_float32_modern_builtin_identifier: return 1;
    case cosine_float64_modern_builtin_identifier: return 1;
    case tangent_float32_modern_builtin_identifier: return 1;
    case tangent_float64_modern_builtin_identifier: return 1;
    case arcsine_float32_modern_builtin_identifier: return 1;
    case arcsine_float64_modern_builtin_identifier: return 1;
    case arccosine_float32_modern_builtin_identifier: return 1;
    case arccosine_float64_modern_builtin_identifier: return 1;
    case arctangent_float32_modern_builtin_identifier: return 1;
    case arctangent_float64_modern_builtin_identifier: return 1;
    case arctangent_fraction_float32_modern_builtin_identifier: return 1;
    case arctangent_fraction_float64_modern_builtin_identifier: return 1;
    case hyperbolic_sine_float32_modern_builtin_identifier: return 1;
    case hyperbolic_sine_float64_modern_builtin_identifier: return 1;
    case hyperbolic_cosine_float32_modern_builtin_identifier: return 1;
    case hyperbolic_cosine_float64_modern_builtin_identifier: return 1;
    case hyperbolic_tangent_float32_modern_builtin_identifier: return 1;
    case hyperbolic_tangent_float64_modern_builtin_identifier: return 1;
    case hyperbolic_arcsine_float32_modern_builtin_identifier: return 1;
    case hyperbolic_arcsine_float64_modern_builtin_identifier: return 1;
    case hyperbolic_arccosine_float32_modern_builtin_identifier: return 1;
    case hyperbolic_arccosine_float64_modern_builtin_identifier: return 1;
    case hyperbolic_arctangent_float32_modern_builtin_identifier: return 1;
    case hyperbolic_arctangent_float64_modern_builtin_identifier: return 1;
    case round_towards_zero_float32_int8_modern_builtin_identifier: return 1;
    case round_towards_zero_float64_int8_modern_builtin_identifier: return 1;
    case round_towards_zero_float32_int16_modern_builtin_identifier: return 1;
    case round_towards_zero_float64_int16_modern_builtin_identifier: return 1;
    case round_towards_zero_float32_int32_modern_builtin_identifier: return 1;
    case round_towards_zero_float64_int32_modern_builtin_identifier: return 1;
    case round_towards_zero_float32_int64_modern_builtin_identifier: return 1;
    case round_towards_zero_float64_int64_modern_builtin_identifier: return 1;
    case round_towards_zero_float32_nat8_modern_builtin_identifier: return 1;
    case round_towards_zero_float64_nat8_modern_builtin_identifier: return 1;
    case round_towards_zero_float32_nat16_modern_builtin_identifier: return 1;
    case round_towards_zero_float64_nat16_modern_builtin_identifier: return 1;
    case round_towards_zero_float32_nat32_modern_builtin_identifier: return 1;
    case round_towards_zero_float64_nat32_modern_builtin_identifier: return 1;
    case round_towards_zero_float32_nat64_modern_builtin_identifier: return 1;
    case round_towards_zero_float64_nat64_modern_builtin_identifier: return 1;
    case round_towards_zero_float32_float32_modern_builtin_identifier: return 1;
    case round_towards_zero_float64_float64_modern_builtin_identifier: return 1;
    case round_away_from_zero_float32_int8_modern_builtin_identifier: return 1;
    case round_away_from_zero_float64_int8_modern_builtin_identifier: return 1;
    case round_away_from_zero_float32_int16_modern_builtin_identifier: return 1;
    case round_away_from_zero_float64_int16_modern_builtin_identifier: return 1;
    case round_away_from_zero_float32_int32_modern_builtin_identifier: return 1;
    case round_away_from_zero_float64_int32_modern_builtin_identifier: return 1;
    case round_away_from_zero_float32_int64_modern_builtin_identifier: return 1;
    case round_away_from_zero_float64_int64_modern_builtin_identifier: return 1;
    case round_away_from_zero_float32_nat8_modern_builtin_identifier: return 1;
    case round_away_from_zero_float64_nat8_modern_builtin_identifier: return 1;
    case round_away_from_zero_float32_nat16_modern_builtin_identifier: return 1;
    case round_away_from_zero_float64_nat16_modern_builtin_identifier: return 1;
    case round_away_from_zero_float32_nat32_modern_builtin_identifier: return 1;
    case round_away_from_zero_float64_nat32_modern_builtin_identifier: return 1;
    case round_away_from_zero_float32_nat64_modern_builtin_identifier: return 1;
    case round_away_from_zero_float64_nat64_modern_builtin_identifier: return 1;
    case round_away_from_zero_float32_float32_modern_builtin_identifier: return 1;
    case round_away_from_zero_float64_float64_modern_builtin_identifier: return 1;
    case round_towards_even_float32_int8_modern_builtin_identifier: return 1;
    case round_towards_even_float64_int8_modern_builtin_identifier: return 1;
    case round_towards_even_float32_int16_modern_builtin_identifier: return 1;
    case round_towards_even_float64_int16_modern_builtin_identifier: return 1;
    case round_towards_even_float32_int32_modern_builtin_identifier: return 1;
    case round_towards_even_float64_int32_modern_builtin_identifier: return 1;
    case round_towards_even_float32_int64_modern_builtin_identifier: return 1;
    case round_towards_even_float64_int64_modern_builtin_identifier: return 1;
    case round_towards_even_float32_nat8_modern_builtin_identifier: return 1;
    case round_towards_even_float64_nat8_modern_builtin_identifier: return 1;
    case round_towards_even_float32_nat16_modern_builtin_identifier: return 1;
    case round_towards_even_float64_nat16_modern_builtin_identifier: return 1;
    case round_towards_even_float32_nat32_modern_builtin_identifier: return 1;
    case round_towards_even_float64_nat32_modern_builtin_identifier: return 1;
    case round_towards_even_float32_nat64_modern_builtin_identifier: return 1;
    case round_towards_even_float64_nat64_modern_builtin_identifier: return 1;
    case round_towards_even_float32_float32_modern_builtin_identifier: return 1;
    case round_towards_even_float64_float64_modern_builtin_identifier: return 1;
    case round_towards_odd_float32_int8_modern_builtin_identifier: return 1;
    case round_towards_odd_float64_int8_modern_builtin_identifier: return 1;
    case round_towards_odd_float32_int16_modern_builtin_identifier: return 1;
    case round_towards_odd_float64_int16_modern_builtin_identifier: return 1;
    case round_towards_odd_float32_int32_modern_builtin_identifier: return 1;
    case round_towards_odd_float64_int32_modern_builtin_identifier: return 1;
    case round_towards_odd_float32_int64_modern_builtin_identifier: return 1;
    case round_towards_odd_float64_int64_modern_builtin_identifier: return 1;
    case round_towards_odd_float32_nat8_modern_builtin_identifier: return 1;
    case round_towards_odd_float64_nat8_modern_builtin_identifier: return 1;
    case round_towards_odd_float32_nat16_modern_builtin_identifier: return 1;
    case round_towards_odd_float64_nat16_modern_builtin_identifier: return 1;
    case round_towards_odd_float32_nat32_modern_builtin_identifier: return 1;
    case round_towards_odd_float64_nat32_modern_builtin_identifier: return 1;
    case round_towards_odd_float32_nat64_modern_builtin_identifier: return 1;
    case round_towards_odd_float64_nat64_modern_builtin_identifier: return 1;
    case round_towards_odd_float32_float32_modern_builtin_identifier: return 1;
    case round_towards_odd_float64_float64_modern_builtin_identifier: return 1;
    case ceiling_float32_int8_modern_builtin_identifier: return 1;
    case ceiling_float64_int8_modern_builtin_identifier: return 1;
    case ceiling_float32_int16_modern_builtin_identifier: return 1;
    case ceiling_float64_int16_modern_builtin_identifier: return 1;
    case ceiling_float32_int32_modern_builtin_identifier: return 1;
    case ceiling_float64_int32_modern_builtin_identifier: return 1;
    case ceiling_float32_int64_modern_builtin_identifier: return 1;
    case ceiling_float64_int64_modern_builtin_identifier: return 1;
    case ceiling_float32_nat8_modern_builtin_identifier: return 1;
    case ceiling_float64_nat8_modern_builtin_identifier: return 1;
    case ceiling_float32_nat16_modern_builtin_identifier: return 1;
    case ceiling_float64_nat16_modern_builtin_identifier: return 1;
    case ceiling_float32_nat32_modern_builtin_identifier: return 1;
    case ceiling_float64_nat32_modern_builtin_identifier: return 1;
    case ceiling_float32_nat64_modern_builtin_identifier: return 1;
    case ceiling_float64_nat64_modern_builtin_identifier: return 1;
    case ceiling_float32_float32_modern_builtin_identifier: return 1;
    case ceiling_float64_float64_modern_builtin_identifier: return 1;
    case floor_float32_int8_modern_builtin_identifier: return 1;
    case floor_float64_int8_modern_builtin_identifier: return 1;
    case floor_float32_int16_modern_builtin_identifier: return 1;
    case floor_float64_int16_modern_builtin_identifier: return 1;
    case floor_float32_int32_modern_builtin_identifier: return 1;
    case floor_float64_int32_modern_builtin_identifier: return 1;
    case floor_float32_int64_modern_builtin_identifier: return 1;
    case floor_float64_int64_modern_builtin_identifier: return 1;
    case floor_float32_nat8_modern_builtin_identifier: return 1;
    case floor_float64_nat8_modern_builtin_identifier: return 1;
    case floor_float32_nat16_modern_builtin_identifier: return 1;
    case floor_float64_nat16_modern_builtin_identifier: return 1;
    case floor_float32_nat32_modern_builtin_identifier: return 1;
    case floor_float64_nat32_modern_builtin_identifier: return 1;
    case floor_float32_nat64_modern_builtin_identifier: return 1;
    case floor_float64_nat64_modern_builtin_identifier: return 1;
    case floor_float32_float32_modern_builtin_identifier: return 1;
    case floor_float64_float64_modern_builtin_identifier: return 1;
    case minimum_bound_int8_modern_builtin_identifier: return 0;
    case minimum_bound_int16_modern_builtin_identifier: return 0;
    case minimum_bound_int32_modern_builtin_identifier: return 0;
    case minimum_bound_int64_modern_builtin_identifier: return 0;
    case minimum_bound_nat8_modern_builtin_identifier: return 0;
    case minimum_bound_nat16_modern_builtin_identifier: return 0;
    case minimum_bound_nat32_modern_builtin_identifier: return 0;
    case minimum_bound_nat64_modern_builtin_identifier: return 0;
    case maximum_bound_int8_modern_builtin_identifier: return 0;
    case maximum_bound_int16_modern_builtin_identifier: return 0;
    case maximum_bound_int32_modern_builtin_identifier: return 0;
    case maximum_bound_int64_modern_builtin_identifier: return 0;
    case maximum_bound_nat8_modern_builtin_identifier: return 0;
    case maximum_bound_nat16_modern_builtin_identifier: return 0;
    case maximum_bound_nat32_modern_builtin_identifier: return 0;
    case maximum_bound_nat64_modern_builtin_identifier: return 0;
    case shift_left_int8_modern_builtin_identifier: return 2;
    case shift_left_int16_modern_builtin_identifier: return 2;
    case shift_left_int32_modern_builtin_identifier: return 2;
    case shift_left_int64_modern_builtin_identifier: return 2;
    case shift_left_nat8_modern_builtin_identifier: return 2;
    case shift_left_nat16_modern_builtin_identifier: return 2;
    case shift_left_nat32_modern_builtin_identifier: return 2;
    case shift_left_nat64_modern_builtin_identifier: return 2;
    case shift_right_int8_modern_builtin_identifier: return 2;
    case shift_right_int16_modern_builtin_identifier: return 2;
    case shift_right_int32_modern_builtin_identifier: return 2;
    case shift_right_int64_modern_builtin_identifier: return 2;
    case shift_right_nat8_modern_builtin_identifier: return 2;
    case shift_right_nat16_modern_builtin_identifier: return 2;
    case shift_right_nat32_modern_builtin_identifier: return 2;
    case shift_right_nat64_modern_builtin_identifier: return 2;
    case rotate_left_int8_modern_builtin_identifier: return 2;
    case rotate_left_int16_modern_builtin_identifier: return 2;
    case rotate_left_int32_modern_builtin_identifier: return 2;
    case rotate_left_int64_modern_builtin_identifier: return 2;
    case rotate_left_nat8_modern_builtin_identifier: return 2;
    case rotate_left_nat16_modern_builtin_identifier: return 2;
    case rotate_left_nat32_modern_builtin_identifier: return 2;
    case rotate_left_nat64_modern_builtin_identifier: return 2;
    case rotate_right_int8_modern_builtin_identifier: return 2;
    case rotate_right_int16_modern_builtin_identifier: return 2;
    case rotate_right_int32_modern_builtin_identifier: return 2;
    case rotate_right_int64_modern_builtin_identifier: return 2;
    case rotate_right_nat8_modern_builtin_identifier: return 2;
    case rotate_right_nat16_modern_builtin_identifier: return 2;
    case rotate_right_nat32_modern_builtin_identifier: return 2;
    case rotate_right_nat64_modern_builtin_identifier: return 2;
    case bit_and_int8_modern_builtin_identifier: return 2;
    case bit_and_int16_modern_builtin_identifier: return 2;
    case bit_and_int32_modern_builtin_identifier: return 2;
    case bit_and_int64_modern_builtin_identifier: return 2;
    case bit_and_nat8_modern_builtin_identifier: return 2;
    case bit_and_nat16_modern_builtin_identifier: return 2;
    case bit_and_nat32_modern_builtin_identifier: return 2;
    case bit_and_nat64_modern_builtin_identifier: return 2;
    case bit_or_int8_modern_builtin_identifier: return 2;
    case bit_or_int16_modern_builtin_identifier: return 2;
    case bit_or_int32_modern_builtin_identifier: return 2;
    case bit_or_int64_modern_builtin_identifier: return 2;
    case bit_or_nat8_modern_builtin_identifier: return 2;
    case bit_or_nat16_modern_builtin_identifier: return 2;
    case bit_or_nat32_modern_builtin_identifier: return 2;
    case bit_or_nat64_modern_builtin_identifier: return 2;
    case bit_xor_int8_modern_builtin_identifier: return 2;
    case bit_xor_int16_modern_builtin_identifier: return 2;
    case bit_xor_int32_modern_builtin_identifier: return 2;
    case bit_xor_int64_modern_builtin_identifier: return 2;
    case bit_xor_nat8_modern_builtin_identifier: return 2;
    case bit_xor_nat16_modern_builtin_identifier: return 2;
    case bit_xor_nat32_modern_builtin_identifier: return 2;
    case bit_xor_nat64_modern_builtin_identifier: return 2;
    case bit_not_int8_modern_builtin_identifier: return 1;
    case bit_not_int16_modern_builtin_identifier: return 1;
    case bit_not_int32_modern_builtin_identifier: return 1;
    case bit_not_int64_modern_builtin_identifier: return 1;
    case bit_not_nat8_modern_builtin_identifier: return 1;
    case bit_not_nat16_modern_builtin_identifier: return 1;
    case bit_not_nat32_modern_builtin_identifier: return 1;
    case bit_not_nat64_modern_builtin_identifier: return 1;
    case decode_utf8_modern_builtin_identifier: return 1;
    case encode_utf8_modern_builtin_identifier: return 1;
    case character_offset_to_byte_offset_utf8_modern_builtin_identifier: return 2;
    case length_bytes_utf8_modern_builtin_identifier: return 1;
    case length_bytes_blob_modern_builtin_identifier: return 1;
    case get_byte_blob_modern_builtin_identifier: return 2;
    case replace_byte_blob_modern_builtin_identifier: return 3;
    case get_data_piece_utf8_modern_builtin_identifier: return 3;
    case get_data_piece_blob_modern_builtin_identifier: return 3;
    case replace_data_piece_utf8_modern_builtin_identifier: return 4;
    case replace_data_piece_blob_modern_builtin_identifier: return 4;
    case empty_utf8_modern_builtin_identifier: return 0;
    case empty_blob_modern_builtin_identifier: return 0;
    case get_sigma_field_value_modern_builtin_identifier: return 1;
    case get_sigma_successor_modern_builtin_identifier: return 1;
    case get_named_value_modern_builtin_identifier: return 1;
    case get_function_type_left_modern_builtin_identifier: return 1;
    case get_function_type_right_modern_builtin_identifier: return 1;
    case get_sigma_type_field_type_modern_builtin_identifier: return 1;
    case get_sigma_type_successor_modern_builtin_identifier: return 1;
    case get_named_type_content_type_modern_builtin_identifier: return 1;
    case get_universe_type_level_modern_builtin_identifier: return 1;
    case make_sigma_modern_builtin_identifier: return 2;
    case make_name_modern_builtin_identifier: return 1;
    case make_named_value_modern_builtin_identifier: return 2;
    case make_function_type_modern_builtin_identifier: return 2;
    case make_sigma_type_modern_builtin_identifier: return 2;
    case make_named_type_modern_builtin_identifier: return 1;
    case make_universe_type_modern_builtin_identifier: return 1;
    case make_maybe_type_modern_builtin_identifier: return 1;
    case get_maybe_type_content_type_modern_builtin_identifier: return 1;
    case maybe_is_just_modern_builtin_identifier: return 1;
    case fmap_maybe_modern_builtin_identifier: return 2;
    case from_maybe_modern_builtin_identifier: return 2;
    case cast_int8_int16_modern_builtin_identifier: return 1;
    case cast_int8_int32_modern_builtin_identifier: return 1;
    case cast_int8_int64_modern_builtin_identifier: return 1;
    case cast_int8_nat8_modern_builtin_identifier: return 1;
    case cast_int8_nat16_modern_builtin_identifier: return 1;
    case cast_int8_nat32_modern_builtin_identifier: return 1;
    case cast_int8_nat64_modern_builtin_identifier: return 1;
    case cast_int16_int8_modern_builtin_identifier: return 1;
    case cast_int16_int32_modern_builtin_identifier: return 1;
    case cast_int16_int64_modern_builtin_identifier: return 1;
    case cast_int16_nat8_modern_builtin_identifier: return 1;
    case cast_int16_nat16_modern_builtin_identifier: return 1;
    case cast_int16_nat32_modern_builtin_identifier: return 1;
    case cast_int16_nat64_modern_builtin_identifier: return 1;
    case cast_int32_int8_modern_builtin_identifier: return 1;
    case cast_int32_int16_modern_builtin_identifier: return 1;
    case cast_int32_int64_modern_builtin_identifier: return 1;
    case cast_int32_nat8_modern_builtin_identifier: return 1;
    case cast_int32_nat16_modern_builtin_identifier: return 1;
    case cast_int32_nat32_modern_builtin_identifier: return 1;
    case cast_int32_nat64_modern_builtin_identifier: return 1;
    case cast_int64_int8_modern_builtin_identifier: return 1;
    case cast_int64_int16_modern_builtin_identifier: return 1;
    case cast_int64_int32_modern_builtin_identifier: return 1;
    case cast_int64_nat8_modern_builtin_identifier: return 1;
    case cast_int64_nat16_modern_builtin_identifier: return 1;
    case cast_int64_nat32_modern_builtin_identifier: return 1;
    case cast_int64_nat64_modern_builtin_identifier: return 1;
    case cast_nat8_int8_modern_builtin_identifier: return 1;
    case cast_nat8_int16_modern_builtin_identifier: return 1;
    case cast_nat8_int32_modern_builtin_identifier: return 1;
    case cast_nat8_int64_modern_builtin_identifier: return 1;
    case cast_nat8_nat16_modern_builtin_identifier: return 1;
    case cast_nat8_nat32_modern_builtin_identifier: return 1;
    case cast_nat8_nat64_modern_builtin_identifier: return 1;
    case cast_nat16_int8_modern_builtin_identifier: return 1;
    case cast_nat16_int16_modern_builtin_identifier: return 1;
    case cast_nat16_int32_modern_builtin_identifier: return 1;
    case cast_nat16_int64_modern_builtin_identifier: return 1;
    case cast_nat16_nat8_modern_builtin_identifier: return 1;
    case cast_nat16_nat32_modern_builtin_identifier: return 1;
    case cast_nat16_nat64_modern_builtin_identifier: return 1;
    case cast_nat32_int8_modern_builtin_identifier: return 1;
    case cast_nat32_int16_modern_builtin_identifier: return 1;
    case cast_nat32_int32_modern_builtin_identifier: return 1;
    case cast_nat32_int64_modern_builtin_identifier: return 1;
    case cast_nat32_nat8_modern_builtin_identifier: return 1;
    case cast_nat32_nat16_modern_builtin_identifier: return 1;
    case cast_nat32_nat64_modern_builtin_identifier: return 1;
    case cast_nat64_int8_modern_builtin_identifier: return 1;
    case cast_nat64_int16_modern_builtin_identifier: return 1;
    case cast_nat64_int32_modern_builtin_identifier: return 1;
    case cast_nat64_int64_modern_builtin_identifier: return 1;
    case cast_nat64_nat8_modern_builtin_identifier: return 1;
    case cast_nat64_nat16_modern_builtin_identifier: return 1;
    case cast_nat64_nat32_modern_builtin_identifier: return 1;
    case cast_utf8_blob_modern_builtin_identifier: return 1;
    case cast_blob_utf8_modern_builtin_identifier: return 1;
    case cast_name_blob_modern_builtin_identifier: return 1;
    case cast_blob_name_modern_builtin_identifier: return 1;
    case cast_float32_float64_modern_builtin_identifier: return 1;
    case cast_float64_float32_modern_builtin_identifier: return 1;
    case cast_blob_int8_modern_builtin_identifier: return 1;
    case cast_blob_int16_modern_builtin_identifier: return 1;
    case cast_blob_int32_modern_builtin_identifier: return 1;
    case cast_blob_int64_modern_builtin_identifier: return 1;
    case cast_blob_nat8_modern_builtin_identifier: return 1;
    case cast_blob_nat16_modern_builtin_identifier: return 1;
    case cast_blob_nat32_modern_builtin_identifier: return 1;
    case cast_blob_nat64_modern_builtin_identifier: return 1;
    case cast_blob_float32_modern_builtin_identifier: return 1;
    case cast_blob_float64_modern_builtin_identifier: return 1;
    case cast_int8_blob_modern_builtin_identifier: return 1;
    case cast_int16_blob_modern_builtin_identifier: return 1;
    case cast_int32_blob_modern_builtin_identifier: return 1;
    case cast_int64_blob_modern_builtin_identifier: return 1;
    case cast_nat8_blob_modern_builtin_identifier: return 1;
    case cast_nat16_blob_modern_builtin_identifier: return 1;
    case cast_nat32_blob_modern_builtin_identifier: return 1;
    case cast_nat64_blob_modern_builtin_identifier: return 1;
    case cast_float32_blob_modern_builtin_identifier: return 1;
    case cast_float64_blob_modern_builtin_identifier: return 1;
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
