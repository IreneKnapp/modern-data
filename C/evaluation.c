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
HELPER uint64_t helper_evaluate_application
  (struct modern_library *library,
   struct modern *left,
   struct modern *right,
   struct helper_target_stack_frame *target_stack,
   struct helper_parameter_stack_frame *parameter_stack,
   struct modern **result);


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
        size_t index = uint64_t node->specifics.backreference.index;
        struct helper_target_stack_frame *frame = target_stack;
        while(frame) {
            if(index < frame->count) return frame->members[i];
            index -= frame->count;
            frame = frame->parent;
        }
        return NULL;
    }
    
    if(node->node_type != apply_modern_node_type) return node;
    
    struct modern *result = NULL;
    uint64_t consumed_count = helper_evaluate_application
        (library,
         node->specifics.apply.left,
         node->specifics.apply.right,
         target_stack,
         NULL,
         &result);
    if(consumed_count == 1) return result;
    else return NULL;
}


HELPER uint64_t helper_evaluate_application
  (struct modern_library *library,
   struct modern *left,
   struct modern *right,
   struct helper_target_stack_frame *target_stack,
   struct helper_parameter_stack_frame *parameter_stack,
   struct modern **result_out)
{
    if(left->node_type == lambda_modern_node_type) {
        struct modern *right = (struct modern *)
            helper_evaluate_node
                (library, node->specifics.apply.right, target_stack);
        if(!right) return 0;
        
        struct helper_target_stack_frame *frame =
            helper_allocate_target_stack_frame(library, 1);
        if(!frame) return 0;
        
        frame->members[0] = right;
        frame->parent = target_stack;
        
        modern *result = helper_evaluate_node
            (library, node->specifics.apply.left, frame);
        
        library->allocator->modern_allocator_free
            (library->client_state, frame);
        
        *result_out = result;
        return 1;
    } else if(left->node_type == apply_modern_node_type)
    {
        struct helper_parameter_stack_frame *frame =
            helper_allocate_parameter_stack_frame(library);
        if(!frame) return -1;
        
        modern *result = NULL;
        uint64_t consumed_count =
            helper_evaluate_application
                (library,
                 left->specifics.apply.left,
                 left->specifics.apply.right,
                 target_stack,
                 frame,
                 &result);
        
        if(consumed_count == 0) {
            return 0;
        } else {
            *result_out = result;
            return consumed_count - 1;
        }
    } else if(node->specifics.apply.left->node_type
              == builtin_modern_node_type)
    {
    } else {
        return NULL;
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
