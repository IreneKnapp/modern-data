#include <string.h>
#include "modern.h"


static void error_memory(size_t requested_size);
static void error_retain_count_overflow(void *retainable);
static void error_retain_count_underflow(void *retainable);
static void error_double_autorelease(void *retainable);
static void error_type_mismatch(modern *expected, modern *actual);


int main(int argc, char **argv) {
    struct modern_error_handler error_handler;
    error_handler.modern_error_handler_memory = error_memory;
    error_handler.modern_error_handler_retain_count_overflow = error_retain_count_overflow;
    error_handler.modern_error_handler_retain_count_underflow = error_retain_count_underflow;
    error_handler.modern_error_handler_double_autorelease = error_double_autorelease;
    error_handler.modern_error_handler_type_mismatch = error_type_mismatch;
    
    struct modern_allocator allocator;
    allocator.modern_allocator_alloc = malloc;
    allocator.modern_allocator_free = free;
    allocator.modern_allocator_realloc = realloc;
    
    modern_autorelease_pool *pool = modern_make_autorelease_pool(&error_handler, &allocator);
    
    modern_context *context = modern_make_initial_context();
    
    struct modern_hash initial_namespace_hash;
    modern_get_initial_namespace_hash(&initial_namespace_hash);

	uint8_t *s_combinator_name = (uint8_t *) "S";
    struct modern_hash s_combinator_name_hash;
    modern_compute_child_hash
      (&initial_namespace_hash, (uint8_t *) "S", 1, &s_combinator_name_hash);
    
    modern *s_combinator =
      modern_get_from_context(context, &s_combinator_name_hash);
    
    uint8_t *local_namespace = (uint8_t *) "com.ireneknapp.example";
    struct modern_hash local_namespace_hash;
    modern_compute_hash
      (local_namespace, strlen((char *) local_namespace), &local_namespace_hash);
    
    modern *leaf_type = modern_node_make_type_index(0);
    modern_autorelease(&error_handler, pool, leaf_type);
    
    modern *tree_subself_type = modern_node_make_type_index(1);
    modern_autorelease(&error_handler, pool, tree_subself_type);
    
    modern *tree_self_type =
      modern_node_make_apply(tree_subself_type, leaf_type);
    modern_autorelease(&error_handler, pool, tree_self_type);
    
    modern *tree_unnamed_type =
      modern_node_make_apply(s_combinator, tree_self_type);
    
    uint8_t *tree_name = (uint8_t *) "Tree";
    struct modern_hash tree_name_hash;
    modern_compute_child_hash
      (&local_namespace_hash, tree_name, strlen((char *) tree_name), &tree_name_hash);
    
    modern *tree_type =
      modern_node_make_named_type(&tree_name_hash, tree_unnamed_type);
    
/*
(named "Tree"
  (apply "S"
    (lambda
      (lambda
        (union (structure (apply 1 0) (apply 1 0))
               0)))))
*/
    //modern *value = modern_node_make_named_value(tree_type, subvalue);
    
    FILE *file = fopen("output.modern", "wb");
    modern_serialize_file(tree_type, context, file);
    fclose(file);
    
    return 0;
}


static void error_memory(size_t requested_size) {
    fprintf(stderr, "Unable to allocate %llu bytes.\n",
            (unsigned long long) requested_size);
    exit(1);
}


static void error_retain_count_overflow(void *retainable) {
    fprintf(stderr, "Retain count overflow on object at 0x%llx.\n",
            (unsigned long long) retainable);
    exit(1);
}


static void error_retain_count_underflow(void *retainable) {
    fprintf(stderr, "Retain count underflow on object at 0x%llx.\n",
            (unsigned long long) retainable);
    exit(1);
}


static void error_double_autorelease(void *retainable) {
    fprintf(stderr, "Double autorelease on object at 0x%llx.\n",
            (unsigned long long) retainable);
    exit(1);
}


static void error_type_mismatch(modern *expected, modern *actual) {
    fprintf(stderr, "Type mismatch.  Expected 0x%llx; got 0x%llx.\n",
            (unsigned long long) expected, (unsigned long long) actual);
    exit(1);
}
