#include <string.h>
#include "modern.h"


void test_main(modern_library *library, modern_autorelease_pool *pool, modern_context *context) {
    struct modern_hash initial_namespace_hash;
    modern_compute_initial_namespace_hash(&initial_namespace_hash);

	uint8_t *s_combinator_name = (uint8_t *) "S";
    struct modern_hash s_combinator_name_hash;
    modern_compute_child_hash
      (&initial_namespace_hash, (uint8_t *) "S", 1, &s_combinator_name_hash);
    
    modern *s_combinator =
      modern_get_from_context(library, context, &s_combinator_name_hash);
    
    uint8_t *local_namespace = (uint8_t *) "com.ireneknapp.example";
    struct modern_hash local_namespace_hash;
    modern_compute_hash
      (local_namespace, strlen((char *) local_namespace),
       &local_namespace_hash);
    
    modern *leaf_type = modern_node_make_type_index(library, 0);
    modern_autorelease(library, pool, leaf_type);
    
    modern *tree_subself_type = modern_node_make_type_index(library, 1);
    modern_autorelease(library, pool, tree_subself_type);
    
    modern *tree_self_type =
      modern_node_make_apply(library, tree_subself_type, leaf_type);
    modern_autorelease(library, pool, tree_self_type);
    
    modern *tree_unnamed_type =
      modern_node_make_apply(library, s_combinator, tree_self_type);
    
    uint8_t *tree_name = (uint8_t *) "Tree";
    struct modern_hash tree_name_hash;
    modern_compute_child_hash
      (&local_namespace_hash,
       tree_name, strlen((char *) tree_name), &tree_name_hash);
    
    modern *tree_type =
      modern_node_make_named_type(library, &tree_name_hash, tree_unnamed_type);
    
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
    modern_serialize_file(library, tree_type, context, file);
    fclose(file);
}
