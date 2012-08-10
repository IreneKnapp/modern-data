#include "modern.h"


int main(int argc, char **argv) {
    struct modern_allocator allocator;
    allocator.modern_allocator_alloc = malloc;
    allocator.modern_allocator_free = free;
    allocator.modern_allocator_realloc = realloc;
    
    modern_autorelease_pool *pool = modern_make_autorelease_pool(&allocator);
    
    modern_context *context = modern_make_initial_context();
    
    struct modern_hash initial_namespace_hash;
    modern_get_initial_namespace_hash(&initial_namespace_hash);

    struct modern_hash s_combinator_name_hash;
    modern_compute_child_hash
      (&initial_namespace_hash, (uint8_t *) "S", 1, &s_combinator_name_hash);
    
    modern *s_combinator =
      modern_get_from_context(context, &s_combinator_name_hash);
    
    uint8_t *local_namespace = (uint8_t *) "com.ireneknapp.example";
    struct modern_hash local_namespace_hash;
    modern_compute_hash
      (local_namespace, strlen(local_namespace), &local_namespace_hash);
    
    uint8_t *left_name = (uint8_t *) "left";
    struct modern_hash left_name_hash;
    modern_compute_child_hash
      (&local_namespace_hash, left_name, strlen(left_name),
       &left_name_hash);
    
    uint8_t *right_name = (uint8_t *) "right";
    struct modern_hash right_name_hash;
    modern_compute_child_hash
      (&local_namespace_hash, right_name, strlen(right_name),
       &right_name_hash);
    
    modern *tree_subself_type = modern_node_make_type_index(1);
    modern_autorelease(pool, tree_subself_type);
    
    modern *tree_self_type =
      modern_node_make_apply(tree_subself_type, leaf_type);
    modern_autorelease(pool, tree_self_type);

    modern_hash *interior_name_hashes[2];
    interior_name_hashes[0] = left_name_hash;
    interior_name_hashes[1] = right_name_hash;
    
    modern *interior_types[2];
    interior_types[0] = tree_self_type;
    interior_types[1] = tree_self_type;
    
    modern *interior_type =
      modern_node_make_structure_type
        (2, &interior_name_hashes, &interior_types);
    
    modern *leaf_type = modern_node_make_type_index(0);
    modern_autorelease(pool, leaf_type);
    
    modern_hash *union_name_hashes[2];
    union_name_hashes[0] = interior_name_hash;
    union_name_hashes[1] = leaf_name_hash;
    
    modern *union_types[2];
    union_types[0] = interior_type;
    union_types[1] = leaf_type;
    
    modern *exterior_type =
      modern_node_make_union_type(2, &union_name_hashes, &union_types);
/*
(named "Tree"
  (apply "S"
    (lambda
      (lambda
        (union ("interior" (structure ("left" (apply 1 0))
                                      ("right" (apply 1 0))))
               ("leaf" 0))))))
*/
    
    FILE *file = fopen("output.modern", "wb");
    modern_serialize_file(value, context, file);
    fclose(file);
    
    return 0;
}

