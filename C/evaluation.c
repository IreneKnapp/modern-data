#include "modern.h"
#include "internal.h"


modern *modern_evaluate
  (modern_library *library_in, modern *node_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *node = (struct modern *) node_in;
    modern_autorelease_pool *pool = modern_make_autorelease_pool(library_in);
    
    modern *result = NULL;
    switch(node->node_type) {
    }
    
    modern_autorelease_pool_release(library_in, pool);
    return (modern *) result;
}
