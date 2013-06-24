#include "modern.h"
#include "internal.h"


INTERNAL void default_finalize
  (modern_library *library_in,
   modern *node_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *node = (struct modern *) node_in;
    
    if(node->finalizer) node->finalizer(library, node);
}

