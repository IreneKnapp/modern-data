#include <math.h>
#include <string.h>
#include "modern.h"
#include "internal.h"


INTERNAL void default_canonical_hash_set
    (modern_library *library_in,
     void *value_in,
     struct modern_hash hash)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern *value = (struct modern *) value_in;
    
    value->canonical_hash_valid = 1;
    value->canonical_hash = hash;
}

