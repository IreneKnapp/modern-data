#include <math.h>
#include <string.h>
#include "modern.h"

#define powq powl


void test_main(modern_library *library, modern_autorelease_pool *pool, modern_context *context) {
    modern_float32 value_float32s[] = {
    	0.0, -0.0,
    	1.0 / 0.0, -1.0 / 0.0,
    	1.0, -1.0,
    	0.5, -0.5,
    	0.25, -0.25,
    	0.125, -0.125,
    	1.0 - 0.5, -1.0 + 0.5,
    	1.0 - 0.25, -1.0 + 0.25,
    	1.0 - 0.125, -1.0 + 0.125,
    };
    for(int i = 0; i < 18; i++) {
		printf("%f", value_float32s[i]);
		modern *value = modern_node_make_float32(library, value_float32s[i]);
		
		struct modern_hash hash;
		modern_node_canonical_hash(library, value, &hash);
		
		modern_release(library, value);
	}
	
    modern_float64 value_float64s[] = {
    	0.0, -0.0,
    	1.0 / 0.0, -1.0 / 0.0,
    	1.0, -1.0,
    	0.5, -0.5,
    	0.25, -0.25,
    	0.125, -0.125,
    	1.0 - 0.5, -1.0 + 0.5,
    	1.0 - 0.25, -1.0 + 0.25,
    	1.0 - 0.125, -1.0 + 0.125,
    	1.0 - pow(2.0, -50.0), -1.0 + pow(2.0, -50.0),
    	1.0 - pow(2.0, -51.0), -1.0 + pow(2.0, -51.0),
    };
    for(int i = 0; i < 22; i++) {
		printf("%.20lf", value_float64s[i]);
		modern *value = modern_node_make_float64(library, value_float64s[i]);
		
		struct modern_hash hash;
		modern_node_canonical_hash(library, value, &hash);
		
		modern_release(library, value);
	}

    modern_float128 value_float128s[] = {
    	0.0L, -0.0L,
    	1.0L / 0.0L, -1.0L / 0.0L,
    	1.0L, -1.0L,
    	0.5L, -0.5L,
    	0.25L, -0.25L,
    	0.125L, -0.125L,
    	1.0L - 0.5L, -1.0L + 0.5L,
    	1.0L - 0.25L, -1.0L + 0.25L,
    	1.0L - 0.125L, -1.0L + 0.125L,
    	1.0L - powq(2.0L, -50.0L), -1.0L + powq(2.0L, -50.0L),
    	1.0L - powq(2.0L, -51.0L), -1.0L + powq(2.0L, -51.0L),
    	1.0L - powq(2.0L, -110.0L), -1.0L + powq(2.0L, -110.0L),
    	1.0L - powq(2.0L, -111.0L), -1.0L + powq(2.0L, -111.0L),
    };
    for(int i = 0; i < 26; i++) {
		printf("%.20Lf", value_float128s[i]);
		modern *value = modern_node_make_float128(library, value_float128s[i]);
		
		struct modern_hash hash;
		modern_node_canonical_hash(library, value, &hash);
		
		modern_release(library, value);
	}
}
