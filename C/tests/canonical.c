#include <math.h>
#include <string.h>
#include "modern.h"
#include "test.h"


void test_main(test_suite *test_suite, modern_library *library) {
    if(!begin_fixtures(test_suite)) return;
    allow_allocation(test_suite);
    modern_autorelease_pool *pool = modern_make_autorelease_pool(library);
    modern_context *context = modern_make_initial_context(library);
    disallow_allocation(test_suite);
    end_fixtures(test_suite);
    
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
}
