#include <math.h>
#include "modern.h"
#include "test.h"

struct test_context {
    test_suite *test_suite;
    modern_library *library;
    modern_autorelease_pool *pool;
    modern_context *context;
    void *value;
};


static int test_int8_node_create_and_readback
  (void *test_context);
static int test_int16_node_create_and_readback
  (void *test_context);
static int test_int32_node_create_and_readback
  (void *test_context);
static int test_int64_node_create_and_readback
  (void *test_context);
static int test_nat8_node_create_and_readback
  (void *test_context);
static int test_nat16_node_create_and_readback
  (void *test_context);
static int test_nat32_node_create_and_readback
  (void *test_context);
static int test_nat64_node_create_and_readback
  (void *test_context);
static int test_float32_node_create_and_readback
  (void *test_context);
static int test_float32_node_create_and_readback_negative_zero
  (void *test_context);
static int test_float32_node_create_expecting_error
  (void *test_context);
static int test_float32_node_create_expecting_error_helper
  (void *test_context);
static int test_float64_node_create_and_readback
  (void *test_context);
static int test_float64_node_create_and_readback_negative_zero
  (void *test_context);
static int test_float64_node_create_expecting_error
  (void *test_context);
static int test_float64_node_create_expecting_error_helper
  (void *test_context);
static int test_utf8_node_create_and_readback
  (void *test_context);


void test_main(test_suite *test_suite, modern_library *library) {
    if(!begin_fixtures(test_suite)) return;
    allow_allocation(test_suite);
    modern_autorelease_pool *pool = modern_make_autorelease_pool(library);
    modern_context *context = modern_make_initial_context(library);
    disallow_allocation(test_suite);
    end_fixtures(test_suite);
    
    struct test_context test_context;
    test_context.test_suite = test_suite;
    test_context.library = library;
    test_context.pool = pool;
    test_context.context = context;
    
    {
        int8_t values[6] = {
            0, 1, -1, 42, INT8_MAX, INT8_MIN,
        };
        
        for(int i = 0; i < 6; i++) {
            int8_t value = values[i];
            test_context.value = &value;
            begin_test_case
                (test_suite,
                 test_int8_node_create_and_readback,
                 (void *) &test_context,
                 "int8 node create-and-readback with value %hhi",
                 value);
        }
    }
    
    {
        int16_t values[8] = {
            0, 1, -1, 42, INT8_MAX, INT8_MIN,
            INT16_MAX, INT16_MIN,
        };
        
        for(int i = 0; i < 8; i++) {
            int16_t value = values[i];
            test_context.value = &value;
            begin_test_case
                (test_suite,
                 test_int16_node_create_and_readback,
                 (void *) &test_context,
                 "int16 node create-and-readback with value %hi",
                 value);
        }
    }
    
    {
        int32_t values[10] = {
            0, 1, -1, 42, INT8_MAX, INT8_MIN,
            INT16_MAX, INT16_MIN,
            INT32_MAX, INT32_MIN,
        };
        
        for(int i = 0; i < 10; i++) {
            int32_t value = values[i];
            test_context.value = &value;
            begin_test_case
                (test_suite,
                 test_int32_node_create_and_readback,
                 (void *) &test_context,
                 "int32 node create-and-readback with value %li",
                 value);
        }
    }
    
    {
        int64_t values[12] = {
            0, 1, -1, 42, INT8_MAX, INT8_MIN,
            INT16_MAX, INT16_MIN,
            INT32_MAX, INT32_MIN,
            INT64_MAX, INT64_MIN,
        };
        
        for(int i = 0; i < 12; i++) {
            int64_t value = values[i];
            test_context.value = &value;
            begin_test_case
                (test_suite,
                 test_int64_node_create_and_readback,
                 (void *) &test_context,
                 "int64 node create-and-readback with value %lli",
                 value);
        }
    }
    
    {
        uint8_t values[4] = {
            0, 1, 42, UINT8_MAX,
        };
        
        for(int i = 0; i < 4; i++) {
            uint8_t value = values[i];
            test_context.value = &value;
            begin_test_case
                (test_suite,
                 test_nat8_node_create_and_readback,
                 (void *) &test_context,
                 "nat8 node create-and-readback with value %hhu",
                 value);
        }
    }
    
    {
        uint16_t values[5] = {
            0, 1, 42, UINT8_MAX,
            UINT16_MAX,
        };
        
        for(int i = 0; i < 5; i++) {
            uint16_t value = values[i];
            test_context.value = &value;
            begin_test_case
                (test_suite,
                 test_nat16_node_create_and_readback,
                 (void *) &test_context,
                 "nat16 node create-and-readback with value %hu",
                 value);
        }
    }
    
    {
        uint32_t values[6] = {
            0, 1, 42, UINT8_MAX,
            UINT16_MAX,
            UINT32_MAX,
        };
        
        for(int i = 0; i < 6; i++) {
            uint32_t value = values[i];
            test_context.value = &value;
            begin_test_case
                (test_suite,
                 test_nat32_node_create_and_readback,
                 (void *) &test_context,
                 "nat32 node create-and-readback with value %lu",
                 value);
        }
    }
    
    {
        uint64_t values[7] = {
            0, 1, 42, UINT8_MAX,
            UINT16_MAX,
            UINT32_MAX,
            UINT64_MAX,
        };
        
        for(int i = 0; i < 7; i++) {
            uint64_t value = values[i];
            test_context.value = &value;
            begin_test_case
                (test_suite,
                 test_nat64_node_create_and_readback,
                 (void *) &test_context,
                 "nat64 node create-and-readback with value %llu",
                 value);
        }
    }
    
    {
        modern_float32 values[17] = {
          0.0,
          1.0, -1.0,
          0.5, -0.5,
          0.25, -0.25,
          0.125, -0.125,
          1.0 - 0.5, -1.0 + 0.5,
          1.0 - 0.25, -1.0 + 0.25,
          1.0 - 0.125, -1.0 + 0.125,
        };
        
        for(int i = 0; i < 18; i++) {
            modern_float32 value = values[i];
            test_context.value = &value;
            begin_test_case
                (test_suite,
                 test_float32_node_create_and_readback,
                 (void *) &test_context,
                 "float32 node create-and-readback with value %f",
                 value);
        }
    }
    
    test_context.value = NULL;
    begin_test_case
        (test_suite,
         test_float32_node_create_and_readback_negative_zero,
         (void *) &test_context,
         "float32 node create-and-readback special case for negative zero");
    
    {
        modern_float32 values[3] = {
            1.0 / 0.0,
            -1.0 / 0.0,
            0.0 /  0.0,
        };
        
        for(int i = 0; i < 3; i++) {
            modern_float32 value = values[i];
            test_context.value = &value;
            begin_test_case
                (test_suite,
                 test_float32_node_create_expecting_error,
                 (void *) &test_context,
                 "float32 node create expecting error with value %f",
                 value);
        }
    }
        
    {
        modern_float64 values[21] = {
          0.0,
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
            modern_float64 value = values[i];
            test_context.value = &value;
            begin_test_case
                (test_suite,
                 test_float64_node_create_and_readback,
                 (void *) &test_context,
                 "float64 node create-and-readback with value %lf",
                 value);
        }
    }
    
    test_context.value = NULL;
    begin_test_case
        (test_suite,
         test_float64_node_create_and_readback_negative_zero,
         (void *) &test_context,
         "float64 node create-and-readback special case for negative zero");
    
    {
        modern_float64 values[3] = {
            1.0 / 0.0,
            -1.0 / 0.0,
            0.0 /  0.0,
        };
        
        for(int i = 0; i < 3; i++) {
            modern_float64 value = values[i];
            test_context.value = &value;
            begin_test_case
                (test_suite,
                 test_float64_node_create_expecting_error,
                 (void *) &test_context,
                 "float64 node create expecting error with value %f",
                 value);
        }
    }
    
    {
        uint8_t *values[5] = {
            (uint8_t *) "",
            (uint8_t *) "A",
            (uint8_t *) "ABC",
            (uint8_t *) "Hello, world!",
            (uint8_t *) "Colorless green ideas sleep furiously.",
        };
        
        for(int i = 0; i < 5; i++) {
            uint8_t *value = values[i];
            test_context.value = &value;
            begin_test_case
                (test_suite,
                 test_utf8_node_create_and_readback,
                 (void *) &test_context,
                 "utf8 node create-and-readback with value \"%s\"",
                 value);
        }
    }
}


static int test_int8_node_create_and_readback
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    int8_t value = *(int8_t *) test_context->value;
    
    allow_allocation(test_suite);
    modern *node = modern_node_make_int8(library, value);
    disallow_allocation(test_suite);
    
    if(!node) return 0;
    
    int succeeded = 1;
    
    if(succeeded) {
        enum modern_node_type node_type =
            modern_node_get_node_type(library, node);
        if(node_type != int8_value_modern_node_type)
            succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_int8(library, node) != value)
            succeeded = 0;
    }
    
    modern *type = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        type = modern_node_get_value_type(library, node);
        disallow_allocation(test_suite);
        if(type) {
            enum modern_node_type type_node_type =
                modern_node_get_node_type(library, type);
            if(type_node_type != int8_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    modern *universe = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        universe = modern_node_get_value_type(library, type);
        disallow_allocation(test_suite);
        if(universe) {
            enum modern_node_type universe_node_type =
                modern_node_get_node_type(library, universe);
            if(universe_node_type != universe_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    if(succeeded) {
        uint64_t level = modern_node_get_universe_type_level
            (library, universe);
        if(level != 0) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, node) != type) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, type) != universe) succeeded = 0;
    }
    
    allow_deallocation(test_suite);
    modern_release(library, node);
    disallow_deallocation(test_suite);
    
    return succeeded;
}


static int test_int16_node_create_and_readback
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    int16_t value = *(int16_t *) test_context->value;
    
    allow_allocation(test_suite);
    modern *node = modern_node_make_int16(library, value);
    disallow_allocation(test_suite);
    
    if(!node) return 0;
    
    int succeeded = 1;
    
    if(succeeded) {
        enum modern_node_type node_type =
            modern_node_get_node_type(library, node);
        if(node_type != int16_value_modern_node_type)
            succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_int16(library, node) != value)
            succeeded = 0;
    }
    
    modern *type = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        type = modern_node_get_value_type(library, node);
        disallow_allocation(test_suite);
        if(type) {
            enum modern_node_type type_node_type =
                modern_node_get_node_type(library, type);
            if(type_node_type != int16_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    modern *universe = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        universe = modern_node_get_value_type(library, type);
        disallow_allocation(test_suite);
        if(universe) {
            enum modern_node_type universe_node_type =
                modern_node_get_node_type(library, universe);
            if(universe_node_type != universe_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    if(succeeded) {
        uint64_t level = modern_node_get_universe_type_level
            (library, universe);
        if(level != 0) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, node) != type) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, type) != universe) succeeded = 0;
    }
    
    allow_deallocation(test_suite);
    modern_release(library, node);
    disallow_deallocation(test_suite);
    
    return succeeded;
}


static int test_int32_node_create_and_readback
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    int32_t value = *(int32_t *) test_context->value;
    
    allow_allocation(test_suite);
    modern *node = modern_node_make_int32(library, value);
    disallow_allocation(test_suite);
    
    if(!node) return 0;
    
    int succeeded = 1;
    
    if(succeeded) {
        enum modern_node_type node_type =
            modern_node_get_node_type(library, node);
        if(node_type != int32_value_modern_node_type)
            succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_int32(library, node) != value)
            succeeded = 0;
    }
    
    modern *type = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        type = modern_node_get_value_type(library, node);
        disallow_allocation(test_suite);
        if(type) {
            enum modern_node_type type_node_type =
                modern_node_get_node_type(library, type);
            if(type_node_type != int32_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    modern *universe = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        universe = modern_node_get_value_type(library, type);
        disallow_allocation(test_suite);
        if(universe) {
            enum modern_node_type universe_node_type =
                modern_node_get_node_type(library, universe);
            if(universe_node_type != universe_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    if(succeeded) {
        uint64_t level = modern_node_get_universe_type_level
            (library, universe);
        if(level != 0) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, node) != type) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, type) != universe) succeeded = 0;
    }
    
    allow_deallocation(test_suite);
    modern_release(library, node);
    disallow_deallocation(test_suite);
    
    return succeeded;
}


static int test_int64_node_create_and_readback
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    int64_t value = *(int64_t *) test_context->value;
    
    allow_allocation(test_suite);
    modern *node = modern_node_make_int64(library, value);
    disallow_allocation(test_suite);
    
    if(!node) return 0;
    
    int succeeded = 1;
    
    if(succeeded) {
        enum modern_node_type node_type =
            modern_node_get_node_type(library, node);
        if(node_type != int64_value_modern_node_type)
            succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_int64(library, node) != value)
            succeeded = 0;
    }
    
    modern *type = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        type = modern_node_get_value_type(library, node);
        disallow_allocation(test_suite);
        if(type) {
            enum modern_node_type type_node_type =
                modern_node_get_node_type(library, type);
            if(type_node_type != int64_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    modern *universe = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        universe = modern_node_get_value_type(library, type);
        disallow_allocation(test_suite);
        if(universe) {
            enum modern_node_type universe_node_type =
                modern_node_get_node_type(library, universe);
            if(universe_node_type != universe_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    if(succeeded) {
        uint64_t level = modern_node_get_universe_type_level
            (library, universe);
        if(level != 0) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, node) != type) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, type) != universe) succeeded = 0;
    }
    
    allow_deallocation(test_suite);
    modern_release(library, node);
    disallow_deallocation(test_suite);
    
    return succeeded;
}


static int test_nat8_node_create_and_readback
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    uint8_t value = *(uint8_t *) test_context->value;
    
    allow_allocation(test_suite);
    modern *node = modern_node_make_nat8(library, value);
    disallow_allocation(test_suite);
    
    if(!node) return 0;
    
    int succeeded = 1;
    
    if(succeeded) {
        enum modern_node_type node_type =
            modern_node_get_node_type(library, node);
        if(node_type != nat8_value_modern_node_type)
            succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_nat8(library, node) != value)
            succeeded = 0;
    }
    
    modern *type = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        type = modern_node_get_value_type(library, node);
        disallow_allocation(test_suite);
        if(type) {
            enum modern_node_type type_node_type =
                modern_node_get_node_type(library, type);
            if(type_node_type != nat8_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    modern *universe = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        universe = modern_node_get_value_type(library, type);
        disallow_allocation(test_suite);
        if(universe) {
            enum modern_node_type universe_node_type =
                modern_node_get_node_type(library, universe);
            if(universe_node_type != universe_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    if(succeeded) {
        uint64_t level = modern_node_get_universe_type_level
            (library, universe);
        if(level != 0) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, node) != type) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, type) != universe) succeeded = 0;
    }
    
    allow_deallocation(test_suite);
    modern_release(library, node);
    disallow_deallocation(test_suite);
    
    return succeeded;
}


static int test_nat16_node_create_and_readback
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    uint16_t value = *(uint16_t *) test_context->value;
    
    allow_allocation(test_suite);
    modern *node = modern_node_make_nat16(library, value);
    disallow_allocation(test_suite);
    
    if(!node) return 0;

    int succeeded = 1;
    
    if(succeeded) {
        enum modern_node_type node_type =
            modern_node_get_node_type(library, node);
        if(node_type != nat16_value_modern_node_type)
            succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_nat16(library, node) != value)
            succeeded = 0;
    }
    
    modern *type = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        type = modern_node_get_value_type(library, node);
        disallow_allocation(test_suite);
        if(type) {
            enum modern_node_type type_node_type =
                modern_node_get_node_type(library, type);
            if(type_node_type != nat16_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    modern *universe = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        universe = modern_node_get_value_type(library, type);
        disallow_allocation(test_suite);
        if(universe) {
            enum modern_node_type universe_node_type =
                modern_node_get_node_type(library, universe);
            if(universe_node_type != universe_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    if(succeeded) {
        uint64_t level = modern_node_get_universe_type_level
            (library, universe);
        if(level != 0) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, node) != type) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, type) != universe) succeeded = 0;
    }
    
    allow_deallocation(test_suite);
    modern_release(library, node);
    disallow_deallocation(test_suite);
    
    return succeeded;
}


static int test_nat32_node_create_and_readback
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    uint32_t value = *(uint32_t *) test_context->value;
    
    allow_allocation(test_suite);
    modern *node = modern_node_make_nat32(library, value);
    disallow_allocation(test_suite);
    
    if(!node) return 0;
    
    int succeeded = 1;
    
    if(succeeded) {
        enum modern_node_type node_type =
            modern_node_get_node_type(library, node);
        if(node_type != nat32_value_modern_node_type)
            succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_nat32(library, node) != value)
            succeeded = 0;
    }
    
    modern *type = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        type = modern_node_get_value_type(library, node);
        disallow_allocation(test_suite);
        if(type) {
            enum modern_node_type type_node_type =
                modern_node_get_node_type(library, type);
            if(type_node_type != nat32_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    modern *universe = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        universe = modern_node_get_value_type(library, type);
        disallow_allocation(test_suite);
        if(universe) {
            enum modern_node_type universe_node_type =
                modern_node_get_node_type(library, universe);
            if(universe_node_type != universe_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    if(succeeded) {
        uint64_t level = modern_node_get_universe_type_level
            (library, universe);
        if(level != 0) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, node) != type) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, type) != universe) succeeded = 0;
    }
    
    allow_deallocation(test_suite);
    modern_release(library, node);
    disallow_deallocation(test_suite);
    
    return succeeded;
}


static int test_nat64_node_create_and_readback
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    uint64_t value = *(uint64_t *) test_context->value;
    
    allow_allocation(test_suite);
    modern *node = modern_node_make_nat64(library, value);
    disallow_allocation(test_suite);
    
    if(!node) return 0;
    
    int succeeded = 1;
    
    if(succeeded) {
        enum modern_node_type node_type =
            modern_node_get_node_type(library, node);
        if(node_type != nat64_value_modern_node_type)
            succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_nat64(library, node) != value)
            succeeded = 0;
    }
    
    modern *type = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        type = modern_node_get_value_type(library, node);
        disallow_allocation(test_suite);
        if(type) {
            enum modern_node_type type_node_type =
                modern_node_get_node_type(library, type);
            if(type_node_type != nat64_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    modern *universe = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        universe = modern_node_get_value_type(library, type);
        disallow_allocation(test_suite);
        if(universe) {
            enum modern_node_type universe_node_type =
                modern_node_get_node_type(library, universe);
            if(universe_node_type != universe_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    if(succeeded) {
        uint64_t level = modern_node_get_universe_type_level
            (library, universe);
        if(level != 0) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, node) != type) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, type) != universe) succeeded = 0;
    }
    
    allow_deallocation(test_suite);
    modern_release(library, node);
    disallow_deallocation(test_suite);
    
    return succeeded;
}


static int test_float32_node_create_and_readback
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    modern_float32 value = *(modern_float32 *) test_context->value;
    
    allow_allocation(test_suite);
    modern *node = modern_node_make_float32(library, value);
    disallow_allocation(test_suite);
    
    if(!node) return 0;
    
    int succeeded = 1;
    
    if(succeeded) {
        enum modern_node_type node_type =
            modern_node_get_node_type(library, node);
        if(node_type != float32_value_modern_node_type)
            succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_float32(library, node) != value)
            succeeded = 0;
    }
    
    modern *type = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        type = modern_node_get_value_type(library, node);
        disallow_allocation(test_suite);
        if(type) {
            enum modern_node_type type_node_type =
                modern_node_get_node_type(library, type);
            if(type_node_type != float32_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    modern *universe = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        universe = modern_node_get_value_type(library, type);
        disallow_allocation(test_suite);
        if(universe) {
            enum modern_node_type universe_node_type =
                modern_node_get_node_type(library, universe);
            if(universe_node_type != universe_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    if(succeeded) {
        uint64_t level = modern_node_get_universe_type_level
            (library, universe);
        if(level != 0) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, node) != type) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, type) != universe) succeeded = 0;
    }
    
    allow_deallocation(test_suite);
    modern_release(library, node);
    disallow_deallocation(test_suite);
    
    return succeeded;
}


static int test_float32_node_create_and_readback_negative_zero
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    
    modern_float32 value_in = -0.0;
    modern_float32 value_out = 0.0;
    
    allow_allocation(test_suite);
    modern *node = modern_node_make_float32(library, value_in);
    disallow_allocation(test_suite);
    
    if(!node) return 0;
    
    int succeeded = 1;
    
    if(succeeded) {
        enum modern_node_type node_type =
            modern_node_get_node_type(library, node);
        if(node_type != float32_value_modern_node_type)
            succeeded = 0;
    }
    
    if(succeeded) {
        modern_float32 actual_out = modern_node_get_float32(library, node);
        int classification = fpclassify(actual_out);
        succeeded =
            (actual_out == value_out)
            && (classification == FP_NORMAL || classification == FP_ZERO);
    }
    
    modern *type = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        type = modern_node_get_value_type(library, node);
        disallow_allocation(test_suite);
        if(type) {
            enum modern_node_type type_node_type =
                modern_node_get_node_type(library, type);
            if(type_node_type != float32_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    modern *universe = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        universe = modern_node_get_value_type(library, type);
        disallow_allocation(test_suite);
        if(universe) {
            enum modern_node_type universe_node_type =
                modern_node_get_node_type(library, universe);
            if(universe_node_type != universe_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    if(succeeded) {
        uint64_t level = modern_node_get_universe_type_level
            (library, universe);
        if(level != 0) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, node) != type) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, type) != universe) succeeded = 0;
    }
    
    allow_deallocation(test_suite);
    modern_release(library, node);
    disallow_deallocation(test_suite);
    
    return succeeded;
}


static int test_float32_node_create_expecting_error
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    
    expect_error_non_numeric_float
        (test_suite,
         test_float32_node_create_expecting_error_helper,
         test_context_in); // Never returns.
    return 0;
}


static int test_float32_node_create_expecting_error_helper
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    modern_float32 value_in = *(modern_float32 *) test_context->value;
    
    modern *node = modern_node_make_float32(library, value_in);
    
    return !node;
}


static int test_float64_node_create_and_readback
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    modern_float64 value = *(modern_float64 *) test_context->value;
    
    allow_allocation(test_suite);
    modern *node = modern_node_make_float64(library, value);
    disallow_allocation(test_suite);
    
    if(!node) return 0;
    
    int succeeded = 1;
    
    if(succeeded) {
        enum modern_node_type node_type =
            modern_node_get_node_type(library, node);
        if(node_type != float64_value_modern_node_type)
            succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_float64(library, node) != value)
            succeeded = 0;
    }
    
    modern *type = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        type = modern_node_get_value_type(library, node);
        disallow_allocation(test_suite);
        if(type) {
            enum modern_node_type type_node_type =
                modern_node_get_node_type(library, type);
            if(type_node_type != float64_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    modern *universe = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        universe = modern_node_get_value_type(library, type);
        disallow_allocation(test_suite);
        if(universe) {
            enum modern_node_type universe_node_type =
                modern_node_get_node_type(library, universe);
            if(universe_node_type != universe_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    if(succeeded) {
        uint64_t level = modern_node_get_universe_type_level
            (library, universe);
        if(level != 0) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, node) != type) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, type) != universe) succeeded = 0;
    }
    
    allow_deallocation(test_suite);
    modern_release(library, node);
    disallow_deallocation(test_suite);
    
    return succeeded;
}


static int test_float64_node_create_and_readback_negative_zero
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    
    modern_float64 value_in = -0.0;
    modern_float64 value_out = 0.0;
    
    allow_allocation(test_suite);
    modern *node = modern_node_make_float64(library, value_in);
    disallow_allocation(test_suite);
    
    if(!node) return 0;
    
    int succeeded = 1;
    
    if(succeeded) {
        enum modern_node_type node_type =
            modern_node_get_node_type(library, node);
        if(node_type != float64_value_modern_node_type)
            succeeded = 0;
    }
    
    if(succeeded) {
        modern_float64 actual_out = modern_node_get_float64(library, node);
        int classification = fpclassify(actual_out);
        succeeded =
            (actual_out == value_out)
            && (classification == FP_NORMAL || classification == FP_ZERO);
    }
    
    modern *type = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        type = modern_node_get_value_type(library, node);
        disallow_allocation(test_suite);
        if(type) {
            enum modern_node_type type_node_type =
                modern_node_get_node_type(library, type);
            if(type_node_type != float64_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    modern *universe = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        universe = modern_node_get_value_type(library, type);
        disallow_allocation(test_suite);
        if(universe) {
            enum modern_node_type universe_node_type =
                modern_node_get_node_type(library, universe);
            if(universe_node_type != universe_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    if(succeeded) {
        uint64_t level = modern_node_get_universe_type_level
            (library, universe);
        if(level != 0) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, node) != type) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, type) != universe) succeeded = 0;
    }
    
    allow_deallocation(test_suite);
    modern_release(library, node);
    disallow_deallocation(test_suite);
    
    return succeeded;
}


static int test_float64_node_create_expecting_error
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    
    expect_error_non_numeric_float
        (test_suite,
         test_float64_node_create_expecting_error_helper,
         test_context_in); // Never returns.
    return 0;
}


static int test_float64_node_create_expecting_error_helper
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    modern_float64 value_in = *(modern_float64 *) test_context->value;
    
    modern *node = modern_node_make_float64(library, value_in);
    
    return !node;
}


static int test_utf8_node_create_and_readback
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    uint8_t *value = *(uint8_t **) test_context->value;
    
    allow_allocation(test_suite);
    modern *node = modern_node_make_utf8(library, value);
    disallow_allocation(test_suite);
    
    if(!node) return 0;
    
    int succeeded = 1;
    
    if(succeeded) {
        enum modern_node_type node_type =
            modern_node_get_node_type(library, node);
        if(node_type != utf8_value_modern_node_type)
            succeeded = 0;
    }
    
    if(succeeded) {
        size_t actual_bytes = modern_node_get_utf8_bytes(library, node);
        uint8_t *actual_value =
            modern_node_get_utf8_data_piece(library, node, 0, actual_bytes);
        if(value == actual_value) succeeded = 0;
        if(strcmp(value, actual_value)) succeeded = 0;
    }
    
    modern *type = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        type = modern_node_get_value_type(library, node);
        disallow_allocation(test_suite);
        if(type) {
            enum modern_node_type type_node_type =
                modern_node_get_node_type(library, type);
            if(type_node_type != utf8_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    modern *universe = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        universe = modern_node_get_value_type(library, type);
        disallow_allocation(test_suite);
        if(universe) {
            enum modern_node_type universe_node_type =
                modern_node_get_node_type(library, universe);
            if(universe_node_type != universe_type_modern_node_type)
                succeeded = 0;
        } else succeeded = 0;
    }
    
    if(succeeded) {
        uint64_t level = modern_node_get_universe_type_level
            (library, universe);
        if(level != 0) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, node) != type) succeeded = 0;
    }
    
    if(succeeded) {
        if(modern_node_get_value_type(library, type) != universe) succeeded = 0;
    }
    
    allow_deallocation(test_suite);
    modern_release(library, node);
    disallow_deallocation(test_suite);
    
    return succeeded;
}
