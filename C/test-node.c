#include <math.h>
#include "modern.h"
#include "test.h"


void test_main(test_suite *test_suite, modern_library *library) {
    modern_autorelease_pool *pool = modern_make_autorelease_pool(library);
    
    modern_context *context = modern_make_initial_context(library);
    
    {
        int8_t values[6] = {
            0, 1, -1, 42, INT8_MAX, INT8_MIN,
        };
        
        for(int i = 0; i < 6; i++) {
            int8_t value = values[i];
            if(begin_test_case
                (test_suite,
                 "int8 node create-and-readback with value %hhi",
                 value))
            {
                modern *node = modern_node_make_int8(library, value);
                int succeeded = modern_node_get_int8(library, node) == value;
                modern_release(library, node);
                end_test_case(test_suite, succeeded);
            }
        }
    }
    
    {
        int16_t values[8] = {
            0, 1, -1, 42, INT8_MAX, INT8_MIN,
            INT16_MAX, INT16_MIN,
        };
        
        for(int i = 0; i < 8; i++) {
            int16_t value = values[i];
            if(begin_test_case
                (test_suite,
                 "int16 node create-and-readback with value %hi",
                 value))
            {
                modern *node = modern_node_make_int16(library, value);
                int succeeded = modern_node_get_int16(library, node) == value;
                modern_release(library, node);
                end_test_case(test_suite, succeeded);
            }
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
            if(begin_test_case
                (test_suite,
                 "int32 node create-and-readback with value %li",
                 value))
            {
                modern *node = modern_node_make_int32(library, value);
                int succeeded = modern_node_get_int32(library, node) == value;
                modern_release(library, node);
                end_test_case(test_suite, succeeded);
            }
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
            if(begin_test_case
                (test_suite,
                 "int64 node create-and-readback with value %lli",
                 value))
            {
                modern *node = modern_node_make_int64(library, value);
                int succeeded = modern_node_get_int64(library, node) == value;
                modern_release(library, node);
                end_test_case(test_suite, succeeded);
            }
        }
    }
    
    {
        uint8_t values[5] = {
            0, 1, -1, 42, UINT8_MAX,
        };
        
        for(int i = 0; i < 5; i++) {
            uint8_t value = values[i];
            if(begin_test_case
                (test_suite,
                 "nat8 node create-and-readback with value %hhu",
                 value))
            {
                modern *node = modern_node_make_nat8(library, value);
                int succeeded = modern_node_get_nat8(library, node) == value;
                modern_release(library, node);
                end_test_case(test_suite, succeeded);
            }
        }
    }
    
    {
        uint16_t values[6] = {
            0, 1, -1, 42, UINT8_MAX,
            UINT16_MAX,
        };
        
        for(int i = 0; i < 6; i++) {
            uint16_t value = values[i];
            if(begin_test_case
                (test_suite,
                 "nat16 node create-and-readback with value %hu",
                 value))
            {
                modern *node = modern_node_make_nat16(library, value);
                int succeeded = modern_node_get_nat16(library, node) == value;
                modern_release(library, node);
                end_test_case(test_suite, succeeded);
            }
        }
    }
    
    {
        uint32_t values[7] = {
            0, 1, -1, 42, UINT8_MAX,
            UINT16_MAX,
            UINT32_MAX,
        };
        
        for(int i = 0; i < 7; i++) {
            uint32_t value = values[i];
            if(begin_test_case
                (test_suite,
                 "nat32 node create-and-readback with value %lu",
                 value))
            {
                modern *node = modern_node_make_nat32(library, value);
                int succeeded = modern_node_get_nat32(library, node) == value;
                modern_release(library, node);
                end_test_case(test_suite, succeeded);
            }
        }
    }
    
    {
        uint64_t values[12] = {
            0, 1, -1, 42, UINT8_MAX,
            UINT16_MAX,
            UINT32_MAX,
            UINT64_MAX,
        };
        
        for(int i = 0; i < 12; i++) {
            uint64_t value = values[i];
            if(begin_test_case
                (test_suite,
                 "nat64 node create-and-readback with value %llu",
                 value))
            {
                modern *node = modern_node_make_int64(library, value);
                int succeeded = modern_node_get_int64(library, node) == value;
                modern_release(library, node);
                end_test_case(test_suite, succeeded);
            }
        }
    }
    
    {
        modern_float32 values[17] = {
          0.0,
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
            modern_float32 value = values[i];
            if(begin_test_case
                (test_suite,
                 "float32 node create-and-readback with value %f",
                 value))
            {
                modern *node = modern_node_make_float32(library, value);
                int succeeded = modern_node_get_float32(library, node) == value;
                modern_release(library, node);
                end_test_case(test_suite, succeeded);
            }
        }
    }
    
    {
        modern_float32 value_in = -0.0;
        modern_float32 value_out = 0.0;
        if(begin_test_case
            (test_suite,
             "float32 node create-and-readback special case for negative zero"))
        {
            modern *node = modern_node_make_float32(library, value_in);
            int succeeded = modern_node_get_float32(library, node) == value_out;
            modern_release(library, node);
            end_test_case(test_suite, succeeded);
        }
    }
    
    {
        modern_float64 values[21] = {
          0.0,
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
            modern_float64 value = values[i];
            if(begin_test_case
                (test_suite,
                 "float64 node create-and-readback with value %lf",
                 value))
            {
                modern *node = modern_node_make_float64(library, value);
                int succeeded = modern_node_get_float64(library, node) == value;
                modern_release(library, node);
                end_test_case(test_suite, succeeded);
            }
        }
    }
    
    {
        modern_float64 value_in = -0.0;
        modern_float64 value_out = 0.0;
        if(begin_test_case
            (test_suite,
             "float64 node create-and-readback special case for negative zero"))
        {
            modern *node = modern_node_make_float64(library, value_in);
            int succeeded = modern_node_get_float64(library, node) == value_out;
            modern_release(library, node);
            end_test_case(test_suite, succeeded);
        }
    }
}
