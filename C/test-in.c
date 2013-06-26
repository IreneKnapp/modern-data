#include <stdio.h>
#include <string.h>
#include "modern.h"
#include "test.h"


struct test_context {
    test_suite *test_suite;
    modern_library *library;
};


static int test_explicatory_file_input
  (void *test_context);


void test_main(test_suite *test_suite, modern_library *library) {
    struct test_context test_context;
    test_context.test_suite = test_suite;
    test_context.library = library;
    
    begin_test_case
        (test_suite,
         test_explicatory_file_input,
         (void *) &test_context,
         "explicatory file input");
}


static int test_explicatory_file_input
  (void *test_context_in)
{
    struct test_context *test_context =
        (struct test_context *) test_context_in;
    test_suite *test_suite = test_context->test_suite;
    modern_library *library = test_context->library;    
    struct modern_allocator *allocator = modern_library_allocator_get(library);
    void *client_state = modern_library_client_state_get(library);
    
    int succeeded = 1;
    
    struct modern_processor *processor = NULL;
    if(succeeded) {
        allow_allocation(test_suite, "test-in.c processor callbacks");
        processor = modern_processor_explicatory_make(library);
        disallow_allocation(test_suite);
        if(!processor) {
            test_message(test_context, "Unable to initialize the processor.");
            succeeded = 0;
        }
    }
    
    void *process_state = NULL;
    if(succeeded) {
        allow_allocation(test_suite, "test-in.c processor state");
        process_state = processor->initialize(library);
        disallow_allocation(test_suite);
        if(!process_state) {
            test_message(test_context,
                "Unable to initialize the processor state.");
            succeeded = 0;
        }
    }
    
    struct modern_stream *stream = NULL;
    if(succeeded) {
        stream = test_stream_make(test_suite);
        if(!stream) {
            test_message(test_suite, "Unable to make the stream callbacks.");
            succeeded = 0;
        }
    }
    
    void *stream_state = NULL;
    if(succeeded) {
        stream_state = test_stream_initialize(test_suite);
        if(!stream_state) {
            test_message(test_suite, "Unable to make the stream state.");
            succeeded = 0;
        }
    }
    
    FILE *file = NULL;
    if(succeeded) {
        file = fopen("input.modern-explicatory", "r");
        if(!file) {
            test_message(test_context, "Unable to open the input file.");
            succeeded = 0;
        }
    }
    
    struct modern_vfile *vfile = NULL;
    if(succeeded) {
        allow_allocation(test_suite, "test-in.c vfile callbacks");
        vfile = modern_vfile_stdio_make(library);
        disallow_allocation(test_suite);
        if(!vfile) {
            test_message(test_context, "Unable to initialize the vfile.");
            succeeded = 0;
        }
    }
    
    void *vfile_state = NULL;
    if(succeeded) {
        allow_allocation(test_suite, "test-in.c vfile state");
        vfile_state = modern_vfile_stdio_initialize(library, file);
        disallow_allocation(test_suite);
        if(!vfile_state) succeeded = 0;
    }
    
    if(succeeded) {
        expect_stream_start(test_suite, stream_state);
        expect_stream_type_family_is_next(test_suite, stream_state, 2);
        expect_stream_type_definition_sigma_is_next(test_suite, stream_state);
        expect_stream_type_definition_int8(test_suite, stream_state);
        expect_stream_lambda_is_next(test_suite, stream_state);
        expect_stream_type_definition_int8(test_suite, stream_state);
        expect_stream_lambda_is_next(test_suite, stream_state);
        expect_stream_type_definition_satisfies_is_next
          (test_suite, stream_state);
        expect_stream_type_definition_int8(test_suite, stream_state);
        expect_stream_lambda_is_next(test_suite, stream_state);
        expect_stream_apply_is_next(test_suite, stream_state);
        expect_stream_builtin(test_suite, stream_state,
            modern_builtin_identifier_not_bool);
        expect_stream_apply_is_next(test_suite, stream_state);
        expect_stream_apply_is_next(test_suite, stream_state);
        expect_stream_builtin(test_suite, stream_state,
            modern_builtin_identifier_equal_to_int8);
        expect_stream_apply_is_next(test_suite, stream_state);
        expect_stream_apply_is_next(test_suite, stream_state);
        expect_stream_builtin(test_suite, stream_state,
            modern_builtin_identifier_compare_int8);
        expect_stream_backreference(test_suite, stream_state, 1);
        expect_stream_backreference(test_suite, stream_state, 0);
        expect_stream_ordering_less(test_suite, stream_state);
        expect_stream_type_definition_sigma_is_next(test_suite, stream_state);
        expect_stream_type_definition_int8(test_suite, stream_state);
        expect_stream_backreference(test_suite, stream_state, 0);
        expect_stream_end(test_suite, stream_state);
        processor->run(process_state, stream, stream_state, vfile, vfile_state);
        
        flush_expectations(test_suite);
    }

    if(vfile_state) {
        allow_deallocation(test_suite, "test-in.c vfile state");
        modern_vfile_stdio_finalize(library, vfile_state);
        disallow_deallocation(test_suite);
    }
    
    if(vfile) {
        allow_deallocation(test_suite, "test-in.c vfile callbacks");
        allocator->free(client_state, vfile);
        disallow_deallocation(test_suite);
    }
    
    if(file) fclose(file);
    
    if(stream_state) free(stream_state);
    if(stream) free(stream);
    
    if(process_state) {
        allow_deallocation(test_suite, "test-in.c processor state");
        processor->finalize(library, process_state);
        disallow_deallocation(test_suite);
    }
    
    if(processor) {
        allow_deallocation(test_suite, "test-in.c processor callbacks");
        allocator->free(client_state, processor);
        disallow_deallocation(test_suite);
    }
    
    return succeeded;
}
