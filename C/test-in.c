#include <string.h>
#include "modern.h"
#include "test.h"


struct test_context {
    test_suite *test_suite;
    modern_library *library;
    struct modern_processor *processor;
};


struct stream_state {
};


static int test_explicatory_file_input
  (void *test_context);


void test_main(test_suite *test_suite, modern_library *library) {
    if(!begin_fixtures(test_suite)) return;
    
    allow_allocation(test_suite);
    struct modern_processor *processor =
        modern_processor_explicatory_make(library);
    disallow_allocation(test_suite);
    
    end_fixtures(test_suite);
    
    struct test_context test_context;
    test_context.test_suite = test_suite;
    test_context.library = library;
    test_context.processor = processor;
    
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
    struct modern_processor *processor = test_context->processor;
    
    int succeeded = 1;
    
    void *process_state = NULL;
    if(succeeded) {
        allow_allocation(test_suite);
        process_state = processor->initialize(library);
        if(!process_state) succeeded = 0;
        disallow_allocation(test_suite);
    }
    
    struct modern_stream *stream = NULL;
    if(succeeded) {
        stream = malloc(sizeof(struct modern_stream));
        if(!stream) succeeded = 0;
    }
    
    struct stream_state *stream_state = NULL;
    if(succeeded) {
        stream_state = malloc(sizeof(struct stream_state));
        if(!stream_state) succeeded = 0;
    }
    
    FILE *file = NULL;
    if(succeeded) {
        fopen("input.modern-explicatory", "r");
        if(!file) succeeded = 0;
    }
    
    if(succeeded) {
        allow_allocation(test_suite);
        struct modern_vfile *vfile = modern_vfile_stdio_make(library);
        disallow_allocation(test_suite);
        
        allow_allocation(test_suite);
        void *vfile_state = modern_vfile_stdio_initialize(library, file);
        disallow_allocation(test_suite);
        
        processor->run(process_state, stream, stream_state, vfile, vfile_state);
        
        allow_deallocation(test_suite);
        modern_vfile_stdio_finalize(library, vfile);
        disallow_deallocation(test_suite);
    }
    
    if(file) fclose(file);
    
    if(stream_state) free(stream_state);
    if(stream) free(stream);
    
    allow_deallocation(test_suite);
    processor->finalize(process_state);
    disallow_deallocation(test_suite);
    
    return 1;
}
