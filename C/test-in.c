#include <stdio.h>
#include <string.h>
#include "modern.h"
#include "test.h"


struct test_context {
    test_suite *test_suite;
    modern_library *library;
};


struct stream_state {
    struct test_context *test_context;
    int succeeded;
};


static int test_explicatory_file_input
  (void *test_context);

static void stream_start
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_magic_number
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_name_definition
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t *data, size_t length);
static void stream_value_definition_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_bool
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_ordering
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_maybe_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_int8
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_int16
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_int32
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_int64
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_nat8
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_nat16
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_nat32
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_nat64
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_float32
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_float64
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_utf8
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_blob
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_function_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_sigma_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_named_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state,
   struct modern_hash name);
static void stream_type_definition_universe
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_bool_false
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_bool_true
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_ordering_less
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_ordering_equal
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_ordering_greater
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_maybe_nothing
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_maybe_just_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_int8
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int8_t value);
static void stream_int16
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int16_t value);
static void stream_int32
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int32_t value);
static void stream_int64
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int64_t value);
static void stream_nat8
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t value);
static void stream_nat16
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint16_t value);
static void stream_nat32
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint32_t value);
static void stream_nat64
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint64_t value);
static void stream_float32
  (struct modern_process *process, void *process_state,
   void *stream_state,
   float value);
static void stream_float64
  (struct modern_process *process, void *process_state,
   void *stream_state,
   double value);
static void stream_utf8_start
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_utf8_data
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t *data, size_t length);
static void stream_utf8_end
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_blob_start
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_blob_data
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t *data, size_t length);
static void stream_blob_end
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_sigma_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state,
   struct modern_hash *type);
static void stream_named_value_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, struct modern_hash name);
static void stream_lambda_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_apply_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_family_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint64_t n_items);
static void stream_let_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint64_t n_items);
static void stream_backreference_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint64_t index);
static void stream_builtin_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint16_t identifier);
static void stream_item_from_context_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, struct modern_hash type);
static void stream_end
  (struct modern_process *process, void *process_state,
   void *stream_state);


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
        stream = malloc(sizeof(struct modern_stream));
        if(!stream) {
            test_message(test_context,
              "Unable to allocate the stream callbacks.");
            succeeded = 0;
        } else {
            stream->start = stream_start;
            stream->magic_number = stream_magic_number;
            stream->name_definition = stream_name_definition;
            stream->value_definition_is_next = stream_value_definition_is_next;
            stream->type_definition_bool = stream_type_definition_bool;
            stream->type_definition_ordering = stream_type_definition_ordering;
            stream->type_definition_maybe_is_next =
              stream_type_definition_maybe_is_next;
            stream->type_definition_int8 = stream_type_definition_int8;
            stream->type_definition_int16 = stream_type_definition_int16;
            stream->type_definition_int32 = stream_type_definition_int32;
            stream->type_definition_int64 = stream_type_definition_int64;
            stream->type_definition_nat8 = stream_type_definition_nat8;
            stream->type_definition_nat16 = stream_type_definition_nat16;
            stream->type_definition_nat32 = stream_type_definition_nat32;
            stream->type_definition_nat64 = stream_type_definition_nat64;
            stream->type_definition_float32 = stream_type_definition_float32;
            stream->type_definition_float64 = stream_type_definition_float64;
            stream->type_definition_utf8 = stream_type_definition_utf8;
            stream->type_definition_blob = stream_type_definition_blob;
            stream->type_definition_function_is_next =
              stream_type_definition_function_is_next;
            stream->type_definition_sigma_is_next =
              stream_type_definition_sigma_is_next;
            stream->type_definition_named_is_next =
              stream_type_definition_named_is_next;
            stream->type_definition_universe = stream_type_definition_universe;
            stream->bool_false = stream_bool_false;
            stream->bool_true = stream_bool_true;
            stream->ordering_less = stream_ordering_less;
            stream->ordering_equal = stream_ordering_equal;
            stream->ordering_greater = stream_ordering_greater;
            stream->maybe_nothing = stream_maybe_nothing;
            stream->maybe_just_is_next = stream_maybe_just_is_next;
            stream->int8 = stream_int8;
            stream->int16 = stream_int16;
            stream->int32 = stream_int32;
            stream->int64 = stream_int64;
            stream->nat8 = stream_nat8;
            stream->nat16 = stream_nat16;
            stream->nat32 = stream_nat32;
            stream->nat64 = stream_nat64;
            stream->float32 = stream_float32;
            stream->float64 = stream_float64;
            stream->utf8_start = stream_utf8_start;
            stream->utf8_data = stream_utf8_data;
            stream->utf8_end = stream_utf8_end;
            stream->blob_start = stream_blob_start;
            stream->blob_data = stream_blob_data;
            stream->blob_end = stream_blob_end;
            stream->sigma_is_next = stream_sigma_is_next;
            stream->named_value_is_next = stream_named_value_is_next;
            stream->lambda_is_next = stream_lambda_is_next;
            stream->apply_is_next = stream_apply_is_next;
            stream->type_family_is_next = stream_type_family_is_next;
            stream->let_is_next = stream_let_is_next;
            stream->backreference_is_next = stream_backreference_is_next;
            stream->builtin_is_next = stream_builtin_is_next;
            stream->item_from_context_is_next =
              stream_item_from_context_is_next;
            stream->end = stream_end;
        }
    }
    
    struct stream_state *stream_state = NULL;
    if(succeeded) {
        stream_state = malloc(sizeof(struct stream_state));
        if(!stream_state) {
            test_message(test_context, "Unable to allocate the stream state.");
            succeeded = 0;
        }
        else {
            stream_state->test_context = test_context;
            stream_state->succeeded = 1;
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
        processor->run(process_state, stream, stream_state, vfile, vfile_state);
        if(!stream_state->succeeded) {
            test_message(test_context,
              "The stream processor did not behave as expected.");
            succeeded = 0;
        }
    }

    if(vfile_state) {
        allow_deallocation(test_suite, "test-in.c vfile state");
        modern_finalize(library, vfile_state);
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
        modern_finalize(library, process_state);
        disallow_deallocation(test_suite);
    }
    
    if(processor) {
        allow_deallocation(test_suite, "test-in.c processor callbacks");
        allocator->free(client_state, processor);
        disallow_deallocation(test_suite);
    }
    
    return succeeded;
}


void stream_start
  (struct modern_process *process, void *process_state,
   void *stream_state_in)
{
    struct stream_state *stream_state =
      (struct stream_state *) stream_state_in;
    struct test_context *test_context = stream_state->test_context;
    
    test_message(test_context, "Unexpected call to stream_start().");
}


void stream_magic_number
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_name_definition
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t *data, size_t length)
{
}


void stream_value_definition_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_bool
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_ordering
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_maybe_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_int8
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_int16
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_int32
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_int64
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_nat8
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_nat16
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_nat32
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_nat64
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_float32
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_float64
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_utf8
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_blob
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_function_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_sigma_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_definition_named_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state,
   struct modern_hash name)
{
}


void stream_type_definition_universe
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_bool_false
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_bool_true
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_ordering_less
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_ordering_equal
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_ordering_greater
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_maybe_nothing
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_maybe_just_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_int8
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int8_t value)
{
}


void stream_int16
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int16_t value)
{
}


void stream_int32
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int32_t value)
{
}


void stream_int64
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int64_t value)
{
}


void stream_nat8
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t value)
{
}


void stream_nat16
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint16_t value)
{
}


void stream_nat32
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint32_t value)
{
}


void stream_nat64
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint64_t value)
{
}


void stream_float32
  (struct modern_process *process, void *process_state,
   void *stream_state,
   float value)
{
}


void stream_float64
  (struct modern_process *process, void *process_state,
   void *stream_state,
   double value)
{
}


void stream_utf8_start
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_utf8_data
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t *data, size_t length)
{
}


void stream_utf8_end
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_blob_start
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_blob_data
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t *data, size_t length)
{
}


void stream_blob_end
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_sigma_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state,
   struct modern_hash *type)
{
}


void stream_named_value_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, struct modern_hash name)
{
}


void stream_lambda_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_apply_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


void stream_type_family_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint64_t n_items)
{
}


void stream_let_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint64_t n_items)
{
}


void stream_backreference_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint64_t index)
{
}


void stream_builtin_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint16_t identifier)
{
}


void stream_item_from_context_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, struct modern_hash type)
{
}


void stream_end
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}

