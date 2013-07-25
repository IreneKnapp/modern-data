#include <ctype.h>
#include <string.h>
#include "modern.h"
#include "internal.h"


HELPER void *processor_explicatory_initialize(modern_library *library);
HELPER void processor_explicatory_finalize
  (modern_library *library_in, void *process_state);
HELPER void processor_explicatory_abort(void *process_state);
HELPER void processor_explicatory_flush(void *process_state);
HELPER void processor_explicatory_step
  (void *process_state,
   struct modern_stream *stream, void *stream_state,
   struct modern_vfile *vfile, void *vfile_state);
HELPER void processor_explicatory_run
  (void *process_state,
   struct modern_stream *stream, void *stream_state,
   struct modern_vfile *vfile, void *vfile_state);
HELPER void processor_explicatory_extend_buffer
  (struct processor_explicatory_state *process_state,
   size_t desired_size);


HELPER uint8_t *raw_keywords[] = {
    (uint8_t *) "name_definition",
    (uint8_t *) "value_definition_is_next",
    (uint8_t *) "type_definition_bool",
    (uint8_t *) "type_definition_ordering",
    (uint8_t *) "type_definition_maybe_is_next",
    (uint8_t *) "type_definition_int8",
    (uint8_t *) "type_definition_int16",
    (uint8_t *) "type_definition_int32",
    (uint8_t *) "type_definition_int64",
    (uint8_t *) "type_definition_nat8",
    (uint8_t *) "type_definition_nat16",
    (uint8_t *) "type_definition_nat32",
    (uint8_t *) "type_definition_nat64",
    (uint8_t *) "type_definition_float32",
    (uint8_t *) "type_definition_float64",
    (uint8_t *) "type_definition_utf8",
    (uint8_t *) "type_definition_blob",
    (uint8_t *) "type_definition_function_is_next",
    (uint8_t *) "type_definition_sigma_is_next",
    (uint8_t *) "type_definition_named_is_next",
    (uint8_t *) "type_definition_universe",
    (uint8_t *) "type_definition_satisfies_is_next",
    (uint8_t *) "bool_false",
    (uint8_t *) "bool_true",
    (uint8_t *) "ordering_less",
    (uint8_t *) "ordering_equal",
    (uint8_t *) "ordering_greater",
    (uint8_t *) "maybe_nothing",
    (uint8_t *) "maybe_just_is_next",
    (uint8_t *) "int8",
    (uint8_t *) "int16",
    (uint8_t *) "int32",
    (uint8_t *) "int64",
    (uint8_t *) "nat8",
    (uint8_t *) "nat16",
    (uint8_t *) "nat32",
    (uint8_t *) "nat64",
    (uint8_t *) "float32",
    (uint8_t *) "float64",
    (uint8_t *) "utf8_start",
    (uint8_t *) "utf8_data",
    (uint8_t *) "utf8_end",
    (uint8_t *) "blob_start",
    (uint8_t *) "blob_data",
    (uint8_t *) "blob_end",
    (uint8_t *) "sigma_is_next",
    (uint8_t *) "named_value_is_next",
    (uint8_t *) "lambda_is_next",
    (uint8_t *) "apply_is_next",
    (uint8_t *) "type_family_is_next",
    (uint8_t *) "let_is_next",
    (uint8_t *) "backreference",
    (uint8_t *) "builtin",
    (uint8_t *) "item_from_context",
    NULL,
};


INTERNAL void
    initialize_processor_explicatory
    (struct modern_library *library)
{
    printf("Hmm\n");
}


INTERNAL void
    finalize_processor_explicatory
    (struct modern_library *library)
{
}


PUBLIC struct modern_processor *modern_processor_explicatory_make
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t size = sizeof(struct modern_processor);
    struct modern_processor *processor =
        library->allocator->alloc(library->client_state, size);
    if(!processor) {
        library->error_handler->memory(library->client_state, size);
        return NULL;
    }
    
    processor->initialize = processor_explicatory_initialize;
    processor->finalize = processor_explicatory_finalize;
    processor->step = processor_explicatory_step;
    processor->run = processor_explicatory_run;
    
    return processor;
}


HELPER void *processor_explicatory_initialize
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t size = sizeof(struct processor_explicatory_state);
    struct processor_explicatory_state *state =
        library->allocator->alloc(library->client_state, size);
    if(!state) {
        library->error_handler->memory(library->client_state, size);
        return NULL;
    }
    
    state->process.abort = processor_explicatory_abort;
    state->process.flush = processor_explicatory_flush;
    state->library = library;
    state->started = 0;
    state->ended = 0;
    state->aborted = 0;
    
    return state;
}


HELPER void processor_explicatory_finalize
  (modern_library *library_in, void *process_state_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    library->allocator->free(library->client_state, process_state_in);
}


HELPER void processor_explicatory_abort(void *process_state)
{
}


HELPER void processor_explicatory_flush(void *process_state)
{
}


HELPER void processor_explicatory_step
  (void *process_state_in,
   struct modern_stream *stream, void *stream_state,
   struct modern_vfile *vfile, void *vfile_state)
{
    struct processor_explicatory_state *process_state =
        (struct processor_explicatory_state *) process_state_in;
    struct modern_library *library = process_state->library;
    
    process_state->aborted = 0;
    
    if(!process_state->started) {
        process_state->started = 1;
        process_state->buffer_length = 0;
        
        stream->start(&process_state->process, process_state_in, stream_state);
    } else {
        while(1) {
            if(!process_state->buffer_length) {
                size_t bytes_read = vfile->read
                    (vfile_state,
                     process_state->buffer, sizeof(process_state->buffer));
                
                if(!bytes_read) {
                    if(!process_state->ended) {
                        process_state->ended = 1;
                        
                        stream->end(&process_state->process, process_state_in,
                                    stream_state);
                    }
                    
                    break;
                } else {
                    process_state->buffer_length += bytes_read;
                }
            }
            
            if(process_state->buffer_length) {
                uint8_t c = process_state->buffer[0];
                if(isspace(c)) {
                    process_state->buffer_length--;
                    if(process_state->buffer_length) {
                        memmove(&process_state->buffer[0],
                                &process_state->buffer[1],
                                process_state->buffer_length);
                    }
                } else if(isalpha(c) || isdigit(c) || (c == '_')) {
                    size_t token_length = 1;
                    while(token_length < process_state->buffer_length) {
                        c = process_state->buffer[token_length];
                        printf("'%c'\n", c);
                        if(!(isalpha(c) || isdigit(c) || (c == '_')))
                            break;
                        token_length++;
                    }
                }
            }
        }
    }
}


HELPER void processor_explicatory_run
  (void *process_state_in,
   struct modern_stream *stream, void *stream_state,
   struct modern_vfile *vfile, void *vfile_state)
{
    struct processor_explicatory_state *process_state =
        (struct processor_explicatory_state *) process_state_in;
    
    process_state->aborted = 0;
    
    while(!process_state->ended && !process_state->aborted)
    {
        processor_explicatory_step
            (process_state_in, stream, stream_state, vfile, vfile_state);
    }
}


HELPER void processor_explicatory_extend_buffer
  (struct processor_explicatory_state *process_state,
   size_t desired_size)
{
}

