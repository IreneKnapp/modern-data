#include <ctype.h>
#include <string.h>
#include "modern.h"
#include "internal.h"
#include "keywords.h"


HELPER void *processor_explicatory_initialize
    (modern_library *library_in);
HELPER void processor_explicatory_finalize
    (modern_library *library_in, void *process_state_in);
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
                        if(!(isalpha(c) || isdigit(c) || (c == '_')))
                            break;
                        token_length++;
                    }
                    void (*emit)
                        (struct processor_explicatory_state *process_state,
                         struct modern_stream *stream, void *stream_state,
                         struct modern_vfile *vfile, void *vfile_state) =
                        get_keyword(process_state->buffer, token_length);
                    
                    process_state->buffer_length -= token_length;
                    memmove(&process_state->buffer[0],
                            &process_state->buffer[token_length],
                            process_state->buffer_length);
                    
                    // TODO actually call emit and do stuff!
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


HELPER void emit_name_definition
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_value_definition
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_bool
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_ordering
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_maybe
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_int8
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_int16
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_int32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_int64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_nat8
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_nat16
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_nat32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_nat64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_float32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_float64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_utf8
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_blob
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_function
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_sigma
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_named
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_universe
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_satisfies
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_bool_false
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_bool_true
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_ordering_less
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_ordering_equal
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_ordering_greater
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_maybe_nothing
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_maybe_just
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_int8
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_int16
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_int32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_int64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_nat8
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_nat16
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_nat32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_nat64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_float32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_float64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_utf8_start
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_utf8_data
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_utf8_end
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_blob_start
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_blob_data
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_blob_end
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_sigma
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_named_value
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_lambda
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_apply
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_family
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_let
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_backreference
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_builtin
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_item_from_context
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}

