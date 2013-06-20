#include "modern.h"
#include "internal.h"


struct processor_explicatory_state {
    struct memory memory;
    struct modern_process process;
    struct modern_library *library;
    int started : 1;
    int ended : 1;
    int aborted : 1;
};


HELPER void *processor_explicatory_initialize(modern_library *library);
HELPER void processor_explicatory_finalize
  (struct modern_library *library, void *process_state);
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


struct modern_processor *modern_processor_explicatory_make
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
    
    state->memory.finalizer = processor_explicatory_finalize;
    state->process.abort = processor_explicatory_abort;
    state->process.flush = processor_explicatory_flush;
    state->library = library;
    state->started = 0;
    state->ended = 0;
    state->aborted = 0;
    
    return state;
}


HELPER void processor_explicatory_finalize
  (struct modern_library *library, void *process_state_in)
{
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
        stream->start(&process_state->process, process_state_in, stream_state);
    } else if(!process_state->ended) {
        process_state->ended = 1;
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

