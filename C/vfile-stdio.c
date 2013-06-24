#include <stdio.h>
#include "modern.h"
#include "internal.h"


struct vfile_stdio_state {
    FILE *stream;
};


HELPER void vfile_stdio_finalize
  (struct modern_library *library, void *vfile_state);
HELPER ssize_t vfile_stdio_read
  (void *vfile_state, uint8_t *buffer, size_t length);
HELPER ssize_t vfile_stdio_write
  (void *vfile_state, uint8_t *buffer, size_t length);


struct modern_vfile *modern_vfile_stdio_make
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t size = sizeof(struct modern_vfile);
    struct modern_vfile *vfile =
        library->allocator->alloc(library->client_state, size);
    if(!vfile) {
        library->error_handler->memory(library->client_state, size);
        return NULL;
    }
    
    vfile->read = vfile_stdio_read;
    vfile->write = vfile_stdio_write;
    
    return vfile;
}


void *modern_vfile_stdio_initialize
  (modern_library *library_in,
   FILE *stream)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t size = sizeof(struct vfile_stdio_state);
    struct vfile_stdio_state *vfile_state =
        library->allocator->alloc(library->client_state, size);
    if(!vfile_state) {
        library->error_handler->memory(library->client_state, size);
        return NULL;
    }
    
    vfile_state->stream = stream;
    
    return vfile_state;
}


HELPER void vfile_stdio_finalize
  (struct modern_library *library, void *vfile_state)
{
    library->allocator->free(library->client_state, vfile_state);
}


HELPER ssize_t vfile_stdio_read
  (void *vfile_state_in, uint8_t *buffer, size_t length)
{
    struct vfile_stdio_state *vfile_state =
        (struct vfile_stdio_state *) vfile_state_in;
    
    return fread(buffer, length, 1, vfile_state->stream);
}


HELPER ssize_t vfile_stdio_write
  (void *vfile_state_in, uint8_t *buffer, size_t length)
{
    struct vfile_stdio_state *vfile_state =
        (struct vfile_stdio_state *) vfile_state_in;
    
    return fwrite(buffer, length, 1, vfile_state->stream);
}

