#include <dirent.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>


enum mode {
    mode_help,
    mode_amalgamation,
    mode_binary,
    mode_test,
    mode_debug,
    mode_clean,
};


enum file_type {
    file_type_unknown = 0,
    file_type_header,
    file_type_source,
};


struct print_state {
    size_t indent;
    void **backreferences;
    struct print_state *parent;
} *print_state = NULL;


struct project {
    char *name;
    struct executable **executables;
    struct library **libraries;
};


struct executable {
    char *name;
    struct directory *directory;
    struct executable **tools;
    struct library **libraries;
    struct object **objects;
};


struct library {
    char *base_name;
    struct directory *directory;
    struct executable **tools;
    struct library **libraries;
    struct object **objects;
    struct header **headers;
};


struct object {
    char *base_name;
    struct provenance *provenance;
    struct header **headers;
    struct source **sources;
};


struct header {
    char *filename;
    struct provenance *provenance;
    struct header **headers;
};


struct source {
    char *filename;
    struct provenance *provenance;
    struct header **headers;
};


struct directory {
    char *path;
    struct header **headers;
    struct source **sources;
};


enum provenance_type {
    provenance_type_directory,
    provenance_type_step,
};


struct provenance {
    enum provenance_type type;
    union {
        struct {
            struct directory *directory;
        } directory;
        struct {
            struct build_step *build_step;
        } build_step;
    } specifics;
};


enum build_step_type {
    build_step_type_foo,
};


struct build_step {
    enum build_step_type type;
};


void help();
void print(void (*printer)(void *), void *scrutinant);
void print_line(char *format, ...);

struct project *project_initialize(char *name);
void project_print(struct project *project);
struct project *project_prepare();
void project_executable_add
    (struct project *project, struct executable *executable);
void project_library_add(struct project *project, struct library *library);
void project_amalgamation(struct project *project);
void project_binary(struct project *project);
void project_test(struct project *project);
void project_debug(struct project *project);
void project_clean(struct project *project);

struct executable *executable_initialize(char *name, char *path);
void executable_print(struct executable *executable);

struct library *library_initialize(char *base_name, char *path);
void library_print(struct library *library);
void library_tool_add(struct library *library, struct executable *tool);
void library_library_add(struct library *library, struct library *sub_library);
void library_object_add(struct library *library, struct object *object);
void library_header_add(struct library *library, struct header *header);

struct directory *directory_initialize(char *path);
void directory_print(struct directory *directory);
void directory_scan(struct directory *directory);
void directory_header_add(struct directory *directory, struct header *header);
void directory_source_add(struct directory *directory, struct source *source);

void object_print(struct object *object);

void header_print(struct header *header);

void source_print(struct source *source);

struct provenance *provenance_directory_initialize
    (struct directory *directory);
void provenance_print
    (struct provenance *provenance);

void build_step_print(struct build_step *build_step);


int main(int argc, char **argv) {
    enum mode mode = mode_help;
    int error = 0;
    for(int i = 1; i < argc; i++) {
        if(!strcmp(argv[i], "help")) {
            mode = mode_help;
        } else if(!strcmp(argv[i], "amalgamation")) {
            mode = mode_amalgamation;
        } else if(!strcmp(argv[i], "binary")) {
            mode = mode_binary;
        } else if(!strcmp(argv[i], "test")) {
            mode = mode_test;
        } else if(!strcmp(argv[i], "debug")) {
            mode = mode_debug;
        } else if(!strcmp(argv[i], "clean")) {
            mode = mode_clean;
        } else {
            error = 1;
        }
    }
    if(error) mode = mode_help;
    
    struct project *project = NULL;
    if(mode != mode_help) project = project_prepare();
    if(project) print((void (*)(void *)) project_print, project);
    
    switch(mode) {
    case mode_help: help(); break;
    case mode_amalgamation: project_amalgamation(project); break;
    case mode_binary: project_binary(project); break;
    case mode_test: project_test(project); break;
    case mode_debug: project_debug(project); break;
    case mode_clean: project_clean(project); break;
    }
    
    if(error) return 1;
    else return 0;
}


void help() {
    printf("Usage: ./build <command>\n");
    printf("\n");
    printf("Commands:\n");
    printf("    help               This message.\n");
    printf("    amalgamation       Build the source-code amalgamation.\n");
    printf("    binary             Build the binary library.\n");
    printf("    test               Build and run the test suite.\n");
    printf("    project_clean              Remove all build products.\n");
    printf("\n");
}


void print(void (*printer)(void *), void *scrutinant) {
    {
        struct print_state *new_print_state =
            malloc(sizeof(struct print_state));
        new_print_state->parent = print_state;
        print_state = new_print_state;
    }

    if(print_state->parent) {
        print_state->indent = print_state->parent->indent + 1;
        print_state->backreferences = print_state->parent->backreferences;
    } else {
        print_state->indent = 0;
        print_state->backreferences = malloc(sizeof(void *) * 1);
        print_state->backreferences[0] = NULL;
    }

    size_t i;
    for(i = 0; print_state->backreferences[i]; i++) {
        if(print_state->backreferences[i] == scrutinant) break;
    }
    if(print_state->backreferences[i]) {
        print_line("#%llu#", (unsigned long long) i);
    } else {
        size_t count;
        for(count = 0; print_state->backreferences[count]; count++);
        print_state->backreferences =
            realloc(print_state->backreferences, sizeof(void *) * (count + 2));
        print_state->backreferences[count] = scrutinant;
        print_state->backreferences[count + 1] = NULL;
        
        printer(scrutinant);
    }

    if(print_state->parent) {
        print_state->parent->backreferences = print_state->backreferences;
    } else {
        free(print_state->backreferences);
    }
    
    {
        struct print_state *temporary_print_state = print_state->parent;
        free(print_state);
        print_state = temporary_print_state;
    }
}


void print_line(char *format, ...) {
    va_list ap;
    va_start(ap, format);
    
    if(print_state) {
        for(int i = 0; i < print_state->indent; i++) printf("    ");
    }
    vprintf(format, ap);
    printf("\n");
    
    va_end(ap);
}


struct project *project_initialize(char *name) {
    struct project *project = malloc(sizeof(struct project));
    project->name = strdup(name);
    project->executables = malloc(sizeof(struct executable *) * 1);
    project->executables[0] = NULL;
    project->libraries = malloc(sizeof(struct library *) * 1);
    project->libraries[0] = NULL;
    return project;
}


void project_print(struct project *project) {
    print_line("Project: %s", project->name);
    
    for(struct executable **executable = project->executables;
        *executable;
        executable++)
    {
        print((void (*)(void *)) executable_print, *executable);
    }
    
    for(struct library **library = project->libraries;
        *library;
        library++)
    {
        print((void (*)(void *)) library_print, *library);
    }
}


struct project *project_prepare() {
    struct project *project = project_initialize("Modern Data");
    
    struct library *main_library = library_initialize("modern", "library/");
    project_library_add(project, main_library);
    
    struct executable *make_keywords_executable =
        executable_initialize("make-keywords", "tools/make-keywords/");
    library_tool_add(main_library, make_keywords_executable);

    return project;
}


void project_executable_add
    (struct project *project, struct executable *executable)
{
    size_t count = 0;
    for(struct executable **i = project->executables; *i; i++, count++);
    project->executables = realloc
        (project->executables, sizeof(struct executable *) * (count + 2));
    project->executables[count] = executable;
    project->executables[count + 1] = NULL;
}


void project_library_add(struct project *project, struct library *library) {
    size_t count = 0;
    for(struct library **i = project->libraries; *i; i++, count++);
    project->libraries = realloc
        (project->libraries, sizeof(struct library *) * (count + 2));
    project->libraries[count] = library;
    project->libraries[count + 1] = NULL;
}


struct executable *executable_initialize(char *name, char *path) {
    struct executable *executable = malloc(sizeof(struct executable));
    executable->name = strdup(name);
    executable->directory = directory_initialize(path);
    executable->tools = malloc(sizeof(struct executable *) * 1);
    executable->tools[0] = NULL;
    executable->libraries = malloc(sizeof(struct library *) * 1);
    executable->libraries[0] = NULL;
    executable->objects = malloc(sizeof(struct object *) * 1);
    executable->objects[0] = NULL;
    return executable;
}


void executable_print(struct executable *executable) {
    print_line("Executable: %s", executable->name);

    print((void (*)(void *)) directory_print, executable->directory);

    for(struct executable **tool = executable->tools;
        *tool;
        tool++)
    {
        print((void (*)(void *)) executable_print, *tool);
    }
    
    for(struct library **library = executable->libraries;
        *library;
        library++)
    {
        print((void (*)(void *)) library_print, *library);
    }
    
    for(struct object **object = executable->objects;
        *object;
        object++)
    {
        print((void (*)(void *)) object_print, *object);
    }
}


struct library *library_initialize(char *base_name, char *path) {
    struct library *library = malloc(sizeof(struct library));
    library->base_name = strdup(base_name);
    library->directory = directory_initialize(path);
    library->tools = malloc(sizeof(struct executable *) * 1);
    library->tools[0] = NULL;
    library->libraries = malloc(sizeof(struct library *) * 1);
    library->libraries[0] = NULL;
    library->objects = malloc(sizeof(struct object *) * 1);
    library->objects[0] = NULL;
    library->headers = malloc(sizeof(struct headers *) * 1);
    library->headers[0] = NULL;
    return library;
}


void library_print(struct library *library) {
    print_line("Library: %s", library->base_name);
    
    print((void (*)(void *)) directory_print, library->directory);

    for(struct executable **tool = library->tools;
        *tool;
        tool++)
    {
        print((void (*)(void *)) executable_print, *tool);
    }
    
    for(struct library **sub_library = library->libraries;
        *sub_library;
        sub_library++)
    {
        print((void (*)(void *)) library_print, *sub_library);
    }
    
    for(struct object **object = library->objects;
        *object;
        object++)
    {
        print((void (*)(void *)) object_print, *object);
    }
    
    for(struct header **header = library->headers;
        *header;
        header++)
    {
        print((void (*)(void *)) header_print, *header);
    }
}


void library_tool_add(struct library *library, struct executable *tool) {
    size_t count = 0;
    for(struct executable **i = library->tools; *i; i++, count++);
    library->tools = realloc
        (library->tools, sizeof(struct executable *) * (count + 2));
    library->tools[count] = tool;
    library->tools[count + 1] = NULL;
}


void library_library_add(struct library *library, struct library *sub_library)
{
    size_t count = 0;
    for(struct library **i = library->libraries; *i; i++, count++);
    library->libraries = realloc
        (library->libraries, sizeof(struct library *) * (count + 2));
    library->libraries[count] = sub_library;
    library->libraries[count + 1] = NULL;
}


void library_object_add(struct library *library, struct object *object) {
    size_t count = 0;
    for(struct object **i = library->objects; *i; i++, count++);
    library->objects = realloc
        (library->objects, sizeof(struct object *) * (count + 2));
    library->objects[count] = object;
    library->objects[count + 1] = NULL;
}


void library_header_add(struct library *library, struct header *header) {
    size_t count = 0;
    for(struct header **i = library->headers; *i; i++, count++);
    library->headers = realloc
        (library->headers, sizeof(struct header *) * (count + 2));
    library->headers[count] = header;
    library->headers[count + 1] = NULL;
}


struct directory *directory_initialize(char *path) {
    struct directory *directory = malloc(sizeof(struct directory));
    directory->path = strdup(path);
    directory->headers = malloc(sizeof(struct header *) * 1);
    directory->headers[0] = NULL;
    directory->sources = malloc(sizeof(struct source *) * 1);
    directory->sources[0] = NULL;
    
    directory_scan(directory);
    
    return directory;
}


void directory_print(struct directory *directory) {
    print_line("Directory: %s", directory->path);
    
    for(struct header **header = directory->headers;
        *header;
        header++)
    {
        print((void (*)(void *)) header_print, *header);
    }
    
    for(struct source **source = directory->sources;
        *source;
        source++)
    {
        print((void (*)(void *)) source_print, *source);
    }
}


void directory_scan(struct directory *directory) {
    struct provenance *provenance = NULL;
    
    DIR *handle = opendir(directory->path);
    while(1) {
        struct dirent *entry = readdir(handle);
        if(!entry) break;
        if(!strcmp(entry->d_name, ".")) continue;
        if(!strcmp(entry->d_name, "..")) continue;
        
        enum file_type type = file_type_unknown;
        
        size_t length = strlen(entry->d_name);
        if((length > 2) && !strcmp(entry->d_name + length - 2, ".h")) {
            type = file_type_header;
        } else if((length > 2) && !strcmp(entry->d_name + length - 2, ".c")) {
            type = file_type_source;
        }
        
        char *filename = NULL;
        if(type != file_type_unknown) {
            filename = malloc(strlen(directory->path) + length + 1);
            strcpy(filename, directory->path);
            strcat(filename, entry->d_name);
            
            if(!provenance) {
                provenance = provenance_directory_initialize(directory);
            }
        }
        
        if(type == file_type_header) {
            struct header *header = malloc(sizeof(struct header));
            header->filename = filename;
            header->provenance = provenance;
            header->headers = malloc(sizeof(struct header *) * 1);
            header->headers[0] = NULL;
            directory_header_add(directory, header);
        } else if(type == file_type_source) {
            struct source *source = malloc(sizeof(struct source));
            source->filename = filename;
            source->provenance = provenance;
            source->headers = malloc(sizeof(struct header *) * 1);
            source->headers[0] = NULL;
            directory_source_add(directory, source);
        }
    }
    closedir(handle);
}


void directory_header_add(struct directory *directory, struct header *header) {
    size_t count = 0;
    for(struct header **i = directory->headers; *i; i++, count++);
    directory->headers = realloc
        (directory->headers, sizeof(struct header *) * (count + 2));
    directory->headers[count] = header;
    directory->headers[count + 1] = NULL;
}


void directory_source_add(struct directory *directory, struct source *source) {
    size_t count = 0;
    for(struct source **i = directory->sources; *i; i++, count++);
    directory->sources = realloc
        (directory->sources, sizeof(struct source *) * (count + 2));
    directory->sources[count] = source;
    directory->sources[count + 1] = NULL;
}


void project_amalgamation(struct project *project) {
}


void project_binary(struct project *project) {
}


void project_test(struct project *project) {
}


void project_debug(struct project *project) {
}


void project_clean(struct project *project) {
}


void object_print(struct object *object) {
    print_line("Object: %s", object->base_name);
    
    print((void (*)(void *)) provenance_print, object->provenance);
    
    for(struct header **header = object->headers;
        *header;
        header++)
    {
        print((void (*)(void *)) header_print, *header);
    }
    
    for(struct source **source = object->sources;
        *source;
        source++)
    {
        print((void (*)(void *)) source_print, *source);
    }
}


void header_print(struct header *header) {
    print_line("Header: %s", header->filename);

    print((void (*)(void *)) provenance_print, header->provenance);

    for(struct header **sub_header = header->headers;
        *sub_header;
        sub_header++)
    {
        print((void (*)(void *)) header_print, *sub_header);
    }
}


void source_print(struct source *source) {
    print_line("Source: %s", source->filename);

    print((void (*)(void *)) provenance_print, source->provenance);

    for(struct header **header = source->headers;
        *header;
        header++)
    {
        print((void (*)(void *)) header_print, *header);
    }
}


struct provenance *provenance_directory_initialize
    (struct directory *directory)
{
    struct provenance *provenance = malloc(sizeof(struct provenance));
    provenance->type = provenance_type_directory;
    provenance->specifics.directory.directory = directory;
    return provenance;
}


void provenance_print(struct provenance *provenance) {
    print_line("Provenance:");

    switch(provenance->type) {
    case provenance_type_directory:
        print((void (*)(void *)) directory_print,
              provenance->specifics.directory.directory);
        break;
    case provenance_type_step:
        print((void (*)(void *)) build_step_print,
              provenance->specifics.build_step.build_step);
        break;
    }
}


void build_step_print(struct build_step *build_step) {
    print_line("Build Step:");
}

