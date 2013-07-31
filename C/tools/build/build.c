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
    struct tool_invocation **tool_invocations;
    struct library **libraries;
    struct object **objects;
};


struct library {
    char *base_name;
    struct directory *directory;
    struct executable **tools;
    struct tool_invocation **tool_invocations;
    struct library **libraries;
    struct object **objects;
    struct header **headers;
};


struct object {
    char *base_name;
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


struct tool_invocation {
    struct executable *tool;
    char **parameters;
    struct header **header_outputs;
};


enum provenance_type {
    provenance_type_directory,
    provenance_type_tool_invocation,
};


struct provenance {
    enum provenance_type type;
    union {
        struct {
            struct directory *directory;
        } directory;
        struct {
            struct tool_invocation *tool_invocation;
        } tool_invocation;
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
void executable_scan(struct executable *executable);
void executable_binary(struct executable *executable);
void executable_tool_add
    (struct executable *executable, struct executable *tool);
void executable_tool_invocation_add
    (struct executable *executable, struct tool_invocation *invocation);
void executable_library_add
    (struct executable *executable, struct library *library);
void executable_object_add
    (struct executable *executable, struct object *object);
char *executable_path_get(struct executable *executable);

struct library *library_initialize(char *base_name, char *path);
void library_print(struct library *library);
void library_scan(struct library *library);
void library_tool_add(struct library *library, struct executable *tool);
void library_tool_invocation_add
    (struct library *library, struct tool_invocation *invocation);
void library_library_add(struct library *library, struct library *sub_library);
void library_object_add(struct library *library, struct object *object);
void library_header_add(struct library *library, struct header *header);
void library_binary(struct library *library);
char *library_path_get(struct library *library);

struct directory *directory_initialize(char *path);
void directory_print(struct directory *directory);
void directory_scan(struct directory *directory);
void directory_header_add(struct directory *directory, struct header *header);
void directory_source_add(struct directory *directory, struct source *source);
char *directory_path_get(struct directory *directory);

struct object *object_initialize(struct source *source);
void object_print(struct object *object);
void object_scan(struct object *object);
void object_binary(struct object *object, char *buildable_name);
char *object_path_get(struct object *object, char *buildable_name);
void object_header_add(struct object *object, struct header *header);
void object_source_add(struct object *object, struct source *source);

void header_print(struct header *header);
void header_binary(struct header *header, char *buildable_name);
char *header_source_path_get(struct header *header);
char *header_product_path_get(struct header *header, char *buildable_name);

void source_print(struct source *source);
char *source_path_get(struct source *source);

struct provenance *provenance_directory_initialize
    (struct directory *directory);
void provenance_print(struct provenance *provenance);
char *provenance_path_get(struct provenance *provenance);

void build_step_print(struct build_step *build_step);

void compiler_invoke(char **parameters);
void libtool_invoke(char **parameters);
void copy_file(char *destination, char *source);


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
    // if(project) print((void (*)(void *)) project_print, project);
    
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


void project_amalgamation(struct project *project) {
}


void project_binary(struct project *project) {
    for(struct executable **executable = project->executables;
        *executable;
        executable++)
    {
        executable_binary(*executable);
    }
    
    for(struct library **library = project->libraries;
        *library;
        library++)
    {
        library_binary(*library);
    }
}


void project_test(struct project *project) {
}


void project_debug(struct project *project) {
}


void project_clean(struct project *project) {
}


struct executable *executable_initialize(char *name, char *path) {
    struct executable *executable = malloc(sizeof(struct executable));
    executable->name = strdup(name);
    executable->directory = directory_initialize(path);
    executable->tools = malloc(sizeof(struct executable *) * 1);
    executable->tools[0] = NULL;
    executable->tool_invocations =
        malloc(sizeof(struct tool_invocation *) * 1);
    executable->tool_invocations[0] = NULL;
    executable->libraries = malloc(sizeof(struct library *) * 1);
    executable->libraries[0] = NULL;
    executable->objects = malloc(sizeof(struct object *) * 1);
    executable->objects[0] = NULL;
    
    executable_scan(executable);
     
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

    for(struct tool_invocation **invocation = executable->tool_invocations;
        *invocation;
        invocation++)
    {
        print((void (*)(void *)) tool_invocation_print, *invocation);
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


void executable_scan(struct executable *executable) {
    for(struct source **source = executable->directory->sources;
        *source;
        source++)
    {
        struct object *object = object_initialize(*source);
        object_source_add(object, *source);
        object_scan(object);
        executable_object_add(executable, object);
    }
}


void executable_binary(struct executable *executable) {
    for(struct executable **tool = executable->tools; *tool; tool++) {
        executable_binary(*tool);
    }

    size_t library_count = 0;
    for(struct library **library = executable->libraries; *library; library++)
    {
        library_count++;
        library_binary(*library);
    }

    size_t object_count = 0;
    for(struct object **object = executable->objects; *object; object++) {
        object_count++;
        object_binary(*object, executable->name);
    }

    char **parameters = malloc
        (sizeof(char *) * (4 + object_count + library_count));
    size_t i = 0;
    parameters[i++] = strdup("-O3");
    parameters[i++] = strdup("-o");
    parameters[i++] = executable_path_get(executable);

    for(struct object **object = executable->objects; *object; object++) {
        parameters[i++] = object_path_get(*object, executable->name);
    }

    for(struct library **library = executable->libraries; *library; library++)
    {
        parameters[i++] = library_path_get(*library);
    }

    parameters[i] = NULL;

    compiler_invoke(parameters);

    for(size_t i = 0; parameters[i]; i++) {
        free(parameters[i]);
    }
    free(parameters);
}


void executable_tool_add
    (struct executable *executable, struct executable *tool)
{
    size_t count = 0;
    for(struct executable **i = executable->tools; *i; i++, count++);
    executable->tools = realloc
        (executable->tools, sizeof(struct executable *) * (count + 2));
    executable->tools[count] = tool;
    executable->tools[count + 1] = NULL;
}


void executable_tool_invocation_add
    (struct executable *executable, struct tool_invocation *invocation)
{
    size_t count = 0;
    for(struct tool_invocation **i = executable->tool_invocations;
        *i;
        i++, count++);
    executable->tool_invocations = realloc
        (executable->tool_invocations,
         sizeof(struct tool_invocation *) * (count + 2));
    executable->tool_invocations[count] = invocation;
    executable->tool_invocations[count + 1] = NULL;
}


void executable_library_add
    (struct executable *executable, struct library *library)
{
    size_t count = 0;
    for(struct library **i = executable->libraries; *i; i++, count++);
    executable->libraries = realloc
        (executable->libraries, sizeof(struct library *) * (count + 2));
    executable->libraries[count] = library;
    executable->libraries[count + 1] = NULL;
}


void executable_object_add
    (struct executable *executable, struct object *object)
{
    size_t count = 0;
    for(struct object **i = executable->objects; *i; i++, count++);
    executable->objects = realloc
        (executable->objects, sizeof(struct object *) * (count + 2));
    executable->objects[count] = object;
    executable->objects[count + 1] = NULL;
}


char *executable_path_get(struct executable *executable) {
    char **path_parts = malloc(sizeof(char *) * 5);
    path_parts[0] = strdup("dist/");
    path_parts[1] = strdup(executable->name);
    path_parts[2] = strdup("/binary/products/");
    path_parts[3] = strdup(executable->name);
    path_parts[4] = NULL;
    size_t path_length;
    for(size_t j = 0; path_parts[j]; j++) {
        path_length += strlen(path_parts[j]);
    }
    char *path = malloc(path_length + 1);
    for(size_t j = 0; path_parts[j]; j++) {
        if(!j) strcpy(path, path_parts[j]);
        else strcat(path, path_parts[j]);
        free(path_parts[j]);
    }
    free(path_parts);
    return path;
}


struct library *library_initialize(char *base_name, char *path) {
    struct library *library = malloc(sizeof(struct library));
    library->base_name = strdup(base_name);
    library->directory = directory_initialize(path);
    library->tools = malloc(sizeof(struct executable *) * 1);
    library->tools[0] = NULL;
        print((void (*)(void *)) tool_invocation_print, *invocation);
    library->tool_invocations =
        malloc(sizeof(struct tool_invocation *) * 1);
    library->tool_invocations[0] = NULL;
    library->libraries = malloc(sizeof(struct library *) * 1);
    library->libraries[0] = NULL;
    library->objects = malloc(sizeof(struct object *) * 1);
    library->objects[0] = NULL;
    library->headers = malloc(sizeof(struct headers *) * 1);
    library->headers[0] = NULL;
    
    library_scan(library);
    
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

    for(struct tool_invocation **invocation = library->tool_invocations;
        *invocation;
        invocation++)
    {
        print((void (*)(void *)) tool_invocation_print, *invocation);
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


void library_scan(struct library *library) {
    for(struct source **source = library->directory->sources;
        *source;
        source++)
    {
        struct object *object = object_initialize(*source);
        object_source_add(object, *source);
        object_scan(object);
        library_object_add(library, object);
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


void library_tool_invocation_add
    (struct library *library, struct tool_invocation *invocation)
{
    size_t count = 0;
    for(struct tool_invocation **i = library->tool_invocations;
        *i;
        i++, count++);
    library->tool_invocations = realloc
        (library->tool_invocations,
         sizeof(struct tool_invocation *) * (count + 2));
    library->tool_invocations[count] = invocation;
    library->tool_invocations[count + 1] = NULL;
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


void library_binary(struct library *library) {
    for(struct executable **tool = library->tools; *tool; tool++) {
        executable_binary(*tool);
    }

    size_t library_count = 0;
    for(struct library **sub_library = library->libraries;
        *sub_library;
        sub_library++)
    {
        library_count++;
        library_binary(*sub_library);
    }

    size_t object_count = 0;
    for(struct object **object = library->objects; *object; object++) {
        object_count++;
        object_binary(*object, library->base_name);
    }

    char **parameters = malloc
        (sizeof(char *) * (4 + object_count + library_count));
    size_t i = 0;
    parameters[i++] = strdup("-o");
    parameters[i++] = library_path_get(library);

    for(struct object **object = library->objects; *object; object++) {
        parameters[i++] = object_path_get(*object, library->base_name);
    }

    for(struct library **sub_library = library->libraries;
        *sub_library;
        sub_library++)
    {
        parameters[i++] = library_path_get(*sub_library);
    }

    parameters[i] = NULL;

    libtool_invoke(parameters);

    for(size_t i = 0; parameters[i]; i++) {
        free(parameters[i]);
    }
    free(parameters);

    for(struct header **header = library->headers; *header; header++) {
        header_binary(*header, library->base_name);
    }
}


char *library_path_get(struct library *library) {
    char **path_parts = malloc(sizeof(char *) * 6);
    path_parts[0] = strdup("dist/");
    path_parts[1] = strdup(library->base_name);
    path_parts[2] = strdup("/binary/products/lib");
    path_parts[3] = strdup(library->base_name);
    path_parts[4] = strdup(".a");
    path_parts[5] = NULL;
    size_t path_length;
    for(size_t j = 0; path_parts[j]; j++) {
        path_length += strlen(path_parts[j]);
    }
    char *path = malloc(path_length + 1);
    for(size_t j = 0; path_parts[j]; j++) {
        if(!j) strcpy(path, path_parts[j]);
        else strcat(path, path_parts[j]);
        free(path_parts[j]);
    }
    free(path_parts);
    return path;
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
            filename = strdup(entry->d_name);
            
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


char *directory_path_get(struct directory *directory) {
    return strdup(directory->path);
}


struct object *object_initialize(struct source *source) {
    struct object *object = malloc(sizeof(struct object));
    size_t base_name_length = strlen(source->filename) - 2;
    object->base_name = malloc(base_name_length + 1);
    strncpy(object->base_name, source->filename, base_name_length);
    object->base_name[base_name_length] = '\0';
    object->headers = malloc(sizeof(struct header *) * 1);
    object->headers[0] = NULL;
    object->sources = malloc(sizeof(struct source *) * 2);
    object->sources[0] = source;
    object->sources[1] = NULL;
    return object;
}


void object_print(struct object *object) {
    print_line("Object: %s", object->base_name);
    
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


void object_scan(struct object *object) {
    // TODO
/*
clang -MM -MG -MF basename.d basename.c

processor-explicatory.o: library/processor-explicatory.c library/modern.h \
      library/internal.h keywords.h

or, when run on a header,
internal.o: library/internal.h
*/
}


void object_binary(struct object *object, char *buildable_name) {
    size_t source_count = 0;
    for(struct source **source = object->sources; *source; source++) {
        source_count++;
    }

    char **parameters = malloc(sizeof(char *) * (4 + source_count));
    size_t i = 0;
    parameters[i++] = strdup("-O3");
    parameters[i++] = strdup("-o");
    parameters[i++] = object_path_get(object, buildable_name);

    for(struct source **source = object->sources; *source; source++) {
        parameters[i++] = source_path_get(*source);
    }

    parameters[i] = NULL;

    compiler_invoke(parameters);

    for(size_t i = 0; parameters[i]; i++) {
        free(parameters[i]);
    }
    free(parameters);
}


char *object_path_get(struct object *object, char *buildable_name) {
    char **path_parts = malloc(sizeof(char *) * 6);
    path_parts[0] = strdup("dist/");
    path_parts[1] = strdup(buildable_name);
    path_parts[2] = strdup("/binary/objects/");
    path_parts[3] = strdup(object->base_name);
    path_parts[4] = strdup(".o");
    path_parts[5] = NULL;
    size_t path_length;
    for(size_t j = 0; path_parts[j]; j++) {
        path_length += strlen(path_parts[j]);
    }
    char *path = malloc(path_length + 1);
    for(size_t j = 0; path_parts[j]; j++) {
        if(!j) strcpy(path, path_parts[j]);
        else strcat(path, path_parts[j]);
        free(path_parts[j]);
    }
    free(path_parts);
    return path;
}


void object_header_add(struct object *object, struct header *header) {
    size_t count = 0;
    for(struct header **i = object->headers; *i; i++, count++);
    object->headers = realloc
        (object->headers, sizeof(struct object *) * (count + 2));
    object->headers[count] = header;
    object->headers[count + 1] = NULL;
}


void object_source_add(struct object *object, struct source *source) {
    size_t count = 0;
    for(struct source **i = object->sources; *i; i++, count++);
    object->sources = realloc
        (object->sources, sizeof(struct source *) * (count + 2));
    object->sources[count] = source;
    object->sources[count + 1] = NULL;
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


void header_binary(struct header *header, char *buildable_name) {
    char *source_path = header_source_path_get(header);
    char *product_path = header_product_path_get(header, buildable_name);
    copy_file(product_path, source_path);
    free(source_path);
    free(product_path);
}


char *header_source_path_get(struct header *header) {
    char **path_parts = malloc(sizeof(char *) * 3);
    path_parts[0] = provenance_path_get(header->provenance);
    path_parts[1] = strdup(header->filename);
    path_parts[2] = NULL;
    size_t path_length;
    for(size_t j = 0; path_parts[j]; j++) {
        path_length += strlen(path_parts[j]);
    }
    char *path = malloc(path_length + 1);
    for(size_t j = 0; path_parts[j]; j++) {
        if(!j) strcpy(path, path_parts[j]);
        else strcat(path, path_parts[j]);
        free(path_parts[j]);
    }
    free(path_parts);
    return path;
}


char *header_product_path_get(struct header *header, char *buildable_name) {
    char **path_parts = malloc(sizeof(char *) * 5);
    path_parts[0] = strdup("dist/");
    path_parts[1] = strdup(buildable_name);
    path_parts[2] = strdup("/binary/products/");
    path_parts[3] = strdup(header->filename);
    path_parts[4] = NULL;
    size_t path_length;
    for(size_t j = 0; path_parts[j]; j++) {
        path_length += strlen(path_parts[j]);
    }
    char *path = malloc(path_length + 1);
    for(size_t j = 0; path_parts[j]; j++) {
        if(!j) strcpy(path, path_parts[j]);
        else strcat(path, path_parts[j]);
        free(path_parts[j]);
    }
    free(path_parts);
    return path;
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


char *source_path_get(struct source *source) {
    char **path_parts = malloc(sizeof(char *) * 3);
    path_parts[0] = provenance_path_get(source->provenance);
    path_parts[1] = strdup(source->filename);
    path_parts[2] = NULL;
    size_t path_length;
    for(size_t j = 0; path_parts[j]; j++) {
        path_length += strlen(path_parts[j]);
    }
    char *path = malloc(path_length + 1);
    for(size_t j = 0; path_parts[j]; j++) {
        if(!j) strcpy(path, path_parts[j]);
        else strcat(path, path_parts[j]);
        free(path_parts[j]);
    }
    free(path_parts);
    return path;
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


char *provenance_path_get(struct provenance *provenance) {
    switch(provenance->type) {
    case provenance_type_directory:
        return directory_path_get(provenance->specifics.directory.directory);
    case provenance_type_step:
        return NULL;
    }
}


void build_step_print(struct build_step *build_step) {
    print_line("Build Step:");
}


void compiler_invoke(char **parameters) {
    printf("clang");
    for(char **parameter = parameters; *parameter; parameter++) {
        printf(" %s", *parameter);
    }
    printf("\n");
}


void libtool_invoke(char **parameters) {
    printf("libtool");
    for(char **parameter = parameters; *parameter; parameter++) {
        printf(" %s", *parameter);
    }
    printf("\n");
}


void copy_file(char *destination, char *source) {
    printf("cp %s %s\n", source, destination);
}

