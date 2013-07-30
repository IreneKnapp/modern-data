#include <dirent.h>
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


struct project {
    char *name;
    struct executable **executables;
    struct library **libraries;
};


struct executable {
    char *name;
    struct directory *directory;
    struct library **libraries;
    struct object **objects;
};


struct library {
    char *base_name;
    struct directory *directory;
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
struct project *project_initialize(char *name);
void **project_print
    (int indent, struct project *project, void **backreferences);
struct project *project_prepare();
void project_executable_add
    (struct project *project, struct executable *executable);
void project_library_add(struct project *project, struct library *library);
void project_amalgamation(struct project *project);
void project_binary(struct project *project);
void project_test(struct project *project);
void project_debug(struct project *project);
void project_clean(struct project *project);

struct library *library_initialize(char *base_name, char *path);
void **library_print
    (int indent, struct library *library, void **backreferences);

struct directory *directory_initialize(char *path);
void **directory_print
    (int indent, struct directory *directory, void **backreferences);
void directory_scan(struct directory *directory);
void directory_header_add(struct directory *directory, struct header *header);
void directory_source_add(struct directory *directory, struct source *source);

void **executable_print
    (int indent, struct executable *executable, void **backreferences);

void **object_print(int indent, struct object *object, void **backreferences);

void **header_print(int indent, struct header *header, void **backreferences);

void **source_print(int indent, struct source *source, void **backreferences);

struct provenance *provenance_directory_initialize
    (struct directory *directory);
void **provenance_print
    (int indent, struct provenance *provenance, void **backreferences);

void **build_step_print
    (int indent, struct build_step *build_step, void **backreferences);


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
    if(project) (void) project_print(0, project, NULL);
    
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


struct project *project_initialize(char *name) {
    struct project *project = malloc(sizeof(struct project));
    project->name = strdup(name);
    project->executables = malloc(sizeof(struct executable *) * 1);
    project->executables[0] = NULL;
    project->libraries = malloc(sizeof(struct library *) * 1);
    project->libraries[0] = NULL;
    return project;
}


void **project_print
    (int indent, struct project *project, void **backreferences)
{
    if(!backreferences) {
        backreferences = malloc(sizeof(void *) * 1);
        backreferences[0] = NULL;
    }
    for(size_t i = 0; backreferences[i]; i++) {
        if(backreferences[i] == project) {
            for(int i = 0; i < indent; i++) printf("    ");
            printf("Project: #%llu\n", (unsigned long long) i);
            return backreferences;
        }
    }
    {
        size_t count;
        for(count = 0; backreferences[count]; count++);
        backreferences = realloc(backreferences, sizeof(void *) * (count + 2));
        backreferences[count] = project;
        backreferences[count + 1] = NULL;
    }
    
    for(int i = 0; i < indent; i++) printf("    ");
    printf("Project: %s\n", project->name);
    
    for(struct executable **executable = project->executables;
        *executable;
        executable++)
    {
        executable_print(indent + 1, *executable, backreferences);
    }
    
    for(struct library **library = project->libraries;
        *library;
        library++)
    {
        library_print(indent + 1, *library, backreferences);
    }

    return backreferences;
}


struct project *project_prepare() {
    struct project *project = project_initialize("Modern Data");
    struct library *library = library_initialize("modern", "library/");
    project_library_add(project, library);
    
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


struct library *library_initialize(char *base_name, char *path) {
    struct library *library = malloc(sizeof(struct library));
    library->base_name = strdup(base_name);
    library->directory = directory_initialize(path);
    library->libraries = malloc(sizeof(struct library *) * 1);
    library->libraries[0] = NULL;
    library->objects = malloc(sizeof(struct object *) * 1);
    library->objects[0] = NULL;
    library->headers = malloc(sizeof(struct headers *) * 1);
    library->headers[0] = NULL;
    return library;
}


void **library_print
    (int indent, struct library *library, void **backreferences)
{
    if(!backreferences) {
        backreferences = malloc(sizeof(void *) * 1);
        backreferences[0] = NULL;
    }
    for(size_t i = 0; backreferences[i]; i++) {
        if(backreferences[i] == library) {
            for(int i = 0; i < indent; i++) printf("    ");
            printf("Library: #%llu\n", (unsigned long long) i);
            return backreferences;
        }
    }
    {
        size_t count;
        for(count = 0; backreferences[count]; count++);
        backreferences = realloc(backreferences, sizeof(void *) * (count + 2));
        backreferences[count] = library;
        backreferences[count + 1] = NULL;
    }

    for(int i = 0; i < indent; i++) printf("    ");
    printf("Library: %s\n", library->base_name);
    
    backreferences =
        directory_print(indent + 1, library->directory, backreferences);
    
    for(struct library **sub_library = library->libraries;
        *sub_library;
        sub_library++)
    {
        backreferences =
            library_print(indent + 1, *sub_library, backreferences);
    }
    
    for(struct object **object = library->objects;
        *object;
        object++)
    {
        backreferences = object_print(indent + 1, *object, backreferences);
    }
    
    for(struct header **header = library->headers;
        *header;
        header++)
    {
        backreferences = header_print(indent + 1, *header, backreferences);
    }

    return backreferences;
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


void **directory_print
    (int indent, struct directory *directory, void **backreferences)
{
    if(!backreferences) {
        backreferences = malloc(sizeof(void *) * 1);
        backreferences[0] = NULL;
    }
    for(size_t i = 0; backreferences[i]; i++) {
        if(backreferences[i] == directory) {
            for(int i = 0; i < indent; i++) printf("    ");
            printf("Directory: #%llu\n", (unsigned long long) i);
            return backreferences;
        }
    }
    {
        size_t count;
        for(count = 0; backreferences[count]; count++);
        backreferences = realloc(backreferences, sizeof(void *) * (count + 2));
        backreferences[count] = directory;
        backreferences[count + 1] = NULL;
    }

    for(int i = 0; i < indent; i++) printf("    ");
    printf("Directory: %s\n", directory->path);
    
    for(struct header **header = directory->headers;
        *header;
        header++)
    {
        backreferences = header_print(indent + 1, *header, backreferences);
    }
    
    for(struct source **source = directory->sources;
        *source;
        source++)
    {
        backreferences = source_print(indent + 1, *source, backreferences);
    }

    return backreferences;
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
            //directory_source_add(directory, source);
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


void **executable_print
    (int indent, struct executable *executable, void **backreferences)
{
    if(!backreferences) {
        backreferences = malloc(sizeof(void *) * 1);
        backreferences[0] = NULL;
    }
    for(size_t i = 0; backreferences[i]; i++) {
        if(backreferences[i] == executable) {
            for(int i = 0; i < indent; i++) printf("    ");
            printf("Executable: #%llu\n", (unsigned long long) i);
            return backreferences;
        }
    }
    {
        size_t count;
        for(count = 0; backreferences[count]; count++);
        backreferences = realloc(backreferences, sizeof(void *) * (count + 2));
        backreferences[count] = executable;
        backreferences[count + 1] = NULL;
    }
    
    for(int i = 0; i < indent; i++) printf("    ");
    printf("Executable: %s\n", executable->name);

    backreferences =
        directory_print(indent + 1, executable->directory, backreferences);
    
    for(struct library **library = executable->libraries;
        *library;
        library++)
    {
        backreferences = library_print(indent + 1, *library, backreferences);
    }
    
    for(struct object **object = executable->objects;
        *object;
        object++)
    {
        backreferences = object_print(indent + 1, *object, backreferences);
    }

    return backreferences;
}


void **object_print
    (int indent, struct object *object, void **backreferences)
{
    if(!backreferences) {
        backreferences = malloc(sizeof(void *) * 1);
        backreferences[0] = NULL;
    }
    for(size_t i = 0; backreferences[i]; i++) {
        if(backreferences[i] == object) {
            for(int i = 0; i < indent; i++) printf("    ");
            printf("Object: #%llu\n", (unsigned long long) i);
            return backreferences;
        }
    }
    {
        size_t count;
        for(count = 0; backreferences[count]; count++);
        backreferences = realloc(backreferences, sizeof(void *) * (count + 2));
        backreferences[count] = object;
        backreferences[count + 1] = NULL;
    }

    for(int i = 0; i < indent; i++) printf("    ");
    printf("Object: %s\n", object->base_name);
    
    backreferences =
        provenance_print(indent + 1, object->provenance, backreferences);
    
    for(struct header **header = object->headers;
        *header;
        header++)
    {
        backreferences = header_print(indent + 1, *header, backreferences);
    }
    
    for(struct source **source = object->sources;
        *source;
        source++)
    {
        backreferences = source_print(indent + 1, *source, backreferences);
    }

    return backreferences;
}


void **header_print
    (int indent, struct header *header, void **backreferences)
{
    if(!backreferences) {
        backreferences = malloc(sizeof(void *) * 1);
        backreferences[0] = NULL;
    }
    for(size_t i = 0; backreferences[i]; i++) {
        if(backreferences[i] == header) {
            for(int i = 0; i < indent; i++) printf("    ");
            printf("Header: #%llu\n", (unsigned long long) i);
            return backreferences;
        }
    }
    {
        size_t count;
        for(count = 0; backreferences[count]; count++);
        backreferences = realloc(backreferences, sizeof(void *) * (count + 2));
        backreferences[count] = header;
        backreferences[count + 1] = NULL;
    }

    for(int i = 0; i < indent; i++) printf("    ");
    printf("Header: %s\n", header->filename);

    provenance_print(indent + 1, header->provenance, backreferences);

    for(struct header **sub_header = header->headers;
        *sub_header;
        sub_header++)
    {
        backreferences = header_print(indent + 1, *sub_header, backreferences);
    }

    return backreferences;
}


void **source_print
    (int indent, struct source *source, void **backreferences)
{
    if(!backreferences) {
        backreferences = malloc(sizeof(void *) * 1);
        backreferences[0] = NULL;
    }
    for(size_t i = 0; backreferences[i]; i++) {
        if(backreferences[i] == source) {
            for(int i = 0; i < indent; i++) printf("    ");
            printf("Source: #%llu\n", (unsigned long long) i);
            return backreferences;
        }
    }
    {
        size_t count;
        for(count = 0; backreferences[count]; count++);
        backreferences = realloc(backreferences, sizeof(void *) * (count + 2));
        backreferences[count] = source;
        backreferences[count + 1] = NULL;
    }

    for(int i = 0; i < indent; i++) printf("    ");
    printf("Source: %s\n", source->filename);

    backreferences = provenance_print
        (indent + 1, source->provenance, backreferences);

    for(struct header **header = source->headers;
        *header;
        header++)
    {
        backreferences = header_print(indent + 1, *header, backreferences);
    }

    return backreferences;
}


struct provenance *provenance_directory_initialize
    (struct directory *directory)
{
    struct provenance *provenance = malloc(sizeof(struct provenance));
    provenance->type = provenance_type_directory;
    provenance->specifics.directory.directory = directory;
    return provenance;
}


void **provenance_print
    (int indent, struct provenance *provenance, void **backreferences)
{
    if(!backreferences) {
        backreferences = malloc(sizeof(void *) * 1);
        backreferences[0] = NULL;
    }
    for(size_t i = 0; backreferences[i]; i++) {
        if(backreferences[i] == provenance) {
            for(int i = 0; i < indent; i++) printf("    ");
            printf("Provenance: #%llu\n", (unsigned long long) i);
            return backreferences;
        }
    }
    {
        size_t count;
        for(count = 0; backreferences[count]; count++);
        backreferences = realloc(backreferences, sizeof(void *) * (count + 2));
        backreferences[count] = provenance;
        backreferences[count + 1] = NULL;
    }

    for(int i = 0; i < indent; i++) printf("    ");
    printf("Provenance:\n");

    switch(provenance->type) {
    case provenance_type_directory:
        directory_print
            (indent + 1, provenance->specifics.directory.directory,
             backreferences);
        break;
    case provenance_type_step:
        build_step_print
            (indent + 1, provenance->specifics.build_step.build_step,
             backreferences);
        break;
    }

    return backreferences;
}


void **build_step_print
    (int indent, struct build_step *build_step, void **backreferences)
{
    if(!backreferences) {
        backreferences = malloc(sizeof(void *) * 1);
        backreferences[0] = NULL;
    }
    for(size_t i = 0; backreferences[i]; i++) {
        if(backreferences[i] == build_step) {
            for(int i = 0; i < indent; i++) printf("    ");
            printf("Build Step: #%llu\n", (unsigned long long) i);
            return backreferences;
        }
    }
    {
        size_t count;
        for(count = 0; backreferences[count]; count++);
        backreferences = realloc(backreferences, sizeof(void *) * (count + 2));
        backreferences[count] = build_step;
        backreferences[count + 1] = NULL;
    }

    for(int i = 0; i < indent; i++) printf("    ");
    printf("Build Step:\n");

    return backreferences;
}

