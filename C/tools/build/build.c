#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


enum mode {
    mode_help,
    mode_amalgamation,
    mode_binary,
    mode_test,
    mode_clean,
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
};


enum provenance_type {
    provenance_type_directory,
    provenance_type_build_step,
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
struct project *prepare();
void build_amalgamation(struct project *project);
void build_binary(struct project *project);
void build_test(struct project *project);
void clean(struct project *project);
void print_project(int indent, struct project *project);
void print_executable(int indent, struct executable *executable);
void print_library(int indent, struct library *library);
void print_object(int indent, struct object *object);
void print_header(int indent, struct header *header);
void print_source(int indent, struct source *source);
void print_directory(int indent, struct directory *directory);
void print_provenance(int indent, struct provenance *provenance);
void print_build_step(int indent, struct build_step *build_step);


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
        } else if(!strcmp(argv[i], "clean")) {
            mode = mode_clean;
        } else {
            error = 1;
        }
    }
    if(error) mode = mode_help;
    
    struct project *project = NULL;
    if(mode != mode_help) project = prepare();
    if(project) print_project(0, project);
    
    switch(mode) {
    case mode_help: help(); break;
    case mode_amalgamation: build_amalgamation(project); break;
    case mode_binary: build_binary(project); break;
    case mode_test: build_test(project); break;
    case mode_clean: clean(project); break;
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
    printf("    clean              Remove all build products.\n");
    printf("\n");
}


struct project *prepare() {
    struct project *project = malloc(sizeof(struct project));
    project->name = strdup("Modern Data");
    project->executables = malloc(sizeof(struct executable *) * 1);
    project->executables[0] = NULL;
    project->libraries = malloc(sizeof(struct library *) * 2);
    project->libraries[0] = malloc(sizeof(struct library));
    project->libraries[0]->base_name = strdup("modern");
    project->libraries[0]->directory = malloc(sizeof(struct directory));
    project->libraries[0]->directory->path = strdup("library/");
    project->libraries[0]->libraries = malloc(sizeof(struct library *) * 1);
    project->libraries[0]->libraries[0] = NULL;
    project->libraries[0]->objects = malloc(sizeof(struct object *) * 1);
    project->libraries[0]->objects[0] = NULL;
    project->libraries[0]->headers = malloc(sizeof(struct headers *) * 1);
    project->libraries[0]->headers[0] = NULL;
    project->libraries[1] = NULL;
    
    return project;
}


void build_amalgamation(struct project *project) {
}


void build_binary(struct project *project) {
}


void build_test(struct project *project) {
}


void clean(struct project *project) {
}


void print_project(int indent, struct project *project) {
    for(int i = 0; i < indent; i++) printf("    ");
    printf("Project: %s\n", project->name);
    
    for(struct executable **executable = project->executables;
        *executable;
        executable++)
    {
        print_executable(indent + 1, *executable);
    }
    
    for(struct library **library = project->libraries;
        *library;
        library++)
    {
        print_library(indent + 1, *library);
    }
}


void print_executable(int indent, struct executable *executable) {
    for(int i = 0; i < indent; i++) printf("    ");
    printf("Executable: %s\n", executable->name);

    print_directory(indent + 1, executable->directory);
    
    for(struct library **library = executable->libraries;
        *library;
        library++)
    {
        print_library(indent + 1, *library);
    }
    
    for(struct object **object = executable->objects;
        *object;
        object++)
    {
        print_object(indent + 1, *object);
    }
}


void print_library(int indent, struct library *library) {
    for(int i = 0; i < indent; i++) printf("    ");
    printf("Library: %s\n", library->base_name);
    
    print_directory(indent + 1, library->directory);
    
    for(struct library **sub_library = library->libraries;
        *sub_library;
        sub_library++)
    {
        print_library(indent + 1, *sub_library);
    }
    
    for(struct object **object = library->objects;
        *object;
        object++)
    {
        print_object(indent + 1, *object);
    }
    
    for(struct header **header = library->headers;
        *header;
        header++)
    {
        print_header(indent + 1, *header);
    }
}


void print_object(int indent, struct object *object) {
    for(int i = 0; i < indent; i++) printf("    ");
    printf("Object: %s\n", object->base_name);
    
    print_provenance(indent + 1, object->provenance);
    
    for(struct header **header = object->headers;
        *header;
        header++)
    {
        print_header(indent + 1, *header);
    }
    
    for(struct source **source = object->sources;
        *source;
        source++)
    {
        print_source(indent + 1, *source);
    }
}


void print_header(int indent, struct header *header) {
    for(int i = 0; i < indent; i++) printf("    ");
    printf("Header: %s\n", header->filename);

    print_provenance(indent + 1, header->provenance);

    for(struct header **sub_header = header->headers;
        *sub_header;
        sub_header++)
    {
        print_header(indent + 1, *sub_header);
    }
}


void print_source(int indent, struct source *source) {
    for(int i = 0; i < indent; i++) printf("    ");
    printf("Source: %s\n", source->filename);

    print_provenance(indent + 1, source->provenance);

    for(struct header **header = source->headers;
        *header;
        header++)
    {
        print_header(indent + 1, *header);
    }
}


void print_directory(int indent, struct directory *directory) {
    for(int i = 0; i < indent; i++) printf("    ");
    printf("Directory: %s\n", directory->path);
}


void print_provenance(int indent, struct provenance *provenance) {
    for(int i = 0; i < indent; i++) printf("    ");
    printf("Provenance:\n");

    switch(provenance->type) {
    case provenance_type_directory:
        print_directory
            (indent + 1, provenance->specifics.directory.directory);
        break;
    case provenance_type_build_step:
        print_build_step
            (indent + 1, provenance->specifics.build_step.build_step);
        break;
    }
}


void print_build_step(int indent, struct build_step *build_step) {
    for(int i = 0; i < indent; i++) printf("    ");
    printf("Build Step:\n");
}

