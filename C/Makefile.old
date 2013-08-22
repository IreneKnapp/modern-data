CC=clang
CFLAGS=-g -O0 --coverage
LIBRARIES=-lm --coverage
OBJECTS=context.o \
        evaluation.o \
        hash.o \
        library.o \
        memory.o \
        node-canonical.o \
        node-get.o \
        node-make.o \
        node-set.o \
        predefined.o \
        processor-explicatory.o \
        vfile-stdio.o
INCLUDES=modern.h internal.h

COVERAGE_FILES=context.gcno context.gcda \
               evaluation.gcno evaluation.gcda \
               hash.gcno hash.gcda \
               library.gcno library.gcda \
               memory.gcno memory.gcda \
               node-canonical.gcno node-canonical.gcda \
               node-get.gcno node-get.gcda \
               node-make.gcno node-make.gcda \
               node-set.gcno node-set.gcda \
               predefined.gcno predefined.gcda \
               processor-explicatory.gcno processor-explicatory.gcda \
               vfile-stdio.gcno vfile-stdio.gcda

default: all
all: test-node test-canonical test-out test-in example-canonical

clean:
	rm -f ${OBJECTS} make-keywords.o test-in.o test-main.o
	rm -f ${COVERAGE_FILES}

context.o: context.c ${INCLUDES}
	${CC} ${CFLAGS} -c context.c -o context.o

evaluation.o: evaluation.c ${INCLUDES}
	${CC} ${CFLAGS} -c evaluation.c -o evaluation.o

hash.o: hash.c ${INCLUDES}
	${CC} ${CFLAGS} -c hash.c -o hash.o

library.o: library.c ${INCLUDES}
	${CC} ${CFLAGS} -c library.c -o library.o

memory.o: memory.c ${INCLUDES}
	${CC} ${CFLAGS} -c memory.c -o memory.o

node-canonical.o: node-canonical.c ${INCLUDES}
	${CC} ${CFLAGS} -c node-canonical.c -o node-canonical.o

node-get.o: node-get.c ${INCLUDES}
	${CC} ${CFLAGS} -c node-get.c -o node-get.o

node-make.o: node-make.c ${INCLUDES}
	${CC} ${CFLAGS} -c node-make.c -o node-make.o

node-set.o: node-set.c ${INCLUDES}
	${CC} ${CFLAGS} -c node-set.c -o node-set.o

predefined.o: predefined.c ${INCLUDES}
	${CC} ${CFLAGS} -c predefined.c -o predefined.o

processor-explicatory.o: processor-explicatory.c ${INCLUDES} keywords.h
	${CC} ${CFLAGS} -c processor-explicatory.c -o processor-explicatory.o

vfile-stdio.o: vfile-stdio.c ${INCLUDES}
	${CC} ${CFLAGS} -c vfile-stdio.c -o vfile-stdio.o

test-canonical.o: test-canonical.c ${INCLUDES}
	${CC} ${CFLAGS} -c test-canonical.c -o test-canonical.o

test-main.o: test-main.c ${INCLUDES}
	${CC} ${CFLAGS} -c test-main.c -o test-main.o

test-node.o: test-node.c ${INCLUDES}
	${CC} ${CFLAGS} -c test-node.c -o test-node.o

test-out.o: test-out.c ${INCLUDES}
	${CC} ${CFLAGS} -c test-out.c -o test-out.o

test-in.o: test-in.c ${INCLUDES}
	${CC} ${CFLAGS} -c test-in.c -o test-in.o

test-canonical: ${OBJECTS} test-main.o test-canonical.o
	${CC} ${CFLAGS} ${LIBRARIES} ${OBJECTS} test-main.o test-canonical.o -o test-canonical

test-node: ${OBJECTS} test-main.o test-node.o
	${CC} ${CFLAGS} ${LIBRARIES} ${OBJECTS} test-main.o test-node.o -o test-node

test-out: ${OBJECTS} test-main.o test-out.o
	${CC} ${CFLAGS} ${LIBRARIES} ${OBJECTS} test-main.o test-out.o -o test-out

test-in: ${OBJECTS} test-main.o test-in.o
	${CC} ${CFLAGS} ${LIBRARIES} ${OBJECTS} test-main.o test-in.o -o test-in

example-canonical.o: example-canonical.c ${INCLUDES}
	${CC} ${CFLAGS} -c example-canonical.c -o example-canonical.o

example-canonical: ${OBJECTS} example-canonical.o
	${CC} ${CFLAGS} ${LIBRARIES} ${OBJECTS} example-canonical.o -o example-canonical

make-keywords.o: make-keywords.c
	${CC} ${CFLAGS} -c make-keywords.c -o make-keywords.o

make-keywords: make-keywords.o hash.o modern.h
	${CC} ${CFLAGS} ${LIBRARIES} make-keywords.o hash.o -o make-keywords

keywords.h: make-keywords
	./make-keywords > keywords.h

.dummy: clean default all

