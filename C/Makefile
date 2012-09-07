CC=clang
CFLAGS=-g -O0
OBJECTS=context.o \
        evaluation.o \
		hash.o \
		library.o \
		memory.o \
		node-canonical.o \
		node-get.o \
		node-make.o
INCLUDES=modern.h internal.h

default: test-node
all: test-node test-canonical test-out

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

test-canonical.o: test-canonical.c ${INCLUDES}
	${CC} ${CFLAGS} -c test-canonical.c -o test-canonical.o

test-main.o: test-main.c ${INCLUDES}
	${CC} ${CFLAGS} -c test-main.c -o test-main.o

test-node.o: test-node.c ${INCLUDES}
	${CC} ${CFLAGS} -c test-node.c -o test-node.o

test-out.o: test-out.c ${INCLUDES}
	${CC} ${CFLAGS} -c test-out.c -o test-out.o

test-canonical: ${OBJECTS} test-main.o test-canonical.o
	${CC} ${CFLAGS} ${OBJECTS} test-main.o test-canonical.o -o test-canonical

test-node: ${OBJECTS} test-main.o test-node.o
	${CC} ${CFLAGS} ${OBJECTS} test-main.o test-node.o -o test-node

test-out: ${OBJECTS} test-main.o test-out.o
	${CC} ${CFLAGS} ${OBJECTS} test-main.o test-out.o -o test-out

.dummy: default all
