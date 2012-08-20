CC=clang
OBJECTS=context.o \
		hash.o \
		library.o \
		memory.o \
		node-canonical.o \
		node-get.o \
		node-make.o \
		node-set.o
INCLUDES=modern.h internal.h

default: test-canonical
all: test-canonical test-out

context.o: context.c ${INCLUDES}
	${CC} -c context.c -o context.o

hash.o: hash.c ${INCLUDES}
	${CC} -c hash.c -o hash.o

library.o: library.c ${INCLUDES}
	${CC} -c library.c -o library.o

memory.o: memory.c ${INCLUDES}
	${CC} -c memory.c -o memory.o

node-canonical.o: node-canonical.c ${INCLUDES}
	${CC} -c node-canonical.c -o node-canonical.o

node-get.o: node-get.c ${INCLUDES}
	${CC} -c node-get.c -o node-get.o

node-make.o: node-make.c ${INCLUDES}
	${CC} -c node-make.c -o node-make.o

node-set.o: node-set.c ${INCLUDES}
	${CC} -c node-set.c -o node-set.o

test-canonical.o: test-canonical.c ${INCLUDES}
	${CC} -c test-canonical.c -o test-canonical.o

test-main.o: test-main.c ${INCLUDES}
	${CC} -c test-main.c -o test-main.o

test-out.o: test-out.c ${INCLUDES}
	${CC} -c test-out.c -o test-out.o

test-canonical: ${OBJECTS} test-main.o test-canonical.o
	${CC} ${OBJECTS} test-main.o test-canonical.o -o test-canonical

test-out: ${OBJECTS} test-main.o test-out.o
	${CC} ${OBJECTS} test-main.o test-out.o -o test-out

.dummy: default all
