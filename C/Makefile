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
	${CC} -c node-make.c -o mode-make.o

node-set.o: node-set.c ${INCLUDES}
	${CC} -c node-set.c -o node-set.o

test-canonical.o: test-canonical.c ${INCLUDES}
	${CC} -c test-canonical.c -o test-canonical.o

test-out.o: test-out.c ${INCLUDES}
	${CC} -c test-out.c -o test-out.o

test-canonical: ${OBJECTS} test-canonical.o
	${CC} ${OBJECTS} test-canonical.o -o test-canonical

test-out: ${OBJECTS} test-out.o
	${CC} ${OBJECTS} test-out.o -o test-out

.dummy: default
