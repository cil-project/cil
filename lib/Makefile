# Makefile for lib; this Makefile is just for building the test
# programs for experimenting with gc

CFLAGS = -g -Wall -Igc/include
CC = gcc $(CFLAGS)

splay: splay.c
	$(CC) -o splay -DTEST_SPLAY splay.c

# I compile with several levels of optimization because this
# greatly purturbs the results
mymark: mymark.c
	$(CC) -o mymark -DTEST_MYMARK mymark.c gc/gc.a
	$(CC) -o mymark-O2 -O2 -DTEST_MYMARK mymark.c gc/gc.a
	$(CC) -o mymark-O3 -O3 -DTEST_MYMARK mymark.c gc/gc.a

mymark.i: mymark.c
	cpp $(CFLAGS) mymark.c | grep -v '^#' > mymark.i

mymarki: mymark.i
	$(CC) -o mymarki mymark.i gc/gc.a
