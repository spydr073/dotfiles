#!/usr/bin/env bash

cat <<EOT >> src/Makefile

all: $1.so

block_file.so : cbits/$1.o
	gcc -shared cbits/$1.o -o $1.so

cbits/block_file.o : cbits/$1.h cbits/$1.c
	gcc -fPIC -c -pedantic -Wall -Wextra -ggdb3 cbits/$1.c -o cbits/$1.o

clean:
	find . -name *.o   -delete
	find . -name *.ibc -delete

EOT

