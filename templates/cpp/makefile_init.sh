#!/usr/bin/env bash

cat <<EOT >> Makefile
#-------------------------------------------------------------------------------------------------
#--
#-- Gerneral Purpose C/C++ Makefile for $1
#--
#-------------------------------------------------------------------------------------------------


PROJECT   = $1
EXTRALIBS = -pthread

CC      = g++
FLAGS   = -O3 #-Wall
SRCEXT  = cc

SRC     = src
BUILD   = build
TARGET  = bin/\$(PROJECT)
INCLUDE = -I include
LIB     = -L lib \$(EXTRALIBS)

SOURCES    = \$(shell find \$(SRC) -type f -name *.\$(SRCEXT))
OBJECTS    = \$(patsubst \$(SRC)/%,\$(BUILD)/%,\$(SOURCES:.\$(SRCEXT)=.o))
BUILDPATHS = \$(dir \$(OBJECTS))

all: \$(TARGET)

\$(TARGET): \$(OBJECTS)
	\$(CC) $^ -o \$(TARGET) \$(LIB)

\$(BUILD)/%.o: \$(SRC)/%.\$(SRCEXT)
	mkdir -p \$(BUILDPATHS)
	\$(CC) \$(FLAGS) \$(INCLUDE) -c -o \$@ $<

clean:
	rm -r \$(BUILD) \$(TARGET)


.PHONY: all clean


EOT

