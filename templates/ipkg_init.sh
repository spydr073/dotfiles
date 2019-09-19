#!/usr/bin/env bash

cat <<EOT >> $1.ipkg
package $1

sourcedir = src
makefile  = Makefile
objs      =

modules   =

tests     =

EOT

