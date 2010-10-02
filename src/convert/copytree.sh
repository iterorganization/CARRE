#!/bin/tcsh

# Create an empty copy of an directory tree

# Argument 1 is path to reference tree
# Argument 2 is path to new tree

if (! -e $2) mkdir $2

set CURDIR = `pwd`
cd $1
set DIRS = `find . -type d | grep -v svn`
cd $CURDIR

cd $2
foreach D (${DIRS})
    mkdir $D
end
