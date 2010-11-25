#!/bin/tcsh

# Argument 1 is path of the source tree

# get all .F90 files in source tree
set CURDIR = `pwd`
cd $1; set FILES = `find . -name '*.F90'`
cd $CURDIR

foreach F (${FILES})
    cat $1/$F | sed -e 's/.F>/.F90>/' > tmp    
    cp tmp $1/$F
    cat $1/$F | sed -e 's/.F\"/.F90\"/' > tmp
    cp tmp $1/$F
end

exit 0
