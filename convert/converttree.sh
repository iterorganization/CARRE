#!/bin/tcsh

# Argument 1 is path of the source tree
# Argument 2 is path of the target tree
# The directory structure of the target tree has to be set up properly

# get all .F files in source tree
set CURDIR = `pwd`
cd $1; set FILES = `find . -name '*.F'`
cd $CURDIR

foreach F (${FILES})
    echo "Converting $F"
    # Copy to working directory
    cp $1/$F .
    # convert
    set FBN = `basename $F`
    echo "./convert.sh $FBN"
    ./convert.sh $FBN
    if ( $? != 0 ) then
	    echo "FAIL: convert.sh $F"
	    exit 1
    endif

    # Copy to correct place in target tree
    set F90 = `echo $FBN | sed -e 's/.F/.f90/'`
    set F90TARGET = `echo $F | sed -e 's/.F/.F90/'`
    #set F90TARGET = `echo $F | sed -e 's/.F/.F/'`
    cp $F90 $2/$F90TARGET
    # cleanup
    rm $FBN $F90
end

exit 0
