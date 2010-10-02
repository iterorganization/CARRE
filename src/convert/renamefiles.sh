#!/bin/tcsh

# Argument 1 is path of the source tree
# Argument 2 is path of the target tree
# The directory structure of the target tree has to be set up properly

# get all .F files in source tree
set CURDIR = `pwd`
cd $1; set FILES = `find . -name '*.F'`
cd $CURDIR

foreach F (${FILES})
    # change filename
    set FBN = `basename $F`
    set FDN = `dirname $F`
    # set B2N = `echo $FBN | sed -e 's/b2/b2n/' | sed -e 's/.F/.F90/'`   
    set B2N = `echo $FBN | sed -e 's/.F/.F90/'`   
    # do not overwrite
    cp -n $1/$F $2/$FDN/$B2N
end

exit 0
