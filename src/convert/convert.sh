#!/bin/tcsh

# first argument is source filename, e.g. b2mn.F
# optional second argument is target filename, e.g. b2mn.F90

# Style 1: Metcalf converter
# Style 2: simple Python script

set STYLE = 2


#set STEM = `basename $1 .F`
set STEM = `echo $1 | sed -e 's/\..*//'`
echo "Stem is $STEM"

# preconverter: input STEM.for, output STEM.f
ln -s $1 $STEM.for
echo "$STEM T T F /" > tmpcmd
echo "./preconvert $STEM"
./preconvert < tmpcmd
if ( $? != 0 ) then
    echo "FAIL: preconvert $STEM"
    exit 1
endif

# converter: input STEM.f, output STEM.f90
if ( $STYLE == 1 ) then
    # Style 1: Metcalf converter
    echo "$STEM 3 10 F F /" > tmpcmd
    echo "./convert $STEM"
    ./convert < tmpcmd
    if ( $? != 0 ) then
	echo "FAIL: convert $STEM"
	exit 2
    endif
endif

# Style 2: Python hack
if ( $STYLE == 2 ) then
    python ./convert.py $STEM.f $STEM.f90
endif

if ( $#argv > 1 ) then
    mv $STEM.f90 $argv[2]
endif

# clean up intermediate files
rm $STEM.for $STEM.f tmpcmd $STEM.log

exit 0
