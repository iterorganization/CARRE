#!/bin/sh

logdir=testcase-verify-logs

inputfiles=`find testcase-verify -name carre.dat`

if [ ! -d $logdir ]; then
    mkdir $logdir
fi

if [ ! $SOLPS_DEBUG = "" ]; then
    export EXT_DEBUG=".debug"
fi
export TOOLCHAIN=${HOST_NAME}.${COMPILER}${EXT_DEBUG}

for inputfile in $inputfiles; do

    dir=`dirname $inputfile`
    logfile=$logdir"/"`echo $dir | tr / _`

    echo "Running testcase $dir, logfile is $logfile"
    rm -f $logfile

    ./clear-case >>$logfile 2>&1
    cp $dir/* .  

    ./clear-case output >>$logfile 2>&1
    
    builds/$TOOLCHAIN/carre.exe >>$logfile 2>&1
    if [ $? -ne 0 -o ! -f carre.out ]; then
        echo "Testcase $dir: CARRE2 failed"
	continue
    fi    

    ./traduit-silo.sh >>$logfile 2>&1
    if [ $? -ne 0 -o ! -f traduitAAA00000 ]; then
        echo "Testcase $dir: traduit-silo.sh failed"
    fi    

    ./traduit-b2.sh >>$logfile 2>&1
    if [ $? -ne 0 -o ! traduit.out ]; then
        echo "Testcase $dir: traduit-b2.sh failed"
    fi    

    ./store-case $dir >>$logfile 2>&1

done

./clear-case
