#!/bin/sh

logdir=testcase-verify-logs

inputfiles=`find testcase-verify -name carre.dat`

if [ ! -d $logdir ]; then
    mkdir $logdir
fi

for inputfile in $inputfiles; do

    dir=`dirname $inputfile`
    logfile=$logdir"/"`echo $dir | tr / _`

    echo "Running testcase $dir, logfile is $logfile"
    rm $logfile

    ./clear-case >>$logfile 2>&1
    cp $dir/* .  

    ./clear-case output >>$logfile 2>&1
    
    $OBJECTCODE/carre >>$logfile 2>&1
    if [ $? -ne 0 -o ! -f carre.out ]; then
        echo "Testcase $dir: CARRE2 failed"
	continue
    fi    

    ./traduit-silo.sh >>$logfile 2>&1
    if [ $? -ne 0 -o ! -f traduitAAA00000 ]; then
        echo "Testcase $dir: traduit-silo.sh failed"
	continue
    fi    

    ./traduit-b2.sh >>$logfile 2>&1
    if [ $? -ne 0 -o ! traduit.out ]; then
        echo "Testcase $dir: traduit-b2.sh failed"
	continue
    fi    

    ./store-case $dir >>$logfile 2>&1

done

./clear-case
