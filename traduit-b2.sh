#!/bin/sh

if [ ! $SOLPS_DEBUG = "" ]; then
    export EXT_DEBUG=".debug"
fi
export TOOLCHAIN=${HOST_NAME}.${COMPILER}${EXT_DEBUG}

if [ ! -e builds/$TOOLCHAIN/traduit.exe ]; then
    echo "No traduit program available"
    exit
fi

cat << EOF | builds/$TOOLCHAIN/traduit.exe
carre.out
2
EOF
