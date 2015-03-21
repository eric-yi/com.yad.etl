#!/bin/sh
# yi_xiaobin@163.com
# 2015-03-18 Eric Yi

ROOT_DIR=$(cd ../"$(dirname "$0")"; pwd)
LIB_DIR=$ROOT_DIR/lib
ETC_DIR=$ROOT_DIR/etc
MAIN_ARGS=$(echo "'"-etc $ETC_DIR"'")
echo $MAIN_ARGS

cd $LIB_DIR ; erl -noshell -s etl main "${MAIN_ARGS}" -s init stop
