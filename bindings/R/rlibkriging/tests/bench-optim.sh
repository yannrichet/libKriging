#!/bin/bash

LIBKRIGING_PATH=/home/richet/libKriging

cd $LIBKRIGING_PATH
.travis-ci/r-linux-macos/build.sh

cd bindings/R
R CMD BUILD --no-multiarch --with-keep.source rlibkriging

cd rlibkriging/tests
Rscript -e "install.packages('../../rlibkriging_0.0-0_R_x86_64-pc-linux-gnu.tar.gz',lib='.'); library('rlibkriging',lib.loc='.'); png('"$LIBKRIGING_PATH"/bench-optim.png',800,800); source('testthat/bench-optim.R'); dev.off()"