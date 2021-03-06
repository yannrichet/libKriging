#!/usr/bin/env bash
set -eo pipefail

if [[ "$DEBUG_CI" == "true" ]]; then
  set -x
fi

if [[ "$ENABLE_COVERAGE" == "on" ]]; then
    cd "${TRAVIS_BUILD_DIR}"/build
    lcov --list coverage.info
    coveralls-lcov coverage.info
fi