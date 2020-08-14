#!/usr/bin/env bash
set -eo pipefail

if [[ "$DEBUG_CI" == "true" ]]; then
  set -x
fi

BASEDIR=$(dirname "$0")
BASEDIR=$(readlink -f "${BASEDIR}")

"${BASEDIR}"/../windows/install.sh

# https://chocolatey.org/docs/commands-install
# https://chocolatey.org/packages/make
choco install -y make --version 4.3

# Using chocolatey and manual command
# https://chocolatey.org/packages/R.Project
choco install -y r.project --version 3.6.0
if [ ! -f "C:/Rtools/VERSION.txt" ]; then
    # https://cran.r-project.org/bin/windows/Rtools
    curl -Lo ${HOME}/Downloads/Rtools.exe https://cran.r-project.org/bin/windows/Rtools/Rtools35.exe
    "${BASEDIR}"/install_rtools.bat
else
	echo "Rtools installation detected: nothing to do"
fi

# For R packaging
choco install -y zip

. ${BASEDIR}/loadenv.sh

## Using Anaconda
## -c r : means "from 'r' channel"
## https://anaconda.org/r/r
## https://anaconda.org/r/rtools
#${HOME}/Miniconda3/condabin/conda.bat install -y -c r r rtools


# Crazy hack since R try to call 'g++ ' as compiler
# Message looks like:
# /usr/bin/sh: line 8: g++ : command not found
#       with a space here ^
ln -sf "$(command -v g++).exe" "$(command -v g++) .exe"