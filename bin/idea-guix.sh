#!/bin/bash

# untested by me, from https://www.draketo.de/software/guix-work.html#intellij
cd ~/
set -x

_IJ_PATH=idea-IU-193.6911.18

while getopts :i: OPT; do
    case $OPT in
        i|+i)
            _IJ_PATH="$OPTARG"
            ;;
        *)
            echo "usage: `basename $0` [+-i ARG} [--] ARGS..."
            exit 2
    esac
done
shift `expr $OPTIND - 1`
OPTIND=1

# echo $_IJ_PATH
# exit 0

# HACK: use system guix to avoid glibc inconsistency problems
# PATH=/var/guix/profiles/system/profile/bin/:$PATH

JAVA17_PACKAGE="openjdk@17"
JDK17_PATH="$(guix build ${JAVA17_PACKAGE} | grep -- '-jdk$')"
# need openjdk 15, because 16 stops with module errors
JAVA15_PACKAGE="openjdk@15"
JDK15_PATH="$(guix build ${JAVA15_PACKAGE} | grep -- '-jdk$')"
# need openjdk 14, because 16 stops with module errors
JAVA14_PACKAGE="openjdk@14"
JDK14_PATH="$(guix build ${JAVA14_PACKAGE} | grep -- '-jdk$')"
# also need openjdk 11 as the official build configuration
JAVA11_PACKAGE="openjdk@11"
JDK11_PATH="$(guix build ${JAVA11_PACKAGE} | grep -- '-jdk$')"
# need to track libstdc++ in the dependencies of the GCC toolchain
GCC_TOOLCHAIN="gcc-toolchain"
GCC_LIB_PATH="$(grep -oE "[^\"]*gcc-12[^\"]*-lib" $(grep -oE "[^\"]*gcc-12[^\"]*drv" $(guix build -d ${GCC_TOOLCHAIN})) | head -n 1)"
SQLITE_PATH="$(guix build sqlite | head -n 1)"
SPATIALITE_PATH="$(guix build libspatialite)"

MAVEN_HOME=$(guix build maven)

exec -a "$0" guix environment --ad-hoc ${JAVA11_PACKAGE}:jdk ${JAVA14_PACKAGE}:jdk ${JAVA15_PACKAGE}:jdk openjdk@16:jdk ${JAVA17_PACKAGE}:jdk glibc ${GCC_TOOLCHAIN} sqlite libspatialite maven zlib e2fsprogs bash -- bash -c "
export JDK15_PATH=\"$(guix build ${JAVA15_PACKAGE} | grep -- '-jdk$')\"
export JDK14_PATH=\"$(guix build ${JAVA14_PACKAGE} | grep -- '-jdk$')\"
# keep working in exwm
export _JAVA_AWT_WM_NONREPARENTING=1
export AWT_TOOLKIT=MToolkit
export JDK11_PATH="$(guix build ${JAVA11_PACKAGE} | grep -- '-jdk$')"

LD_LIBRARY_PATH="'${GUIX_ENVIRONMENT}'"/lib:${SQLITE_PATH}/lib:${SPATIALITE_PATH}/lib:${GCC_LIB_PATH}/lib:."' MAVEN_HOME='"${MAVEN_HOME}"' IDEA_JDK='"${JDK14_PATH}"' exec -a '"$0"' bash -x '"$_IJ_PATH"'/bin/idea.sh'

# change to IJPATH above
