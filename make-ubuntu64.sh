#!/bin/sh

version=$1
[ -z "$version" ] && echo "first arg must be version" && exit 1

mkdir -p dist/deb/usr/local/bin
cp dist/build/check-graph/check-graph dist/deb/usr/local/bin
if which fpm >/dev/null 2>&1 ; then
    fpm -t deb -s dir -C dist/deb -d libffi-dev -d libgmp10 -n check-graph -v $version
else
    echo "Need fpm to build debian pkg: https://github.com/jordansissel/fpm#system-packages"
fi
