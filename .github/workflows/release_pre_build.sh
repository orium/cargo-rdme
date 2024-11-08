#!/bin/bash

set -e

# See https://github.com/rust-build/rust-build.action/pull/99.
apk add --no-cache zlib-static
