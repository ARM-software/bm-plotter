#!/bin/bash

# Copyright (c) 2016, ARM Limited and Contributors.
# All rights reserved.
# SPDX-Licence-Identifier: BSD-3-Clause

set -eu

SCRIPT_DIR=`dirname "${BASH_SOURCE[0]}"`
cd "$SCRIPT_DIR/.."

./plot examples/data/js-multiset.csv --out=examples/js-multiset.svg
