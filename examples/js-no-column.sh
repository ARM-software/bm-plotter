#!/bin/bash

# Copyright (c) 2016, ARM Limited and Contributors.
# All rights reserved.
# SPDX-Licence-Identifier: BSD-3-Clause

set -eu

SCRIPT_DIR=`dirname "${BASH_SOURCE[0]}"`
cd "$SCRIPT_DIR/.."

# Test that we can omit the 'Column' column.

./plot examples/data/js-no-column.csv --out=examples/js-no-column.svg
