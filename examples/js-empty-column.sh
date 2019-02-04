#!/bin/bash

# Copyright (c) 2019, ARM Limited and Contributors.
# All rights reserved.
# SPDX-Licence-Identifier: BSD-3-Clause

set -eu

SCRIPT_DIR=`dirname "${BASH_SOURCE[0]}"`
cd "$SCRIPT_DIR/.."

# Test that an empty 'Column' column is the same as no 'Column' at all.

./plot examples/data/js-empty-column.csv --out=examples/js-empty-column.svg
