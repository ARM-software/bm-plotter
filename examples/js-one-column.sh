#!/bin/bash

# Copyright (c) 2019, ARM Limited and Contributors.
# All rights reserved.
# SPDX-Licence-Identifier: BSD-3-Clause

set -eu

SCRIPT_DIR=`dirname "${BASH_SOURCE[0]}"`
cd "$SCRIPT_DIR/.."

# Test that we display a single column if the 'Column' row has a single value.

./plot examples/data/js-one-column.csv --out=examples/js-one-column.svg
