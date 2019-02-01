#!/bin/bash

# Copyright (c) 2019, ARM Limited and Contributors.
# All rights reserved.
# SPDX-Licence-Identifier: BSD-3-Clause

set -eu

SCRIPT_DIR=`dirname "${BASH_SOURCE[0]}"`
cd "$SCRIPT_DIR/.."

# Test that we can use the 'Direction' column to plot scores and timings but
# with an empty 'Column' column.

./plot examples/data/js-mix-score-and-time-empty-column.csv --out=examples/js-mix-score-and-time-empty-column.svg
