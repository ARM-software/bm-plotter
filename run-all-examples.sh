#!/bin/bash

# Copyright (c) 2016, ARM Limited and Contributors.
# All rights reserved.
# SPDX-Licence-Identifier: BSD-3-Clause

set -eu

parallel --verbose ::: examples/*.sh
