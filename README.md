# `bm-plotter`

Plot graphs for comparing benchmark results.

In particular, the graphs are useful for analysing JavaScript benchmark results.
JavaScript benchmarks often exhibit bimodal (or otherwise noisy) behaviour, and
take a very long time to run, so simply examining the mean of a set of 10 runs
tends to be misleading. This tool tries to plot every result in a way such that
trends and patterns can be spotted.

## Dependencies

bm-plotter depends on the following Ubuntu packages (or equivalents):

    r-base
    r-cran-ggplot2
    libstatistics-r-perl
    libtext-csv-perl

## Usage

Run "./plot --help".

## Licence

This project is licensed under BSD-3-Clause.
Contributions are accepted under BSD-3-Clause.
