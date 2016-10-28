#!/usr/bin/env perl

# Copyright (c) 2016, ARM Limited and Contributors.
# All rights reserved.
# SPDX-Licence-Identifier: BSD-3-Clause

use strict;
use warnings;

use Getopt::Long;
use Statistics::R;
use File::Temp;
use Text::CSV;

my @colours;
my $out = 'plot.svg';
my $reference = 'median';
my $run_r = 1;
my $legend = 1;
my @annotation_list;
my $sort = 'none';
my $bigger_is_better = 0;
my $x_label = '';
my $width = 1.0;
my $height = 1.0;
my $overplot = 'alpha';
my $minalpha = 0.1;

sub ExitWithUsage {
  my ($code) = @_;
  print(<<USAGE
Usage: $0 [option]... [data.csv]...
       $0 [option]... < data.csv

Plot graphs for comparing benchmark results.

In particular, the graphs are useful for analysing JavaScript benchmark results.
JavaScript benchmarks often exhibit bimodal (or otherwise noisy) behaviour, and
take a very long time to run, so simply examining the mean of a set of 10 runs
tends to be misleading. This tool tries to plot every result in a way such that
trends and patterns can be spotted.

CSV FORMAT

  A header row naming the columns is required. The columns can appear in any
  order, but the names of the columns are significant. In particular,
  unrecognised columns will cause an error (on the basis that they're probably
  typing errors). To remove unwanted columns from your input, use the `cut`
  utility.

  Mandatory columns:

    Result (number):
      Some kind of result to plot. By default, results are assumed to be
      benchmark run times or equivalent, so smaller results are considered
      better. To change this, use the --bigger-is-better option.

    Benchmark (string):
      A name that uniquely identifies a benchmark.

    Set (string):
      The name of the sets that you want to compare. Typically, this will be a
      build identifier of some kind.

      The first value of 'Set' that is encountered is assumed to be the
      baseline. Sort your input data accordingly. (The baseline determines the
      horizontal position of the benchmark results.)

  Optional columns:

    Column (string):
      If present, a separate graph will be drawn (side-by-side) for each value
      of Column. Typically, this can be used to show results from multiple
      platforms side-by-side, but it could also be used to show different result
      types (such as performance counter values) alongside the main result.

OPTIONS

  -h, --help
      Print this help text and exit.

  --out=<result>
      Output file name. The default is '$out'.

  --bigger-is-better
  --smaller-is-better
      Determine which results are considered best. This also affects the display
      order; better results appear further to the right.

      By default, smaller is better.

  --reference=<best|worst|mean|median>
      Select the reference point. The default is '$reference'.

      The meanings of 'best' and 'worst' change with --bigger-is-better.

  --colours=...
      Manually set colours to use for each 'Set'.

      Each colour should be a 6-digit hexadecimal code. Leading '#' characters,
      like HTML colour codes, are accepted but not required.

      The --colours argument can be specified multiple times, or multiple
      colours can be separated by a comma.

      For example: --colours=#000000 --colours=ff0000,00ff00,0000ff

      By default, something sensible will be used.

  --[no-]legend
      Turn the legend on or off. By default, the legend is shown.

  --sort=<none|ref>
      Sorting options (for the Benchmark column).

      none:             Don't sort. Specifically, benchmarks are shown in
                        order of first appearance.
      ref:              Sort by the average reference value (specified by
                        --reference) for each benchmark.

  --x-label=<label>
      Set the label for the X axis. The label is blank by default.

      Whilst a sensible default can be found that covers most use-cases, it's
      too easy to forget to change it when plotting other quantities, and the
      result is more confusing that simply omitting the label.

  --height=<multiplier>
  --width=<multiplier>
      Size multipliers, so you can draw really wide (or narrow) graphs.

  --overplot=<alpha|violin|jitter>
      Specify how to handle overplotting. The default is '$overplot'. All values
      other than "alpha" are experimental and may be removed.

  --minalpha=<minalpha>
      Set the minimum alpha value used for each point, where 0.0 is transparent
      and 1.0 is opaque. The default is $minalpha.

  --annotations=<annotation>...
  --annotate=<annotation>...
      Control extra annotations and marks. Some of these are on by default, but
      any annotation can be turned off using the "no-" prefix (or by using the
      special "none" value).

      For general use and presentation of results, the defaults should be the
      best. The other options are provided for debugging and verification, but
      may result in noisy or hard-to-read graphs.

      The --annotations argument can be specified multiple times, or multiple
      annotations can be separated by a comma.

      none:             Clear all annotations (including on-by-default ones).
      [no-]label-delta: Off by default. Show (in text) the performance
                        improvement, comparing the baseline reference to the
                        reference of each set. The reference point is determined
                        from the --reference option.
      [no-]hgrid:       On by default. Show faint horizontal grid lines to make
                        wide graphs easier to read.
      [no-]vgrid:       Off by default. Show faint vertical grid lines to allow
                        more accurate measurements to be taken.
      [no-]label-count: Show the number of results in each group.
USAGE
  );

  exit($code);
};

exit(1) unless GetOptions("help" => sub { ExitWithUsage(0) },
                          "out=s" => \$out,
                          "reference=s" => \$reference,
                          "colours=s" => \@colours,
                          "legend!" => \$legend,
                          "sort=s" => \$sort,
                          "overplot=s" => \$overplot,
                          "minalpha=f" => \$minalpha,
                          "annotations|annotate=s" => \@annotation_list,
                          "bigger-is-better" => sub { $bigger_is_better = 1 },
                          "smaller-is-better" => sub { $bigger_is_better = 0 },
                          "x-label=s" => \$x_label,
                          "width=f" => \$width,
                          "height=f" => \$height,
                          "run-R!" => \$run_r);  # Note that --[no-]run-R is undocumented.

# All of the possible values for --reference are just R functions, so once
# validated, we don't need to process it further.
unless ($reference =~ /^(?:best|worst|mean|median)$/) {
  print("Unsupported value for '--reference': $reference\n");
  ExitWithUsage(1);
}
# Internally, everything is normalised to smaller-is-better.
$reference = 'min' if ($reference eq 'best');
$reference = 'max' if ($reference eq 'worst');

unless ($sort =~ /^(?:none|ref)$/) {
  print("Unsupported value for '--sort': $sort\n");
  ExitWithUsage(1);
}

unless ($overplot =~ /^(?:alpha|violin|jitter)$/) {
  print("Unsupported value for '--overplot': $overplot\n");
  ExitWithUsage(1);
}

$minalpha = 0.0 unless ($minalpha > 0.0);
$minalpha = 1.0 unless ($minalpha < 1.0);

# Treat "--colours=aaaaaa,bbbbbb" like "--colours=aaaaaa, --colours=bbbbbb".
@colours = map { split(',', $_) } @colours;
@annotation_list = map { split(',', $_) } @annotation_list;

# Check colours.
for my $colour (@colours) {
  unless ($colour =~ /^#?([0-9a-fA-F]{6})$/) {
    print("Unsupported value for '--colours': $colour\n");
    ExitWithUsage(1);
  }
  $colour = "'#$1'";
}
# Create an R list of colours. (This list might be empty if no colours were
# manually specified.)
my $c_colours = 'c(' . join(', ', @colours) . ')';

# Apply annotation options.
my %annotations = ('label-delta' => 0, 'hgrid' => 1, 'vgrid' => 0, 'label-count' => 0);
for my $annotation (@annotation_list) {
  if ($annotation eq 'none') {
    $annotations{$_} = 0 for (keys %annotations);
    next;
  }

  $annotation =~ /^(no-)?(.*)$/;
  my $value = $1 ? 0 : 1;
  my $key = $2;
  die ("Unrecognised annotation") unless exists($annotations{$key});
  $annotations{$key} = $value;
}

$legend = $legend ? '"legend"' : 'FALSE';


# Concatenate and dump CSV input to a file.
my $csv_collated = File::Temp->new();   # TODO: Use mkfifo.
my $csv = Text::CSV->new();

my @columns = qw/ Column Set Benchmark Result /;
my %input_columns;
my $column_count = 0;
print($csv_collated "\n");
while (my $record = $csv->getline(*ARGV)) {
  next if ((@$record == 1) and ($record->[0] =~ /^\s*$/));
  die("Inconsistent column count") if ($column_count and ($column_count != @$record));
  $column_count = @$record;
  die("Expected 3 or 4 columns") if (($column_count != 3) and ($column_count != 4));

  # If the record looks like a header, treat it as such.
  my %new_input_columns;
  for (my $i = 0; $i < $column_count; $i++) {
    last unless ($record->[$i] =~ /^(Column|Set|Benchmark|Result)$/);
    $new_input_columns{$record->[$i]} = $i;
  }
  if (keys(%new_input_columns) == $column_count) {
    %input_columns = %new_input_columns;
    next;
  }

  # If we haven't got an input header, define a default one.
  unless (%input_columns) {
    my $i = 0;
    %input_columns = map { $_ => $i++ } @columns[-$column_count .. -1];
  }

  # Print the fields in the proper order.
  @$record = map {
    exists($input_columns{$_}) ? $record->[$input_columns{$_}] : 'DUMMY'
  } @columns;
  $csv->print($csv_collated, $record);
  print($csv_collated "\n");
}

my $facet = ($column_count == 4) ? 1 : 0;

my $src = <<R
library(grid, quietly=TRUE)
library(ggplot2, quietly=TRUE)
library(plyr, quietly=TRUE)
library(scales, quietly=TRUE)
library(gtable, quietly=TRUE)

data <- read.csv(file('$csv_collated'), header=FALSE,
                 col.names=c('Column', 'Set', 'Benchmark', 'Result'),
                 colClasses=c('character', 'character', 'character', 'numeric'))
if (length(row.names(data)) == 0) {
  cat("No input data for '$out'.\n");
  quit(status=1);
}
cat("Generating graph '$out'...\n");

# Manually convert strings to factors so we can preserve the ordering.
data\$Set <- factor(data\$Set, unique(data\$Set))
data\$Benchmark <- factor(data\$Benchmark, rev(unique(data\$Benchmark)))
data\$Column <- factor(data\$Column, unique(data\$Column))

# Drop everything that we don't recognise.
data <- data[, c("Column", "Set", "Benchmark", "Result")]

# Normalise to smaller-is-better.
if ($bigger_is_better) {
  data\$Result <- 1 / data\$Result;
}

# Assign baseline tags per group, in case some results are missing.
data <- ddply(data, c("Column", "Benchmark"), transform,
              IsBaseline=(as.numeric(Set) == min(as.numeric(Set))))

data\$Group = factor(paste(data\$Column, data\$Set, data\$Benchmark))
data <- ddply(data, "Group", transform,
              GroupRef=$reference(Result), GroupMin=min(Result), GroupMax=max(Result), GroupLength=length(Result))

# Calculate scores relative to the baseline. (The baseline is always the first
# specified build.)
ref <- unique(data[data\$IsBaseline, c("Column", "Benchmark", "GroupRef")])
names(ref)[names(ref) == "GroupRef"] <- "BaselineRef"
data <- merge(data, ref)

# Values higher than 0 are better (faster).
# Values lower than 0 are worse (slower).
data\$RelResult <- data\$BaselineRef / data\$Result - 1.0
data\$RelGroupMin <- data\$BaselineRef / data\$GroupMin - 1.0
data\$RelGroupMax <- data\$BaselineRef / data\$GroupMax - 1.0
data\$RelGroupRef <- data\$BaselineRef / data\$GroupRef - 1.0

if ("$sort" == "ref") {
  data.order <- ddply(data[, colnames(data) %in% c("Benchmark", "RelGroupRef")], c("Benchmark"), transform, Order=sum(RelGroupRef))
  data.order <- unique(data.order[order(data.order\$Order), colnames(data.order) %in% c("Benchmark", "Order")])
  data\$Benchmark <- factor(data\$Benchmark, levels=data.order\$Benchmark)
}

# The built-in 'percent' function sometimes produces NaNs for negative ratios,
# so we override it.
Percent <- function(ratio) {
  sign <- ifelse(ratio < 0, "-", "")
  magnitude <- percent(abs(ratio))
  paste0(sign, magnitude)
}

# Actually plot the graph.
set_count <- length(levels(data\$Set))
benchmark_count <- length(levels(data\$Benchmark))
y_steps_per_row <- set_count * 2 + 1
data\$YBase <- as.numeric(data\$Benchmark)
data\$Y <- data\$YBase + (as.numeric(data\$Set) - 0.5 - (set_count / 2)) / y_steps_per_row
data\$EvenRow <- (as.numeric(data\$Benchmark) %% 2) == 0
data\$Alpha <- $minalpha + ((1.0 - $minalpha) / data\$GroupLength)

colours <- $c_colours
if (length(colours) == 0) {
  # Use something like scale_colour_hue.
  hues <- seq(0, 360, length = length(levels(data\$Set)) + 1)
  length(hues) <- length(hues) - 1        # Remove the duplicated wrap-around value.
  colours <- hcl(h=hues+15, c=100, l=55)
} else {
  # Some colours were manually specified.
  # Assign the first colour only once (to the baseline). After that, recycle
  # colours as much as needed.
  if (length(colours) < 2) {
    colours <- rep_len(colours, 2)
  }
  first <- colours[1]
  others <- rep_len(colours[-1], length(levels(data\$Set)) - 1)
  colours <- c(first, others)
}

plot <- ggplot(data=data)
if ($facet) {
  plot <- plot + facet_grid(. ~ Column)
}

# Work out the plot area manually.
plot.xmin <- min(data\$RelResult)
plot.xmax <- max(data\$RelResult)
plot.xrange <- plot.xmax - plot.xmin
# A bit of padding.
plot.xlpad = 1/80
plot.xrpad = 1/80
if ($annotations{'label-delta'}) {
  # Space for labels. (Very rough.)
  plot.xrpad <- plot.xrpad + 1/10
}
if ($annotations{'label-delta'} && $annotations{'label-count'}) {
  # We only add space for label-count if we're also showing label-delta.
  plot.xrpad <- plot.xrpad + 1/10
}
plot.xscale <- plot.xrange / (1 - plot.xlpad - plot.xrpad)
plot.xmin <- plot.xmin - plot.xlpad * plot.xscale
plot.xmax <- plot.xmax + plot.xrpad * plot.xscale
plot.labelpad <- plot.xscale / 30

if ($annotations{'hgrid'}) {
  plot <- plot + geom_rect(data=data, aes(ymin=YBase-0.55, ymax=YBase-0.45), xmin=plot.xmin, xmax=plot.xmax, stat="unique", fill="#f8f8f8")
}

plot <- plot +
        theme(panel.background = element_rect(fill="#ffffff")) +
        theme(panel.margin = unit(4, "mm")) +
        theme(axis.ticks = element_blank()) +
        theme(axis.text = element_text(size=8)) +
        theme(axis.title.y = element_blank()) +
        theme(strip.background = element_rect(fill="#dddddd")) +
        theme(legend.background = element_blank()) +
        theme(legend.key = element_blank()) +
        theme(legend.title.align = 0.5) +
        scale_y_continuous(breaks=1:benchmark_count, labels=levels(data\$Benchmark), expand=c(0,0.25)) +
        scale_x_continuous(labels=Percent, expand=c(0,0), limits=c(plot.xmin, plot.xmax)) +
        scale_alpha_continuous(guide=FALSE, range=c(min(data\$Alpha), max(data\$Alpha))) +
        scale_colour_manual(values=colours, guide=$legend) +
        annotate("segment", x=0, xend=0, y=0, yend=max(data\$Y)+1, colour="#999999", size=0.15) +
        xlab('$x_label')

# The range marker is shown for all plot types.
plot <- plot + geom_segment(data=data, aes(x=RelGroupMin, xend=RelGroupMax, y=Y, yend=Y, colour=Set), size=0.15, alpha=0.4, stat="unique")

if ('$overplot' == 'alpha') {
  plot <- plot + geom_point(aes(x=RelResult, y=Y, alpha=Alpha, colour=Set), shape=18)
  plot <- plot + guides(colour = guide_legend(reverse=TRUE))

} else if ('$overplot' == 'violin') {
  GroupDensity <- function(group) {
    d <- density(group\$RelResult, bw=(plot.xrange / 300), kernel="epanechnikov")
    result <- data.frame(RelResult=d\$x, Density=d\$y)
    result\$Density <- result\$Density / (2.5 * y_steps_per_row * max(result\$Density))
    result\$Group <- rep(group\$Group[1], nrow(result))
    result\$Column <- rep(group\$Column[1], nrow(result))
    result\$Set <- rep(group\$Set[1], nrow(result))
    result\$Y <- rep(group\$Y[1], nrow(result))
    result\$Y1 <- result\$Y + result\$Density
    result\$Y2 <- result\$Y - result\$Density
    result
  }

  data.density <- ddply(data[, colnames(data) %in% c("Group", "Set", "Column", "Y", "RelResult")], "Group", GroupDensity)
  plot <- plot + geom_ribbon(data=data.density, aes(x=RelResult, ymin=Y1, ymax=Y2, fill=Set, group=Group))
  plot <- plot + guides(colour = FALSE,
                        fill = guide_legend(reverse=TRUE))

} else {  # 'jitter'
  plot <- plot + geom_point(aes(x=RelResult, y=Y, alpha=Alpha, colour=Set), shape=18, position=position_jitter(w=0))
  plot <- plot + guides(colour = guide_legend(reverse=TRUE))
}

if ($annotations{'label-delta'}) {
  if (length(which(!data\$IsBaseline)) > 0) {
    plot <- plot + geom_text(aes(x=RelGroupMin+plot.labelpad, y=Y, label=Percent(RelGroupRef)), colour='#dddddd', size=2, hjust=0, stat="unique", data=data[!data\$IsBaseline,])
    plot <- plot + geom_text(aes(x=RelGroupMin+plot.labelpad, y=Y, label=Percent(RelGroupRef), colour=Set), alpha=0.3, size=2, hjust=0, stat="unique", data=data[!data\$IsBaseline,])
  }
}

if ($annotations{'label-count'}) {
  plot <- plot + geom_text(aes(x=plot.xmax, y=Y, label=paste0("(", GroupLength, ")")), colour='#dddddd', size=2, hjust=1, stat="unique")
  plot <- plot + geom_text(aes(x=plot.xmax, y=Y, label=paste0("(", GroupLength, ")"), colour=Set), alpha=0.3, size=2, hjust=1, stat="unique")
}

if ($annotations{'vgrid'}) {
  plot <- plot + theme(panel.grid.minor = element_blank()) +
                 theme(panel.grid.major.x = element_line(colour="#eeeeee"))
} else {
  plot <- plot + theme(panel.grid.major = element_blank()) +
                 theme(panel.grid.minor = element_blank())
}

# Adjust the size so that the result is roughly consistent as rows and columns
# are added.
width <- 8 + (length(levels(data\$Column)) * 14 * $width)
height <- 3 + (benchmark_count * y_steps_per_row * 0.2 * $height)
ggsave('$out', width=width/2.54, height=height/2.54, limitsize=FALSE)
R
;

if ($run_r) {
  my $r_fh = File::Temp->new();
  my $r_file = $r_fh->filename;
  print($r_fh $src);
  (system('Rscript', $r_file) == 0) or exit(1);
} else {
  print($src);
}
