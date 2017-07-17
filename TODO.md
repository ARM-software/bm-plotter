- Actually validate the input, including useful error messages.
- Provide examples with missing data (such as my performance counter charts).
- Provide examples showing how to handle mixed bigger-is-better and
  smaller-is-better inputs.
- Implement a BiggerIsBetter column so that Octane-like results can be mixed
  with time-like results.
- Add an annotation option that shows the absolute (reference) value.
- Make the width calculation more stable. (This requires fiddling with the grid
  layout that ggplot generates.)
- Allow the range to be controlled so that noisy benchmarks don't stretch the
  scale to the point of hiding the interesting things.
- Pass the R script line by line, so that R can report errors in a more useful
  manner.
- The use of the terms "best" and "worst" is probably misleading, particularly
  when used for performance counters, where we would intuitively use
  --bigger-is-better, but where a higher counter value is probably not better.
- Possible annotations:
  - Tufte-style ticks on the X axis indicating the range of each column.
  - Add a tick on the X axis for each result, showing overall density.
- Make the R script a separate file. It would be callable directly, but wouldn't
  do input validation and suchlike. Ultimately, it would be nice to write it all
  in R, but its CLI tools seem to be pretty rudimentary.
- Handle unicode (UTF-8) in the CSV file, for column labels and suchlike.
- Duplicate the X axis scale at the top of the plot.
- The plot script hangs if the input lacks an EOL on the last line. This could
  be improved.
- Be more robust about input data. Ignore or warn about columns with
  unrecognised names.
