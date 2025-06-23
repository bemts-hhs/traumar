## R CMD check results

0 errors | 0 warnings | 1 note

* This is a patch version release to 1.2.1 to fix a few minor issues. 
* No breaking changes are expected.
  
- Within the `trauma_performance()` function, renamed the variable `predicted_prob_death` to `scale_factor` which is commensurate with the source literature.

- updated comments in `trauma_performance()` for `z_method` method of the `Z_score` to reflect the right text

- In `trauma_performance()`, completed the comment where the `scale_factor` is created so that it is complete and clear

- Corrected a test error at CRAN from using bootstrap CI process in testing with 100,000 observations and 100 bootstrap samples to make sure `rmm()` and `rm_bin_summary()` ran in under 60 sec. That test has been removed as timing will be done outside unit tests.

- Cleaned up other tests within for relative_mortality.R that were checking for correct error / warning handling where multiple lines of output were sent to the console. Built a custom function to deal with those scenarios and correctly perform those unit tests.

*** 1 NOTE ***

❯ checking for future file timestamps ... NOTE
  unable to verify current time
