## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a patch version release to 1.2.1 to add a few new features and
  enhancements to the existing functions. No breaking changes are expected.
  
- Within the `trauma_performance()` function, renamed the variable `predicted_prob_death` to `scale_factor` which is commensurate with the source literature.

- updated comments in `trauma_performance()` for `z_method` method of the `Z_score` to reflect the right text

- In `trauma_performance()`, completed the comment where the `scale_factor` is created so that it is complete and clear

- corrected a test error at CRAN from using bootstrap CI process in testing with 100,000 observations and 100 bootstrap samples to make sure `rmm()` and `rm_bin_summary()` ran in under 60 sec. That test now does not use the bootstrap process so the core function can be tested and will run in under a minute with 100,000 observations.
