Last update was only a few days ago, but this submission fixes a critical bug 
in the code.

## Changelog

* BUG FIX `project_waypoints_coloured()`: Fix refactoring issue "Must supply a symbol or a string as argument" (#54).

* BUG FIX `project_waypoints_coloured()`: Fix wrong results when projecting waypoint segments (#54 bis).

## Test environments
* local R installation, R 4.0.5
* ubuntu 20.04, mac os x, windows (on github actions), R 4.0.5
* win-builder (release, devel)

## R CMD check results

```
── R CMD check results ────────────────────────────────────── dynplot 1.1.0 ────
Duration: 2m 6.5s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded
```
