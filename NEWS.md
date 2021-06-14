# dynplot 1.1.0

Initial release on CRAN.

 * MINOR CHANGE: Add `arrow` parameter to all plot functions.
 
 * BUG FIX: Apply fixes for new versions of tibble, tidyr, and ggraph.
 
 * BUG FIX `optimise_order()`: Fix problem where GA::ga() wouldn't run on 
   milestone networks with 4 edges.

 * BUG FIX `linearise_cells()`: Fix ordering issue when `equal_cell_width` is `TRUE`.
 
 * MINOR CHANGE: Clean imports and supposed undefined variables.

# dynplot 1.0.2 (04-07-2019) 

 * BUG FIX: Fix weird ceiling warning.
 
 * BUG FIX: Fix for new select waypoints (#41).
 
 * MINOR CHANGE: Added parameters `size_cells`, `alpha_cells` and 
   `border_radius_percentage` to plotting functions that plot cells (#40).

# dynplot 1.0.1 (07-05-2019)

 * BUG FIX: Remove dependency on shades, fixing rgb2hsv bug

# dynplot 1.0.0 (28-03-2019)

 * Initial release of dynplot
 * Plotting of trajectories
 * Compatible with all methods of dynmethods
