# Add package to test renv
library(tictoc)
tic()
toc()

# Then run
renv::snapshot()
# It will ask you to update the lockfile