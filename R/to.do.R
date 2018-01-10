##############
### To do list
##############

# Last updated 7 July 2004

# 
# (*) Replace silly LL expressions with dnorms
# (*) Add one variance parameter per group
# (*) Define a single "mu" function to be called within the fitting and plotting functions
# (*) Add Alistair's resampling test
# (*) Add checks for parameter names against model types
# (*) Add a single checking function?
# (*) Add a "sanity" and a "test" function - so that as much as possible can be removed from the LL function to speed up profiling
# (*) Add "proper" residual and fitted value methods
# (*) Write documentation
# (*) Write examples
# (*) Compile package and make availble for testing
# (*) See if the LL function can't be sped up somehow (is using the lapply(...) approach the most efficient?)
# (*) See if recursing optim can't be used to implement multi-phase estimation
# (*) Work out how to calculate LL derivatives on the fly so that they can be passed to optim
