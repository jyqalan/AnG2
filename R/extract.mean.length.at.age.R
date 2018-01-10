extract.mean.length.at.age <- function(VB, AGE=NULL, GROUP=NULL, calc.for.group=FALSE)
{
    # A function to extract (calculate) mean length-at-age given a
    # particular model fit (i.e., a particular assumed error structure
    # -- likelihood -- and all sorts of other things)

    # Heads' up
    ERROR <- VB$model$error.type
    SIGMASQ <- VB$par[grep("SigmaSq", names(VB$par))]

    # Do work
    RES <- extract.mu(VB=VB, AGE=AGE, GROUP=GROUP, calc.for.group=calc.for.group)

    RES <- switch(ERROR,
                  add.norm  = RES,
                  mult.norm = log(RES),#<-- This is strictly correct...
                  log.norm  = exp(log(RES) + (1/2)*SIGMASQ),
                  cv.norm   = RES)

    # Go home
    return(RES)
}

