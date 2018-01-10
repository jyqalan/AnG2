residuals.VB <- function(x,type="response",...)
{
    if(!(type %in% c("response","pearson","deviance")))
    {stop("No other residual types implemented yet...")}

    r <- x$residuals#<-- Response residuals produced by the fitting algorithm (fit.VB) by default
    mu <- fitted(x)
    
    y <- switch(type,
                response=r,
                pearson=r / sqrt(var(mu)),
                deviance=sign(r) * sqrt(r^2) )#<-- pearson and deviance residuals for a gaussian model are the same...
 
    return(y)
}
