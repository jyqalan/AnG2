print.summary.VB <- function(x)
{
    cat("\nModel configuration:\n")
    CFG <- x$model
    CFG$model.groups <- length(CFG$model.groups)
    names(CFG)[2] <- "n.groups"
    print(format(CFG),quote=FALSE)

    cat("\nGroup names:\n")
    print(x$model$model.groups)

    cat("\nMLEs of model parameters:\n")
    print(x$MLEs)

    if(is.null(x$SEs))
    {cat("\n",x$object," does not contain a hessian matrix, hence no standard errors are available just now, but you can also calculate them by hand, if you're game...\n",sep="")}
    else
    {
        cat("\nApproximate SEs and ", 100*(1-x$alpha),"% CI calculated from the hessian:\n",sep="")
        print(x$SEs)
    }

    cat("\nMaximum log.likelihood:\n")
    print(x$max.log.like)

    cat("\nAIC:\n")
    print(x$AIC)

    invisible(x)
}
