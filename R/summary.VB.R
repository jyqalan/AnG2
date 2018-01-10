summary.VB <- function(VB,alpha=0.05)
{
    # A summary method for VB objects the main purpose of which is to
    # calculate SEs and analytical approximate univariate CIs

    # Currently only:
    
    # (1.) summarises model fitted
    OBJECT <- deparse(substitute(VB))
    MODEL <- VB$model

    # (2.) parameters
    P <- VB$par

    # (3.) Uunivariate standard errors calcualted from the
    # hessian (AKA, the inverse of the Fisher Information Matrix...)
    if(is.null(VB$hessian))
    {OUT <- NULL}
    else
    {
        SE <- sqrt(diag(solve(VB$hessian)))
        Z <- qnorm(1-(alpha/2))
        OUT <- cbind(P,SE,P-(Z*SE),P+(Z*SE))
        colnames(OUT) <- c("Estimate","Std.Error","CI.LB","CI.UB")
    }    

    # (4.) MaxLLike and AIC
    MLL <- logLik(VB)
    AIC <- AIC(VB)

    RES <- list(OBJECT,MODEL,P,OUT,alpha,MLL,AIC)
    names(RES) <- c("object","model","MLEs","SEs","alpha","max.log.like","AIC")
    class(RES) <- c("summary.VB")
    return(RES)
}
