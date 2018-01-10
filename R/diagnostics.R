diagnostics <- function(x, res.type="response",
                        fit.xlab="Fitted values", fit.ylab="Residuals", qqnorm.xlab="Quantiles of the standard normal distribution", qqnorm.ylab="Residuals",
                        fit.xlim=NULL, fit.ylim=NULL, qqnorm.xlim=NULL, qqnorm.ylim=NULL, line=NULL, lty=1, ...)
{
    # Simple regression diagnostics method for VB objects. More will
    # (may) be added later, as and when...

    FIT <- fitted(x)
    RES <- residuals(x, type=res.type)

    plot(FIT, RES, xlab="", ylab="", xlim=fit.xlim, ylim=fit.ylim,...)
    title(xlab=fit.xlab, ylab=fit.ylab, line=line)
    abline(h=0, lty=lty)
    
    qqnorm(RES, xlab="", ylab="", main="", xlim=qqnorm.xlim, ylim=qqnorm.ylim,...)
    qqline(RES, lty=lty)
    title(xlab=qqnorm.xlab, ylab=qqnorm.ylab, line=line)
}
