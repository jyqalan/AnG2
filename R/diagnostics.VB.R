diagnostics.VB <- function(x,res.type="response",xlim=NULL,ylim=NULL,line=1,...)                       
{
    FIT <- fitted(x)
    RES <- residuals(x,type=res.type)

    plot(FIT,RES,xlab="",ylab="",xlim=xlim,ylim=ylim,...)
    title(xlab="Fitted values",ylab="Residuals",line=line)
    
    qqnorm(RES,xlab="",ylab="",main="",...)
    qqline(RES)
    title(xlab="Quantiles of the standard normal distribution",ylab="Residuals",line=line)
}
