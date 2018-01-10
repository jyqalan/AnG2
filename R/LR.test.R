LR.test <- function(x,y,full=TRUE,return.results=FALSE)
{
    # A function to carry out a likelihood ratio test for two fitted VB objects

    # Heads' up
    if( !(class(x)=="VB" && class(y)=="VB") )
    {stop(paste("At least one of ",deparse(substitute(x))," or ",deparse(substitute(y))," is not a VB objet",sep=""))}
    
    cat("\nCarrying out a likelihood ratio test of ",deparse(substitute(x))," and ",deparse(substitute(x)),":\n",sep="")
    if(full)
    {
        cat("(*) Full model:\t\t",deparse(substitute(x)),"\n")
        cat("(*) Reduced model:\t",deparse(substitute(y)),"\n")
        FULL <- x
        RED <- y        
    }
    else
    {
        cat("(*) Full model: ",deparse(substitute(y)),"\n")
        cat("(*) Reduced model: ",deparse(substitute(x)),"\n")
        FULL <- y
        RED <- x
    }

    # Do test
    LL.FULL <- logLik(FULL)
    LL.RED <- logLik(RED)
    CHI <- -2*(LL.RED - LL.FULL)
    DF <- length(FULL$par)-length(RED$par)
    p.CHI <- 1-pchisq(CHI,df=DF)
    
    OUT <- c(LL.FULL,LL.RED,CHI,DF,p.CHI)
    names(OUT) <- c("LL.Full","LL.Red","Chi","DF","p(X>Chi)")

    # Print some output or return if requested
    cat("\nTest results:\n")
    if(return.results)
    {return(OUT)}
    else{print(OUT)}
}
