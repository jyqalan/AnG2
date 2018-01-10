VB.LL <- function(p, LENGTH=NULL, AGE=NULL, GROUP=NULL, WEIGHTS=NULL, data=NULL, model.type="VB", error.type="add.norm", obj=FALSE, noisy=TRUE, A1=NULL, A2=NULL)
{
    ###########################################################################
    ### Apply the LL function over the DATA chunks and sum to find the joint LL
    ###########################################################################
    
    # Calculate OBJ, a constant that specifies whether the log.LL or
    # -ve log.LL (an OBJ function for optim, say...) will be returned
    if(obj)
    {OBJ <- (-1);FUNC <- "objective function"}
    else
    {OBJ <- (1);FUNC <- "log-likelihood"}

    # Evaluate the marginal LLs and sum to find the joint LL and return a scalar output
    RES <- sapply(data,function(x,y)
              {
                  x.name <- as.character(unique(x$GROUP))
                  p.use <- p[grep(x.name,names(p))]
                  SigmaSq <- p[names(p)=="SigmaSq"]#<-- Here's the point to let in multiple SigmaSqs
                  N <- length(x$LENGTH)

                  y <- OBJ * sum(marginal.LL(LENGTH=x$LENGTH, AGE=x$AGE, WEIGHTS=x$WEIGHTS, p=p.use, SigmaSq=SigmaSq, N=N, A1=A1, A2=A2,model.type=model.type,error.type=error.type))
                  return(y)
              })
    RES <- c(RES,sum(RES))
    names(RES) <- c(names(data),"Total")

    if(noisy)
    {
        cat("Contribution to the joint ",FUNC ," by GROUP:\n",sep="")
        print(RES)
        cat("\n")
    }

    # Say "Bye bye"
    invisible(RES["Total"])
}
