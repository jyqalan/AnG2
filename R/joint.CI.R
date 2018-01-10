joint.CI <- function(VB,WHICH=NULL,CHI.Q=0.95,CHI=NULL,LABELS=NULL,P1=NULL,P2=NULL,alpha=0.05,domain=3,units=51,noisy=FALSE,
                     add=FALSE,draw.conf=TRUE,draw.like=TRUE,draw.centre=TRUE,
                     conf.fill=FALSE,centre.pch=3,centre.cex=1,
                     xlab=NULL,ylab=NULL,main=NULL,output=FALSE,
                     L.LIKE.levels=NULL,L.LIKE.col="darkgrey",
                     CHI.lty=3,CHI.col="black",CHI.lwd=2,CHI.border=CHI.col,...)
{
    # A function to draw a confidence region of the specified (1 -
    # alpha) level of confidence around the parameter values for a
    # particular group in a length-at-age dataset

    # Takes as input a fitted VB.LL object, extracts needed
    # information, then draws the CR for the specified parameters
    # ("WHICH") conditioning ON EVERY OTHER parameter in the fitted
    # model


    # Heads' up
    require(clines)#<-- This is temporary untill R gets updated
    
    if(noisy)cat("\nDoing sanity checks...")    
    if(!(class(VB)=="VB"))
      {stop(paste(deparse(substitute(VB)), " is not a VB object (i.e., a fitted VB model of class == \"VB\" produced after a call to fit.VB()\n",sep=""))}

    if(is.null(WHICH))
      {stop("You haven't specified parameters to draw confidence regions for\n")}

    if(any(!(WHICH %in% names(VB$par))))
      {stop(paste("At least one of the parameters you specified was not estimated when ",deparse(substitute(VB)) ," was fitted\n",sep=""))}

    if(!(VB$convergence == 0))
      {stop(paste(deparse(substitute(VB)), " did not converge successfully\n",sep=""))}

    if(is.null(VB$data))
       {stop(paste(deparse(substitute(VB))," does not contain any data\n",sep=""))}

    MODEL <- VB$model$model.type
    ERROR <- VB$model$error.type
    DATA <- VB$data
    EST <- VB$par
    COEF <- EST[names(EST) %in% WHICH]
    SE <- sqrt(diag(solve(VB$hessian)))
    Z <- qnorm((1-(alpha/2)),0,1)
    MAX.L.LIKE <- -VB$value#<-- if class(VB)==VB, then VB$value == min value of obj function
    if(noisy)cat("done\n")

    # Make a grid of parameter values
    if(noisy)cat("\nMaking a grid...")
        if(is.null(P1))
      {P1 <- COEF[1] + seq(-domain * SE[1], domain * SE[1], length=units)}
    if(is.null(P2))
      {P2 <- COEF[2] + seq(-domain * SE[2], domain * SE[2], length=units)}
    GRID <- expand.grid(P1,P2)
    if(noisy)cat("done\n")

    # Make DATA from the data in VB
    if(noisy){cat("\nExtracting data...")}
    TMP <- data.frame(VB$data$LENGTH,VB$data$AGE,VB$data$GROUP)
    names(TMP) <- c("LENGTH","AGE","GROUP")
    DATA.list <- split(TMP,TMP$GROUP)
    if(noisy)cat("done\n")

    # Calculate the value of the log-likelihood function for the
    # values in GRID conditioning on every other parameter in the
    # model
    if(noisy)cat("\nEvaluating the LL function at each point in the grid conditioning on all other parameters...")
    L.LIKE.VALS <- apply(GRID,1,function(x)
                         {
                           y <- x
                           names(y) <- WHICH
                           p <- EST
                           p[WHICH] <- y
                           names(p) <- names(EST)

                           OUT <- VB.LL(p=p,data=DATA.list,model.type=VB$model$model.type,error.type=VB$model$error.type,noisy=FALSE,obj=FALSE,A1=VB$model$A1,A2=VB$model$A2)
#                           OUT <- VB.LL2(p=p,data=DATA.list,model.type=VB$model$model.type,error.add=VB$model$error.add,noisy=FALSE,obj=FALSE,A1=VB$model$A1,A2=VB$model$A2)
#                           OUT <- VB.LL(p=p,LENGTH=DATA$LENGTH,AGE=DATA$AGE,GROUP=DATA$GROUP,data=DATA.list,error.add=ERROR,noisy=FALSE,obj=FALSE)
                           return(OUT)
                         })
    if(noisy)cat("done\n")

    # Calculate the likelihood ratio test statistic comparing
    # MAX.L.LIKE and L.LIKE.VALS and make +ve to allow the confidence
    # region inequality to be "solved"
    if(noisy)cat("\nCalculating the likelihood ratio test statistic...")
    CHI.VALS <- -2 *  (L.LIKE.VALS - MAX.L.LIKE)
    CHI.Qs <- qchisq(CHI.Q,df=1)
    CHI.mat <- matrix(CHI.VALS,nrow=length(P1),ncol=length(P2),byrow=FALSE)
    dimnames(CHI.mat) <- list(P1,P2)
    
    L.LIKE.mat <- matrix(L.LIKE.VALS,nrow=length(P1),ncol=length(P2),byrow=FALSE)
    dimnames(L.LIKE.mat) <- list(P1,P2)
    if(noisy)cat("done\n")

    # Draw a plot or add contours to an existing plot as required
    if(noisy)cat("\nDrawing plot...")
    if(is.null(LABELS))
      {drawlabels=FALSE}

    if(!(add))
      {plot(c(min(P1),max(P1)),c(min(P2),max(P2)),xlab="",ylab="",main="",type="n")}
    
    if(draw.like)
      {
        if(is.null(L.LIKE.levels))
          {tmp.levels <- pretty(L.LIKE.VALS,n=21)}
        else
          {tmp.levels <- L.LIKE.levels}
        contour(P1,P2,L.LIKE.mat,levels=tmp.levels,add=TRUE,col=L.LIKE.col,lty=3)
      }

    if(!(is.null(CHI)))
      {contour(P1,P2,CHI.mat,levels=CHI,labels=LABELS,drawlabels=drawlabels,add=TRUE,lty=CHI.lty,col=CHI.col,lwd=CHI.lwd)}
    
    if(draw.conf)
      {
        if(conf.fill)
          {
            CONF <- clines(P1,P2,CHI.mat,levels=CHI.Qs)
            lapply(CONF,polygon,col=CHI.col,border=CHI.border)
          }
        else
          {contour(P1,P2,CHI.mat,levels=CHI.Qs,labels=LABELS,drawlabels=drawlabels,add=TRUE,col=CHI.col,...)}
      }
    
    if(draw.centre)
      {
        points(COEF[1],COEF[2],pch=centre.pch,cex=centre.cex)
      }

    if(!(add))
      {
        if(is.null(xlab)){title(xlab=WHICH[1])}
        if(is.null(ylab)){title(ylab=WHICH[2])}
        if(is.null(main)){title(main=paste("A profile of ",WHICH[1]," and ",WHICH[2],sep=""))}
      }
    if(noisy)cat("done\n")
    
    # Generate and return some output
    UNI <- cbind((EST - (Z*SE)), EST, (EST + (Z*SE)))
    colnames(UNI) <- c("LB","EST","UB")

    cat("\nUnivariate confidence intervals calculated from the hessian for each paramter",sep="")
    cat(" calculated at the alpha == ", alpha," level of confidence",sep="")
    cat(" (LB == lower bound, EST == estimate, UB == upper bound):\n",sep="")

    print(UNI,sep="\t")
    cat("\n")

    if(output)
      {
        OUT <- list(UNI,L.LIKE.mat,CHI.mat)
        names(OUT) <- c("Univariate.CIs","log.likelihoods","Chi.square")
        return(OUT)
      }
}
