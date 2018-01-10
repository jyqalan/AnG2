schnute.support <- function(VB,WHICH=NULL)
{
    # A function to calculate inflection points and asymptotes for
    # Schnute models from the results of a fit.VB call, if appropriate

    # Heads' up
    MODEL <- VB$model$model.type
    if(is.null(WHICH))
    {stop("You have not specified any parameter names (argument WHICH) to evaluate an asymptote or inflection point with")}

    if( (MODEL=="S1") && (length(WHICH)!=4) )
    {stop("You have not specified the right number of parameter names")}

    if( (MODEL %in% c("S2","S5")) && (length(WHICH)!=3) )
    {stop("You have not specified the right number of parameter namess")}


    if(MODEL == "S1")
    {
        ASYM.f <- function(P)
        {
            L1 <- P[grep("L1",names(P))]
            L2 <- P[grep("L2",names(P))]
            K <- P[grep("K",names(P))]
            G <- P[grep("G",names(P))]

            A1 <- VB$model$A1
            A2 <- VB$model$A2

            RES <- ( ( (L2^G) - ( (exp(-K * (A2 - A1) )) * (L1^G) )  ) / (1 - exp(-K * (A2-A1) ) ) ) ^ (1/G)
            names(RES) <- NULL
            return(RES)
        }

        INFL.f <- function(P)
        {
            L1 <- P[grep("L1",names(P))]
            L2 <- P[grep("L2",names(P))]
            K <- P[grep("K",names(P))]
            G <- P[grep("G",names(P))]

            A1 <- VB$model$A1
            A2 <- VB$model$A2

            ASYM <- ASYM.f(P=P)

            RES <- Re(ASYM*(((1 - G) + 0i) ^ (1/G)))

            names(RES) <- NULL
            return(RES)
        }            
    }
    else if(MODEL == "S2")
    {
        ASYM.f <- function(P)
        {
            L1 <- P[grep("L1",names(P))]
            L2 <- P[grep("L2",names(P))]
            K <- P[grep("K",names(P))]

            A1 <- VB$model$A1
            A2 <- VB$model$A2

            RES <- exp( (log(L2) - ( exp(-K * (A2 - A1)) * log(L1))) / (1 - exp(-K * (A2 - A1))) )
            names(RES) <- NULL
            return(RES)
        }

        INFL.f <- function(P)
        {
            L1 <- P[grep("L1",names(P))]
            L2 <- P[grep("L2",names(P))]
            K <- P[grep("K",names(P))]

            A1 <- VB$model$A1
            A2 <- VB$model$A2
            
            ASYM <- ASYM.f(P=P)

            RES <- ASYM/exp(1)
            names(RES) <- NULL
            return(RES)
        }    
    }
    else if(MODEL == "S5")
    {
        ASYM.f <- function(P)
        {
            L1 <- P[grep("L1",names(P))]
            L2 <- P[grep("L2",names(P))]
            K <- P[grep("K",names(P))]

            A1 <- VB$model$A1
            A2 <- VB$model$A2            

            RES <- ( (L2) - ( exp( -K *(A2 - A1)) * (L1) ) ) / ( 1 - exp( -K * (A2 - A1) ) )
            names(RES) <- NULL
            return(RES)
        }

        INFL.f <- function(P)
        {
            RES <- 0
            return(RES)
        }    
    }


    # Calculate inflection point and asymptote
    if(MODEL %in% c("S3","S4"))
    {
        cat("\nACHTUNG! Neither cases 3 or 4 of the Schnute model have either an asymptote or inflection point\n")
        ASYM <- NULL
        INFL <- NULL
    }
    else if(MODEL == "VB")
    {
        cat("\nACHTUNG! The asymptote is parameterised in the VB model (Linf) and size inflexes at size = 0\n")
        ASYM <- NULL
        INFL <- NULL
    }
    else
    {
        P.use <- VB$par[WHICH]
        ASYM <- ASYM.f(P=P.use)
        INFL <- INFL.f(P=P.use)
    }

    OUT <- list(ASYM,INFL)
    names(OUT) <- c("asymptote","inflexion")
    return(OUT)
    
}
