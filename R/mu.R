mu <- function(AGE, p, A1=NULL, A2=NULL, model.type=NULL)
{
    # Below would be slicker with a switch...
    
    if(model.type=="S1")
    {
        L1 <- p[1]
        L2 <- p[2]
        K <- p[3]
        G <- p[4]
        OUT <- ( (L1^G) + ( ( (L2^G) - (L1^G) ) * ( ( 1 - exp(-K * (AGE - A1)) ) / (1 - exp(-K * (A2 - A1))) ) ) ) ^ (1/G)
    }
    else if(model.type=="S2")
    {
        L1 <- p[1]
        L2 <- p[2]
        K <- p[3]
        OUT <- L1 * exp( log(L2/L1) * ( ( 1 - exp(-K * (AGE - A1)) ) / (1 - exp(-K * (A2 - A1))) ) )
    }
    else if(model.type=="S3")
    {
        L1 <- p[1]
        L2 <- p[2]
        G <- p[3]
        OUT <- ( (L1^G) +  (  ( (L2^G) - (L1^G) ) * ( (AGE - A1) / (A2 - A1) ) )  ) ^ (1/G)
    }
    else if(model.type=="S4")
    {
        L1 <- p[1]
        L2 <- p[2]
        OUT <- L1 * exp(  log(L2/L1) * ( (AGE - A1) / (A2 - A1) )  )
    }
    else if(model.type=="S5")
    {
        L1 <- p[1]
        L2 <- p[2]
        K <- p[3]
        OUT <- L1 + (L2 - L1) * ( (1 - exp(-K * (AGE - A1)) ) / (1 - exp(-K * (A2 - A1))) )
    }
    else if(model.type=="VB")
    {
        Linf <- p[1]
        K <- p[2]
        t0 <- p[3]
        OUT <- Linf * (1 - exp( -K * (AGE - t0)))
    }
    
    return(OUT)    
}
