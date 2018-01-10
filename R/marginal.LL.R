marginal.LL <- function(LENGTH, AGE, WEIGHTS, p, SigmaSq, N, A1, A2, model.type, error.type)
{
    OUT <- switch(error.type,
                  add.norm  = WEIGHTS * log(dnorm(x=LENGTH,mean=mu(AGE=AGE,p=p,A1=A1,A2=A2,model.type=model.type),sd=sqrt(SigmaSq))),
                  mult.norm = WEIGHTS * log(dnorm(x=log(LENGTH),mean=log(mu(AGE=AGE,p=p,A1=A1,A2=A2,model.type=model.type)),sd=sqrt(SigmaSq))),
                  log.norm  = WEIGHTS * log(dlnorm(x=LENGTH,meanlog=log(mu(AGE=AGE,p=p,A1=A1,A2=A2,model.type=model.type)),sdlog=sqrt(SigmaSq))))
    return(OUT)
}
