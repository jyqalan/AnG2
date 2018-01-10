calc.mu <- function(VB,GROUP=NULL)
{
    # A function to calculate mean length-at-age given a VB model fit
    # for a subset of parameters within the fitted model, indexed by a GROUP
    
    # Heads' up
    MODEL <- VB$model$model.type
    
    if(is.null(GROUP))
    {stop("You have not specified a GROUP")}

    if(!is.character(GROUP))
    {stop("GROUP is not a character vector")}

    if( !(GROUP %in% unique(VB$data$GROUP)) )
    {stop("GROUP is not a valid group name in VB$data$GROUP")}

    # Now calculate mu
    P <- VB$par[grep(GROUP,names(VB$par))]
    AGE <- VB$data$AGE[VB$data$GROUP == GROUP]

    RES <- mu(AGE=AGE,p=P,A1=VB$model$A1,A2=VB$model$A2,model.type=VB$model$model.type)
    return(RES)
}
