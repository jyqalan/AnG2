extract.mu <- function(VB, AGE=NULL, GROUP=NULL, calc.for.group=FALSE)
{
    # A (non-user) function to extract (mean) lengths-at-age from a VB
    # model fit, possibly for a subset of parameters within the fitted
    # model, possibly indexed by a GROUP. The user calls
    # extract.mean.length.at.age, which calls this function

    # ACHTUNG! The default settings will calculate mu(AGE) for all AGE
    # in the fitted dataset

    # Heads' up    
    if(calc.for.group && is.null(GROUP))
    {stop("calc.for.group is switched on, yet you have not specified a GROUP")}
    
    if(calc.for.group && (length(GROUP) > 1))
    {stop("calc.for.group is switched on, yet you have specified more than one value in GROUP")}

    if(calc.for.group && !is.character(GROUP))
    {stop("Your GROUP is not a character string")}

    if(!calc.for.group && !is.null(GROUP) && is.null(AGE))
    {stop("You have not specified any AGE data -- consider setting calc.for.group=TRUE if you want results for a particular GROUP...")}#<-- Don't need this -- recycling rule...

    if(!is.null(GROUP) && !all(unique(GROUP) %in% unique(as.character(VB$data$GROUP))) )
    {stop("Your GROUP is not a valid group name in VB$data$GROUP")}

    VB.MODEL <- VB$model$model.type
    VB.P <- VB$par
    VB.A1 <- VB$model$A1
    VB.A2 <- VB$model$A2

    # Make DATA (now the composition of DATA depends on the arguments
    # specified, which determines what, precisely, the output is
    # (GROUP etc.); this is *much* tidier than having a bunch of
    # spaghetti code ["if(...){...}else{...}" and so forth...]
    # statements

    DATA <-
        if(calc.for.group)
        {VB$data[VB$data$GROUP == GROUP, c("AGE", "GROUP")]}#<-- Get all VB$data indexed by GROUP
        else if(!calc.for.group && is.null(GROUP))
        {VB$data[, c("AGE", "GROUP")]}#<-- Get all VB$data
        else
        {data.frame(AGE=AGE, GROUP=GROUP)}#<-- Get the specified data


    # Now calculate mu
    RES <- apply(DATA, 1, function(x)
             {
                 G.P <- VB.P[grep(as.character(x[2]),names(VB.P))]
                 G.AGE <- as.numeric(x[1])#<-- applying across a dataframe with a character element causes all elements in the DF to be coerced to character...
                 OUT <- mu(AGE=G.AGE, p=G.P, A1=VB.A1, A2=VB.A2, model.type=VB.MODEL)
                 return(OUT)
             })

    # And go home
    return(RES)
}

