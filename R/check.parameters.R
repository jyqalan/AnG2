check.parameters <- function(p, DATA, model.type, noisy)
{
    # A function to check that a parameter vector passed to VB.LL is
    # consistent with the numbers of groups in the data and is
    # correctly named and possibly a bunch of other things if I ever
    # get around to adding them

    # Length of parameters vector
    if(model.type %in% c("S1"))
    {
        if( !(length(p) == ( (4 * length(DATA)) + 1)) )
          {stop("The length of p does not match the model you have selected and the number of groups in your data")}        
    }
    else if(model.type %in% c("S2","S3","S5","VB"))
    {
        if( !(length(p) == ( (3 * length(DATA)) + 1)) )
          {stop("The length of p does not match the model you have selected and the number of groups in your data")}
    }
    else if(model.type %in% c("S4"))
    {
        if( !(length(p) == ( (2 * length(DATA)) + 1)) )
          {stop("The length of p does not match the model you have selected and the number of groups in your data")}
    }

    
    # Check if p has a names attribute and print it out if asked
    if(is.null(names(p)))
      {
        if(!is.null(p.names))
          {names(p) <- p.names}
        else
          {stop("ACHTUNG! p has no names attribute")}
      }

    if(noisy)
      {
        cat("\nParameter vector:\n")
        print(p)
        cat("\n")
      }

    
    
}
