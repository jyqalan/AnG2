fit.VB <- function(p,LENGTH=NULL,AGE=NULL,GROUP=NULL,WEIGHTS=NULL,data=NULL,
                    model.type="VB",error.type="add.norm",obj=TRUE,noisy=TRUE,A1=NULL,A2=NULL,check.LL=FALSE,
                    hessian=TRUE,method="BFGS",control=list(),keep.data=TRUE,...)
{
    # Need to work on an arbitray deriv3 function to include with
    # VB.LL so that gradient attributes can be passed to optim

    # Note that much of the idiot-proofing needed for optim is done in
    # VB.LL - should it be done here? Probably - but doesn't seem to
    # slow the minimisation down unduly

    #################
    ### Sanity checks
    #################

    # Heads' up
    if(noisy){cat("\nPre-processing data:\n")}

    if(is.null(model.type))
    {stop("Model type not specified")}

    if(!(model.type %in% c("S1","S2","S3","S4","S5","VB")))
    {stop("Invalid model type specified")}

    if( (is.null(A1) || is.null(A2)) && (!(model.type == "VB")) )
    {stop("You have specified a Schnute model and at least one of A1 or A2 are NULL")}

    if(!(error.type %in% c("add.norm","mult.norm","log.norm")))
    {stop("Not a valid error structure (likelihood)")}

    
    # Check that objective function is switched on if check.LL is switched off
    if((!check.LL) && (!obj))
      {cat("(*) You are not minimising the objective function (consider changing the obj argument to FALSE)\n")}


    # Make WEIGHTS if null
    if(is.null(WEIGHTS))
    {
        WEIGHTS <- rep(1, length(LENGTH))
    }

    
    # Check if data is null and make a list if not then pass to optim
    # call below to avoid any unnecessary frigging around in the
    # minimisation (saves 0.02 seconds a call...)
    if(is.null(data))       
      {
        if(is.null(LENGTH) && is.null(AGE) && is.null(GROUP))
          {stop("You have not specified any data (all of LENGTH, AGE, GROUP, and data are NULL)!")}
        
        if(is.null(LENGTH) || is.null(AGE) || is.null(GROUP))
          {stop("One of  LENGTH, AGE, or GROUP is NULL!")}

        if( any(is.na(as.character(LENGTH))) || any(is.na(as.character(AGE))) || any(is.na(as.character(GROUP))) )
        {cat("(*) ACHTUNG! Your data contains missing values -- cases with missing values will be removed\n")}
        INDEX <- is.na(as.character(LENGTH)) | is.na(as.character(AGE)) | is.na(as.character(GROUP))

        LENGTH <- LENGTH[!INDEX]
        AGE <- AGE[!INDEX]
        GROUP <- GROUP[!INDEX]
        WEIGHTS <- WEIGHTS[!INDEX]

        TMP <- data.frame(LENGTH=LENGTH,AGE=AGE,GROUP=GROUP,WEIGHTS=WEIGHTS)
        DATA <- split(TMP,TMP$GROUP)
      }
    else
      {
        if( (!is.null(LENGTH)) || (!is.null(AGE)) || (!is.null(GROUP)) )
          {if(noisy){cat("(*) You have specified a \"data\" list and at least one of LENGTH, AGE, or GROUP is not NULL -- the latter will be ignored\n")}}
        DATA <- data
      }

    # Check parameter names, number, and whether appropriate for model
    # fitted (i.e. that the names match the model type and number of
    # groups in the model) and that DATA is OK (if !is.null(data)...)

    # Check that the objects in data are each data.frames with the
    # right names otherwise stop
    
    if(noisy){cat("...pre-processing completed\n")}

    ########################################
    ### Check the LL if asked and exit if so
    ########################################

    if(check.LL)
    {
        if(noisy){cat("\nChecking the model:\n")}
        CHECK <- VB.LL(p=p, LENGTH=LENGTH, AGE=AGE, GROUP=GROUP, WEIGHTS=WEIGHTS, data=DATA, model.type=model.type, error.type=error.type, obj=obj, noisy=TRUE, A1=A1, A2=A2)
        if(noisy){cat("...and leaving the building\n\n")}
        invisible(CHECK)
    }
    

    ###########################
    ### Otherwise Fit the model
    ###########################

    # Do the fit
    if(noisy){cat("\nFitting the model:\n")}
    FIT <- optim(p=p, VB.LL, LENGTH=LENGTH, AGE=AGE, GROUP=GROUP, WEIGHTS=WEIGHTS, data=DATA, model.type=model.type, error.type=error.type, obj=obj, noisy=FALSE, A1=A1, A2=A2,
                 hessian=hessian, method=method, control=control, ...)
    class(FIT) <- "VB"
    if(noisy){cat(".. model fit done\n")}

    # Bolt stuff describing the model onto FIT and leave the building
    if(noisy){cat("\nCleaning up:\n")}

    # Model type
    FIT$model$model.type <- model.type
    
    # Define groups
    if(is.null(data))
      {model.groups <- as.character(sort(unique(GROUP)))}
    else
      {model.groups <- names(data)}
    FIT$model$model.groups <- model.groups

    # Error structure
    FIT$model$error.type <- error.type

    # A1 and A2
    FIT$model$A1 <- A1
    FIT$model$A2 <- A2
    
    # Keep data?
    if(keep.data)
      {
        if(is.null(data))
          {
            FIT$data <- list(LENGTH,AGE,GROUP)
            names(FIT$data) <- c("LENGTH","AGE","GROUP")
          }
        else
          {

            TMP <- list(TMP.LENGTH,TMP.AGE,TMP.GROUP)
            names(TMP) <- c("LENGTH","AGE","GROUP")
            FIT$data <- TMP
          }
      }
    else
      {FIT$data <- NULL}

    # Make fitted values and residuals (there has to be a smarter way to do this)
    RESIDUALS <- FITTED <- as.list(unique(FIT$data$GROUP))
    FITTED <- unlist(lapply(FITTED,function(x)
                        {
                            XNAME <- as.character(x)
                            y <- calc.mu(FIT,GROUP=XNAME)

                            SIGMASQ <- FIT$par[grep("SigmaSq",names(FIT$par))]

                            y <- switch(error.type,
                                        add.norm  = y,
                                        mult.norm = log(y),
                                        log.norm  = exp(log(y) +  (SIGMASQ/2)) )

                            return(y)
                        }))

    RESIDUALS <- unlist(lapply(RESIDUALS,function(x)
                           {
                               XNAME <- as.character(x)
                               y <- FIT$data$LENGTH[FIT$data$GROUP == XNAME]

                               y <- switch(error.type,
                                           add.norm  = y,
                                           mult.norm = log(y),
                                           log.norm  = y )

                               return(y)
                           }))
    
    RESIDUALS <- FITTED-RESIDUALS
    FIT$fitted <- FITTED
    FIT$residuals <- RESIDUALS

    #######################
    ### Say goodbye, nicely
    ######################
    
    if(noisy){cat("...and leaving the building\n\n")}
    invisible(FIT)

}
