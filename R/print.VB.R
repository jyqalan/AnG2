print.VB <- function(x)
{
    cat("\nModel configuration:\n")
    CFG <- x$model
    CFG$model.groups <- length(CFG$model.groups)
    names(CFG)[2] <- "n.groups"
    print(format(CFG),quote=FALSE)
}
