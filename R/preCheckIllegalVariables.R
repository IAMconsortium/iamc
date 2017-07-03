
#check variable names that are in x but not in template cfg and need to be filtered
preCheckIllegalVariables <- function(x, intersectVariables) {
    return(list(message="your data contains %# non-standard variables, which will be deleted",
                failed=setdiff(x$variable,intersectVariables)))

}
