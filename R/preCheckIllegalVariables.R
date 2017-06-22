
#check variable names that are in x but not in template cfg and need to be filtered
preCheckIllegalVariables <- function(x, intersectVariables) {
    return(list(message="data contains %# non-standard variables",
                failed=setdiff(x$variable,intersectVariables)))

}
