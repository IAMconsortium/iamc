
#check variable names that are not in x but maybe important
preCheckMissingVariables <- function(cfg, intersectVariables) {
    return(list(message="your data is missing the following %# standard variables ",
                failed=setdiff(cfg$variable,intersectVariables)))

}
