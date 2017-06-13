
#check variable names
preCheckVariables <- function(data, intersectVariables, type) {
  if(type=="x") {
    return(list(message="data contains %# non-standard variable names",
                failed=setdiff(data$variable,intersectVariables)))
  } else if(type=="cfg") {
    return(list(message="data missing following standard variable names %# ",
                failed=setdiff(data$variable,intersectVariables)))
  } else {stop("undeined parameter for type")}
}
