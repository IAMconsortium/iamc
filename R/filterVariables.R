
#check variable names
filterVariables <- function(x, cfg) {
  return(list(message="data contains %# non-standard variable names",
              failed=setdiff(x$variable,cfg$variable)))
}
