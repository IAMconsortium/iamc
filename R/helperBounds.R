helperBounds <- function(mx, cfg, type) {
  ref <- as.magpie(cfg[,c("variable",type)],datacol=2)
  if(type=="min") {
    check <- mx>=ref
  } else if(type=="max") {
    check <- mx<=ref
  } else {
    stop("Unknown type ",type)
  }
  check[is.na(check)] <- TRUE
  check <- as.quitte(check)
  f <- check[!check$value,]
  failed <- unique(paste(f$model, f$scenario, f$region, f$variable, f$period, sep=" | "))
  return(failed)
}
