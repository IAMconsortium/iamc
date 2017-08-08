helperBounds <- function(x, cfg, type) {
  x$ref <- cfg[[type]][x$variable]
  if(type=="min") {
    x$check <- (x$value>=x$ref)
  } else if(type=="max") {
    x$check <- (x$value<=x$ref)
  } else {
    stop("Unknown type ",type)
  }
  x$check[is.na(x$check)] <- TRUE
  f <- x[!x$check,]
  failed <- unique(paste(f$model, f$scenario, f$region, f$variable, f$period, sep=" | "))
  return(failed)
}
