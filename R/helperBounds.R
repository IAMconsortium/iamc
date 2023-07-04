helperBounds <- function(x, cfg, type) {
  for (i in levels(x$variable)) {
    x[which(x$variable == i), "ref"] <- cfg[[type]][which(cfg$variable == i)]
  }
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
