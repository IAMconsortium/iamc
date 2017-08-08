
checkMax <- function(x, cfg) {
  return(list(message="%# values lie above allowed maximum",
              failed=helperBounds(x, cfg, "max")))
}
