
checkMax <- function(mx, cfg) {
  return(list(message="%# values lie above allowed maximum",
              failed=helperBounds(mx, cfg, "max")))
}
