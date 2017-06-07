checkMin <- function(mx, cfg) {
  return(list(message="%# values lie below allowed minimum",
              failed=helperBounds(mx, cfg, "min")))
}
