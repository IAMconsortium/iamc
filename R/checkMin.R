checkMin <- function(x, cfg) {
  return(list(message="%# values lie below allowed minimum.",
              failed=helperBounds(x, cfg, "min")))
}
