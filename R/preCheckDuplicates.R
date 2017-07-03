#check for duplicate entries in data
preCheckDuplicates <- function(x) {
  id <- paste(x$model, x$scenario, x$region, x$variable, x$period, sep=" | ")
  return(list(message="data contains %# duplicate entries.",
              failed=id[duplicated(id)]))

}
