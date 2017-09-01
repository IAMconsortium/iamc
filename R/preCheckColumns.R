preCheckColumns <- function(x) {
  standard_columns <- c("model","scenario","region","variable","unit","period","value")
  return(list(message="your data contains %# non-standard columns, which will be deleted.",
              failed=setdiff(colnames(x),standard_columns)))
}
