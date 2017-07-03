checkUnits <- function(x, cfg) {
  x_var_unit <- unique(paste(x$variable, x$unit, sep=" | unit: "))
  cfg_var_unit <- unique(paste(cfg$variable, cfg$unit, sep=" | unit: "))
  failed <- x_var_unit[!(x_var_unit %in% cfg_var_unit)]
  return(list(message="%# variables are reported in the wrong unit",
              failed=failed))
}
