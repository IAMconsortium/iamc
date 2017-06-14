## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ---- echo=TRUE----------------------------------------------------------
library(iamc)
iamCheck(example_landcover, cfg="CDLINKS")

## ---- echo=TRUE----------------------------------------------------------
# load cfg
cfg <- iamProjectConfig("CDLINKS")

# modify cfg
cfg$variable[cfg$variable=="Land Cover|Built-up Area"] <- "Land Cover|Built-upS Area"
cfg$max[cfg$variable=="Land Cover|Built-upS Area"] <- 100

# run check with new cfg
iamCheck(example_landcover, cfg=cfg)

## ---- echo=TRUE----------------------------------------------------------
library(iamc)

checkUnits <- function(x, cfg) {
  x_var_unit <- unique(paste(x$variable, x$unit, sep=" | unit: "))
  cfg_var_unit <- unique(paste(cfg$variable, cfg$unit, sep=" | unit: "))
  failed <- x_var_unit[!(x_var_unit %in% cfg_var_unit)]
  return(list(message="%# variables are reported in the wrong unit",
              failed=failed))
}

iamCheck(example_landcover, globalenv=TRUE)

# run again with modified cfg to trigger unit warning
cfg <- iamProjectConfig("CDLINKS")
cfg$unit[cfg$variable=="Land Cover"] <- "weird_unit"

iamCheck(example_landcover, cfg=cfg, globalenv=TRUE)


