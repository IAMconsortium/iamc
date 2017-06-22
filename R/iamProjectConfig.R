#' iamProjectConfig
#'
#' Function to return available variables and corresponding properties for a given project configuration.
#'
#' @param cfg Project configuration that should be used. Either a project name (currently available: "examplePROJECT"), a path to a
#' config file or a data frame specifying available variables and corresponding properties
#' @return data frame containing avaible variables and corresponding properties
#' @author Jan Philipp Dietrich
#' @seealso \code{\link[quitte]{as.quitte}}, \code{\link[quitte]{is.quitte}}
#' @examples
#' iamProjectConfig()
#'
#' @importFrom utils read.csv
#' @importFrom dplyr tbl_df
#' @export

iamProjectConfig <- function(cfg="examplePROJECT") {

  #read config file
  if(!is.character(cfg) & !is.data.frame(cfg)) stop("Illegal cfg. Configuration must be provided as poject name, file path or data frame!")
  if(is.character(cfg)) {
    configs <- dir(system.file("extdata",package = "iamc"))
    names(configs) <- tolower(sub(".csv","",configs))
    if(tolower(cfg) %in% names(configs))  {
      cfg <- system.file("extdata",configs[tolower(cfg)],package = "iamc")
    } else if(!file.exists(cfg)) {
      stop("Given cfg setting is neither a project for which a configuration exists nor an existing configuration file!")
    }
    cfg <- read.csv(cfg, sep=";", stringsAsFactors = FALSE)
  }

  return(tbl_df(cfg))
}
