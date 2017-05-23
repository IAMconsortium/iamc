#' iamValidationData
#'
#' Function to return historical data for validation.
#'
#' @param val Validation data for comparison. Either a project name (currently available: "IAMC"), a path to a
#' file or a data frame containing the data.
#' @return quitte object containing the validation data set
#' @author Jan Philipp Dietrich
#' @seealso \code{\link[quitte]{as.quitte}}, \code{\link[quitte]{is.quitte}}
#' @examples
#' iamValidationData()
#'
#' @importFrom quitte as.quitte
#' @export

iamValidationData <- function(val="IAMC") {
  if(!is.character(val) & !is.data.frame(val)) stop("Illegal input. Validation setting must be provided as poject name, file path or data frame!")
  if(is.character(val)) {
    if(length(val)>1) {
      val <- val[1]
      warning("val has length > 1 and only the first element will be used")
    }
    if(!grepl(".",val,fixed=TRUE)) {
      if(!(tolower(val) %in% names(valdata))) stop("Unknown validation source ", val,"!")
      val <- valdata[[tolower(val)]]
    } else if(!file.exists(val)) {
      stop("Given val setting is neither a project for which a data set exists nor an existing configuration file!")
    }
  }
  return(as.quitte(val))
}
