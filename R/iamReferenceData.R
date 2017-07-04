#' iamReferenceData
#'
#' Function to return historical reference data for validation.
#'
#' @param ref Reference data for comparison. Either a project name (currently available: "IAMC"), a path to a
#' file or a data frame containing the data.
#' @return Quitte object containing the reference data set
#' @author Jan Philipp Dietrich
#' @seealso \code{\link[quitte]{as.quitte}}, \code{\link[quitte]{is.quitte}}
#' @examples
#' iamReferenceData()
#'
#' @importFrom quitte as.quitte
#' @export

iamReferenceData <- function(ref="IAMC") {
  if(!is.character(ref) & !is.data.frame(ref)) stop("Illegal input. Reference setting must be provided as poject name, file path or data frame!")
  if(is.character(ref)) {
    if(length(ref)>1) {
      ref <- ref[1]
      warning("ref has length > 1 and only the first element will be used")
    }
    if(!grepl(".",ref,fixed=TRUE)) {
      if(!(tolower(ref) %in% names(valdata))) stop("Unknown refidation source ", ref,"!")
      ref <- valdata[[tolower(ref)]]
    } else if(!file.exists(ref)) {
      stop("Given reference setting is neither a project for which a data set exists nor an existing reference file!")
    }
  }
  return(as.quitte(ref))
}
