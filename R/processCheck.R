#' processCheck
#'
#' Processes a check, which means that it returns a note if the check fails, returns a check summary based on the chosen
#' verbosity and returns the check results in a structured way. All checks should be run within processCheck.
#' Requirement for a check function is that it returns a list with 2 elements: "message" which contains the standard message
#' that should show up for the test and "failed" which is a vector of names for which the corresponding test failed.
#'
#' @check function call that should be run
#' @param verbose Boolean influencing the degree of information returned by the function. \code{verbose=TRUE} returns
#' detailed information whereas \code{verbose=FALSE} returns a summary.
#' @return summary of check results
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{iamVariables}}, \code{\link[quitte]{as.quitte}}, \code{\link[quitte]{is.quitte}}
#' @examples
#'
#' processCheck(iamc:::checkVariable(example_magpie, cfg=iamVariables()))
#'
#' @importFrom methods is
#' @export

processCheck <- function(check, verbose=TRUE) {
  funcname <- as.character(as.list(match.call())$check)[1]
  r <- try(check, silent = TRUE)
  if(is(r,"try-error")) {
    warning(funcname,": Test failed! ", r, call. = FALSE)
    return(NULL)
  }
  nfailed <- length(r$failed)
  message(funcname,": ",sub("%#",nfailed,r$message,fixed=TRUE))
  if(nfailed>0 & verbose) {
    for(elem in r$failed) message(paste("  ", elem))
  }
  out <- list()
  out[[funcname]] <- r
  return(out)
}
