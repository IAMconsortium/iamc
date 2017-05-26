#' processCheck
#'
#' Processes a check, which means that it returns a note if the check fails, returns a check summary based on the chosen
#' verbosity and returns the check results in a structured way. All checks should be run within processCheck.
#' Requirement for a check function is that it returns a list with 2 elements: "message" which contains the standard message
#' that should show up for the test and "failed" which is a vector of names for which the corresponding test failed.
#'
#' @param check function call that should be run
#' @param env Environment containing all relevant settings but also outputs of previous checks.
#' Outputs of the given check function will be added to the environment
#' @return environment containing all settings and outputs
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{iamVariables}}, \code{\link[quitte]{as.quitte}}, \code{\link[quitte]{is.quitte}}
#' @examples
#'
#' e <- processCheck(iamc:::checkVariable(example_magpie, cfg=iamVariables()), env=new.env())
#' e$out
#' rm(out, envir=e)
#' @importFrom methods is
#' @export

processCheck <- function(check, env) {
  if(is.null(env$verbose)) env$verbose <- TRUE
  funcname <- as.character(as.list(match.call())$check)[1]
  r <- try(check, silent = TRUE)
  if(is(r,"try-error")) {
    warning(funcname,": Test failed! ", r, call. = FALSE)
    return(NULL)
  }
  nfailed <- length(r$failed)
  message(funcname,": ",sub("%#",nfailed,r$message,fixed=TRUE))
  if(nfailed>0 & env$verbose) {
    for(elem in r$failed) message(paste("  ", elem))
  }
  if(is.null(env$out)) env$out <- list()
  env$out[[funcname]] <- r

  return(env)
}
