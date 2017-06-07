#' processCheck
#'
#' Processes a check, which means that it returns a note if the check fails, returns a check summary based on the chosen
#' verbosity and returns the check results in a structured way. All checks should be run within processCheck.
#' Requirement for a check function is that it returns a list with 2 elements: "message" which contains the standard message
#' that should show up for the test and "failed" which is a vector of names for which the corresponding test failed.
#'
#' @param check function call as character that should be run
#' @param env Environment containing all relevant settings but also outputs of previous checks.
#' Outputs of the given check function will be added to the environment
#' @param input named list with elements available for check functions
#' @return environment containing all settings and outputs
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{iamVariables}}, \code{\link[quitte]{as.quitte}}, \code{\link[quitte]{is.quitte}}
#' @examples
#'
#' e <- processCheck(check = "filterVariables(x, cfg)",
#'                   env = new.env(),
#'                   input = list(x=example_magpie, cfg=iamVariables()))
#' e$out
#' rm(out, envir=e)
#' @importFrom methods is
#' @export

processCheck <- function(check, env, input) {
  for(i in names(input)) assign(i,input[[i]])
  if(is.null(env$verbose)) env$verbose <- TRUE
  funcname <- sub("\\(.*$","",check)
  r <- try(eval(parse(text = check)), silent = TRUE)
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
