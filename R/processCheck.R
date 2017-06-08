#' processCheck
#'
#' Processes a check, which means that it returns a note if the check fails, returns a check summary based on the chosen
#' verbosity and returns the check results in a structured way. All checks should be run within processCheck.
#' Requirement for a check function is that it returns a list with 2 elements: "message" which contains the standard message
#' that should show up for the test and "failed" which is a vector of names for which the corresponding test failed.
#'
#' @param check function call as character that should be run
#' @param input named list with elements available for check functions
#' @return list containing check results
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{iamProjectConfig}}, \code{\link[quitte]{as.quitte}}, \code{\link[quitte]{is.quitte}}
#' @examples
#'
#' out <- processCheck(check = "filterVariables(x, cfg)",
#'                   input = list(x=example_magpie, cfg=iamProjectConfig()))
#' @importFrom methods is
#' @export

processCheck <- function(check, input) {
  if(is.null(input$verbose)) input$verbose <- TRUE
  for(i in names(input)) assign(i,input[[i]])
  funcname <- sub("\\(.*$","",check)
  r <- try(eval(parse(text = check)), silent = TRUE)
  if(is(r,"try-error")) {
    warning(funcname,": Test failed! ", r, call. = FALSE)
    return(NULL)
  }
  nfailed <- length(r$failed)
  message(funcname,": ",sub("%#",nfailed,r$message,fixed=TRUE))
  if(nfailed>0 & input$verbose) {
    for(elem in r$failed) message(paste("  ", elem))
  }
  out <- list()
  out[[funcname]] <- r
  return(out)
}
