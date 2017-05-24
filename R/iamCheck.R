#' iamCheck
#'
#' Runs various diagnostics over a provided data set checking whether
#' the provided data is in line with IAMC database guidelines.
#'
#'
#' @param x Input data that should be checked, provided as a file path to a reporting file,
#' a quitte object or an object which can be converted to quitte using \code{\link[quitte]{as.quitte}}
#' @param pdf File name used for a PDF containing diagnostic results of the check. If set to NULL
#' no pdf will be written.
#' @param cfg Configuration that should be used. Either a project name (currently available: "IAMC"), a path to a
#' config file or a data frame specifying available variables and corresponding properties as returned by
#' \code{\link{iamVariables}()}.
#' @param val Validation data for comparison. Either a project name (currently available: "IAMC"), a path to a mif
#' file or a quitte object containing the data.
#' @param verbose Boolean influencing the degree of information returned by the function. \code{verbose=TRUE} returns
#' detailed information whereas \code{verbose=FALSE} returns a summary.
#' @return Number of notes returned by iamCheck
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{iamVariables}}, \code{\link[quitte]{as.quitte}}, \code{\link[quitte]{is.quitte}}
#' @examples
#'
#' # run check with example data
#' iamCheck(example_magpie)
#'
#' @importFrom quitte as.quitte is.quitte
#' @importFrom magclass as.magpie collapseNames
#' @importFrom mip validationpdf
#' @importFrom methods is
#' @export

iamCheck <- function(x, pdf=NULL, cfg="IAMC", val="IAMC", verbose=TRUE) {

  if(missing(x)) stop("x needs to be provided!")

  #read config file
  cfg <- iamVariables(cfg)

  #get validation data
  val <- iamValidationData(val)

  #try to convert input to quitte
  x <- try(as.quitte(x))
  if(is(x,"try-error")) stop("Incompatible data input format. Data could not be converted to quitte object!")

  #reduce config to variables which exist in x
  variables <- intersect(x$variable, cfg$variable)
  cfg <- cfg[cfg$variable %in% variables,]

  runCheck <- function(verbose=TRUE, func, ...) {
    funcname <- as.character(as.list(match.call())$func)
    r <- try(func(...), silent = TRUE)
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

  # check variable names
  out  <- runCheck(verbose, checkVariable, x=x, cfg=cfg)

  # filter x based on variable check (as only settings for allowed variable names are available)
  x  <- as.quitte(droplevels(x[!(x$variable %in% out$checkVariable$failed),]))

  # convert x to magclass format as alternative source for checks
  mx <- collapseNames(as.magpie(x), collapsedim = 4)

  runCheck(verbose, checkBounds, mx=mx, cfg=cfg, type="min")
  runCheck(verbose, checkBounds, mx=mx, cfg=cfg, type="max")

 if(!is.null(pdf)) {
   validationpdf(x=x,hist=iamValidationData(val=val),file = pdf)
 }
}
