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
#' @param globalenv Boolean deciding whether functions in the global environment should be considered
#' or not.
#' @return list of all outputs created by the performed checks
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
#' @export

iamCheck <- function(x, pdf=NULL, cfg="IAMC", val="IAMC", verbose=TRUE, globalenv=FALSE) {

  if(missing(x)) stop("x needs to be provided!")

  #read config file
  cfg <- iamVariables(cfg)

  #get validation data
  val <- iamValidationData(val)

  #try to convert input to quitte
  x <- try(as.quitte(x))
  if(is(x,"try-error")) stop("Incompatible data input format. Data could not be converted to quitte object!")

  #create environment to store settings and outputs
  e <- new.env()
  e$verbose <- verbose
  on.exit(rm(list=ls(envir = e), envir=e))

  #reduce config to variables which exist in x
  variables <- intersect(x$variable, cfg$variable)
  cfg <- cfg[cfg$variable %in% variables,]

  input <- list(x=x, cfg=cfg, val=val)

  # check variable names
  processCheck("filterVariables(x, cfg)", e, input)

  # filter x based on variable check (as only settings for allowed variable names are available)
  input$x <- as.quitte(input$x[input$x$variable %in% variables,])

  # convert x to magclass format as alternative source for checks
  input$mx <- collapseNames(as.magpie(x), collapsedim = 4)

  checks <- collectFunctions("^check", globalenv=globalenv, allowed_args=names(input))
  for(check in checks) {
    processCheck(check, e, input)
  }

  if(!is.null(pdf)) {
    validationpdf(x=x,hist=val,file = pdf)
  }
  return(e$out)
}
