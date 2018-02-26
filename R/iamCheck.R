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
#' @param cfg Project configuration that should be used. Either a project name (currently available: "CDLINKS"), a path to a
#' config file or a data frame specifying available variables and corresponding properties as returned by
#' \code{\link{iamProjectConfig}()}.
#' @param refData Reference data for comparison. Either a project name (currently available: "IAMC"), a path to a mif
#' file or a quitte object containing the data.
#' @param verbose Boolean influencing the degree of information returned by the function. \code{verbose=TRUE} returns
#' detailed information whereas \code{verbose=FALSE} returns a summary.
#' @param globalenv Boolean deciding whether functions in the global environment should be considered
#' or not.
#' @param pdfStyle list of style-options for the pdf
#' @param ... additional data objects which are forwarded to the check functions
#' @return List of all inputs and outputs created by the performed checks (invisible)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{iamProjectConfig}}, \code{\link[quitte]{as.quitte}}, \code{\link[quitte]{is.quitte}}
#' @examples
#'
#' # run check with example data
#' iamCheck(example_REMIND, cfg="CDLINKS")
#'
#' @export

iamCheck <- function(x, pdf=NULL, cfg="CDLINKS", refData="IAMC", verbose=FALSE, globalenv=FALSE, pdfStyle=NULL, ...) {


  if(missing(x)) stop("x needs to be provided!")

  # init ------------------------------------------------
  input <- NULL
  out   <- NULL

  # create input data -----------------------------------
  input <- createInputData(x, cfg, refData, verbose, ...)

  # filter input data -----------------------------------
  resultFiltering <- filterInputData(input, cfg, globalenv, out)
  input           <- resultFiltering$input
  out             <- resultFiltering$out

  # collect and run available checks --------------------
  if(!is.null(input$x)) {
    checks <- collectFunctions("^check", globalenv=globalenv, allowed_args=names(input))
    for(check in checks) out <- c(out, processCheck(check, input))
  } else {
    message("There is no intersect between variables in data and config. Further checks are skipped!")
  }

  # write output pdf ------------------------------------
  if(!is.null(pdf)) iamSummaryPDF(input = input, check_results = out, file = pdf, pdfStyle = pdfStyle)

  # return check results --------------------------------
  invisible(list(out=out, input=input))
}
