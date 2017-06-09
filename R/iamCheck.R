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
#' @param val Validation data for comparison. Either a project name (currently available: "IAMC"), a path to a mif
#' file or a quitte object containing the data.
#' @param verbose Boolean influencing the degree of information returned by the function. \code{verbose=TRUE} returns
#' detailed information whereas \code{verbose=FALSE} returns a summary.
#' @param globalenv Boolean deciding whether functions in the global environment should be considered
#' or not.
#' @param ... additional data objects which are forwarded to the check functions
#' @return list of all outputs created by the performed checks (invisible)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{iamProjectConfig}}, \code{\link[quitte]{as.quitte}}, \code{\link[quitte]{is.quitte}}
#' @examples
#'
#' # run check with example data
#' iamCheck(example_landcover)
#'
#' @importFrom quitte as.quitte is.quitte
#' @importFrom magclass as.magpie collapseNames
#' @export

iamCheck <- function(x, pdf=NULL, cfg="CDLINKS", val="IAMC", verbose=TRUE, globalenv=FALSE, ...) {

  if(missing(x)) stop("x needs to be provided!")

  #building input data object
  input <- list(x       = try(as.quitte(x)),      # data to be tested
                verbose = verbose,                # verbosity
                cfg     = iamProjectConfig(cfg),  # read project config
                val     = iamValidationData(val), # read validation data
                ... )                             # additional input data

  # test whether x could be converted to quitte object
  if(is(input$x,"try-error")) stop("Incompatible data input format. Data could not be converted to quitte object!")

  #reduce config to variables which exist in x
  variables <- intersect(input$x$variable, input$cfg$variable)
  input$cfg <- input$cfg[input$cfg$variable %in% variables,]

  # check variable names
  out <- processCheck("filterVariables(x, cfg)", input)

  # reduce x to variables which exist in config
  input$x <- as.quitte(droplevels(input$x[input$x$variable %in% variables,]))

  # convert x to magclass format as alternative source for checks and drop unit
  input$mx <- collapseNames(as.magpie(input$x), collapsedim = "unit")

  # collect all available checks
  checks <- collectFunctions("^check", globalenv=globalenv, allowed_args=names(input))

  # run collected checks
  for(check in checks) out <- c(out, processCheck(check, input))

  # write output pdf
  if(!is.null(pdf)) iamSummaryPDF(input = input, check_results = out, file = pdf)

  # return check results
  invisible(out)
}
