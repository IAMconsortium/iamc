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
#' @return list of all inputs and outputs created by the performed checks (invisible)
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
  out <- NULL

  # -------------------------- create input data ------------------------------

  if(missing(x)) stop("x needs to be provided!")

  # LB/CA put as.quitte and error message together
  # test whether x could be converted to quitte object
  xQitte <- try(as.quitte(x))
  if(is(xQitte,"try-error")) stop("Incompatible data input format. Data could not be converted to quitte object!")

  #building input data object
  input <- list(x       = xQitte,                # data to be tested
                verbose = verbose,                # verbosity
                cfg     = iamProjectConfig(cfg),  # read project config
                val     = iamValidationData(val), # read validation data
                ... )                             # additional input data

  # convert x to magclass format as alternative source for checks and drop unit
  input$mx <- collapseNames(as.magpie(input$x), collapsedim = "unit")

  # ----------------------------------------------------------------------------

  # -------------------------- filter input data -------------------------------

  #reduce config to variables which exist in x
  intersectVariables <- intersect(input$x$variable, input$cfg$variable)   #save?
  input$intersectVariables <- intersectVariables

  # check variable occurence/existence and output result
  preChecks <- collectFunctions("^preCheck", globalenv=globalenv, allowed_args=names(input))
  for(preCheck in preChecks) out <- c(out, processCheck(preCheck, input))

  # reduce cfg to variables which exist in cfg
  input$cfg <- input$cfg[input$cfg$variable %in% intersectVariables,]
  # reduce x to variables which exist in cfg
  input$x <- input$x[input$x$variable %in% intersectVariables,]
  # reduce mx to variables which exist in cfg
  input$mx <- input$mx[,,intersectVariables]

  # ----------------------------------------------------------------------------

  # -------------------------- collect all available checks --------------------

  checks <- collectFunctions("^check", globalenv=globalenv, allowed_args=names(input))

  # ----------------------------------------------------------------------------

  # -------------------------- run collected checks ----------------------------

  for(check in checks) out <- c(out, processCheck(check, input))

  # ----------------------------------------------------------------------------

  # write output pdf
  if(!is.null(pdf)) iamSummaryPDF(input = input, check_results = out, file = pdf)

  # return check results
  invisible(list(out=out, input=input))
}
