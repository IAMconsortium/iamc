#' filterInputData
#'
#' Filters given data (input) according to available default variables (cfg).
#' Pre-checks are performed about the consistency of the input data (e.g. illegal or missing variables)
#' and the status (out) returned.
#'
#' @param input Named list with elements available for check functions
#' @param cfg Project configuration that should be used. Either a project name (currently available: "CDLINKS"), a path to a
#' config file or a data frame specifying available variables and corresponding properties as returned by
#' \code{\link{iamProjectConfig}()}.
#' @param globalenv Boolean deciding whether functions in the global environment should be considered
#' @param out List with status from pre-checks, e.g. illegal or missing variables.
#' @return List with 1) filtered input and 2) status-output about the consistency of the input data
#' @author Cornelia Auer


filterInputData <- function(input, cfg="CDLINKS", globalenv = FALSE, out = NULL )
{

  #reduce config to variables which exist in x
  intersectVariables <- intersect(input$x$variable, input$cfg$variable)   #save?
  input$intersectVariables <- intersectVariables

  # check variable occurence/existence and output result
  preChecks <- collectFunctions("^preCheck", globalenv=globalenv, allowed_args=names(input))
  for(preCheck in preChecks) out <- c(out, processCheck(preCheck, input))

  #find and remove duplicates
  id <- paste(input$x$model, input$x$scenario, input$x$region, input$x$variable, input$x$period, sep=" | ")
  input$x <- input$x[!duplicated(id),]

  # reduce cfg to variables which exist in cfg
  input$cfg <- input$cfg[input$cfg$variable %in% intersectVariables,]
  # reduce x to variables which exist in cfg
  input$x <- input$x[input$x$variable %in% intersectVariables,]

  # convert x to magclass format as alternative source for checks and drop unit
  input$mx <- collapseNames(as.magpie(input$x), collapsedim = "unit")

  if(dim(input$x)[1]==0) {
    input$x <- NULL
    input$mx <- NULL
  }

  return (list("input" = input, "out" = out))

}
