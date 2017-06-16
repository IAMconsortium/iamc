#' filterInputData
#'
#' Filters given data (input) according to available default variables and
#' returns status (out) about illegal or missing variables
#'
#' @param input named list with elements available for check functions
#' @param cfg Project configuration that should be used. Either a project name (currently available: "CDLINKS"), a path to a
#' config file or a data frame specifying available variables and corresponding properties as returned by
#' \code{\link{iamProjectConfig}()}.
#' @param globalenv Boolean deciding whether functions in the global environment should be considered
#' @param out list to be filled for status-output
#' @return list with 1) filtered input and 2) status-output about illegal or missing variables
#' @author Cornelia Auer


filterInputData <- function(input, cfg="CDLINKS", globalenv = FALSE, out = NULL )
{

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

  return (list("input" = input, "out" = out))

}
