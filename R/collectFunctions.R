#' collectFunctions
#'
#' Collects functions which follow a given name pattern
#' and compares their arguments against a list of allowed arguments
#'
#'
#' @param pattern Name pattern the function name should match. Default is to collect functions starting with "check"
#' @param globalenv Boolean deciding whether functions in the global environment should be considered
#' or not.
#' @param allowed_args Vector of allowed arguments. If a function contains an argument not listed here it will be
#' ignored and a warning will be returned
#' @return a character vector of function calls fulfilling all requirements
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{iamCheck}}
#' @examples
#'
#' collectFunctions()
#'
#' @importFrom methods formalArgs
#' @export

collectFunctions <- function(pattern="^check", globalenv=FALSE, allowed_args=c("x","mx","cfg")) {

  funcs <- paste0("iamc:::",ls(getNamespace("iamc"), pattern=pattern))
  if(globalenv) {
    funcs <- c(funcs, ls(globalenv(),pattern=pattern))
    funcnames <- sub("^.*:::","",funcs)
    if(anyDuplicated(funcnames)) {
      warning("Function name(s) in global environment match name(s) in iamc package. Function(s) \"",paste(funcnames[duplicated(funcnames)],collapse="\", \""),"\" in global environment will be ignored. Please use different function name(s)!")
      funcs <- funcs[!duplicated(funcnames)]
    }
  }

  checks <- NULL

  for(func in funcs) {
    args <- formalArgs(eval(parse(text = func)))
    if(all(args %in% allowed_args)) {
      checks <- c(checks, paste0(func,"(",paste(args,collapse=", "),")"))
    } else {
      invalid_args <- args[!(args %in% allowed_args)]
      warning("Function \"",func,"\" ignored because of invalid arguments (",paste(invalid_args, collapse=", "),")")
    }
  }

  #remove package prefix
  checks <- sub("^.*:::","",checks)

  return(checks)
}
