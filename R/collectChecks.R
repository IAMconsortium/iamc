

collectChecks <- function(id="^check", globalenv=FALSE) {

  funcs <- paste0("iamc:::",ls(getNamespace("iamc"), pattern=id))
  if(globalenv) {
    funcs <- c(funcs, ls(globalenv(),pattern=id))
  }

  checks <- NULL

  for(func in funcs) {
    args <- formalArgs(eval(parse(text = func)))
    checks <- c(checks, paste0(func,"(",paste(args,collapse=", "),")"))
  }
  return(checks)
}
