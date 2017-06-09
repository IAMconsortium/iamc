#' iamSummaryPDF
#'
#' Creates a PDF summarizing check results and adding
#' additional validation output
#'
#'
#' @param input named list with elements available for check functions
#' @param check_results list with check results as returned by \code{\link{iamCheck}}
#' @param file File name the summary should be written to.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{iamCheck}}, \code{\link{iamProjectConfig}}
#' @examples
#' \dontrun{
#' input <- list(x=example_landcover, val=iamValidationData())
#' check_results <- iamCheck(example_landcover)
#'
#' iamSummaryPDF(input, check_results)
#' }
#' @importFrom lusweave swopen swlatex swclose
#' @importFrom mip validationpdf
#' @export



iamSummaryPDF <- function(input, check_results=NULL, file="summary.pdf") {
  if(is.null(input$x)) stop("input list must at least contain x!")

  template <-  c("\\documentclass[a4paper, portrait ]{article}",
                 "\\setlength{\\parindent}{0in}",
                 "\\usepackage{float}",
                 "\\usepackage[bookmarksopenlevel=section]{hyperref}",
                 "\\hypersetup{bookmarks=true,pdfauthor={IAMC data analysis package}}",
                 "\\usepackage{graphicx}",
                 "\\usepackage{rotating}",
                 "\\usepackage[strings]{underscore}",
                 "\\usepackage[margin=2cm]{geometry}",
                 "\\usepackage{fancyhdr}",
                 "\\pagestyle{fancy}",
                 "\\begin{document}",
                 "<<echo=false>>=",
                 "options(width=90)",
                 "@")

  sw <- swopen(outfile = file, template = template)
  swlatex(sw,c("\\title{IAMC data check}","\\author{Aperture Science Enrichment Center}","\\maketitle","\\tableofcontents"))
  on.exit(swclose(sw, engine="knitr"))

  if(length(check_results)>0) {
    swlatex(sw,"\\clearpage")
    swlatex(sw,"\\section{Data checks}")

    for(chk in names(check_results)) {
      nfailed <- length(check_results[[chk]]$failed)
      swlatex(sw,paste0("\\subsection{",chk,": ",sub("%#",nfailed,check_results[[chk]]$message,fixed=TRUE),"}"))
      swlatex(sw,paste0("  ",check_results[[chk]]$failed, collapse="\\n"))
    }
  }

  swlatex(sw,"\\clearpage")

  if(!is.null(input$val)) validationpdf(x=input$x, hist=input$val, file = sw, prefix = "Validation - ")

}
