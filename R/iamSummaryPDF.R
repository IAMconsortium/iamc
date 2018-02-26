#' iamSummaryPDF
#'
#' Creates a PDF summarizing check results and adding additional results for a
#' comparison to reference data (e.g. matching to historical data)
#'
#'
#' @param input named list with elements available for check functions
#' @param check_results list with check results as returned by \code{\link{iamCheck}}
#' @param file File name the summary should be written to or a Sweave object. If a sweave object is provided the function will return the updated object, otherwise it will write its content to the file.
#' @param maxLinesOutput maximum number of lines that should be output in the pdf
#' @param pdfStyle list of style-options for the pdf
#' @param ... additional arguments sent to \code{\link[lusweave]{swclose}}
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{iamCheck}}, \code{\link{iamProjectConfig}}
#' @examples
#' \dontrun{
#' input <- list(x=example_REMIND, ref=iamReferenceData())
#' check_results <- iamCheck(example_REMIND)
#'
#' iamSummaryPDF(check_results$input, check_results$out)
#' }
#' @importFrom lusweave swopen swlatex swclose swtable swR
#' @importFrom mip validationpdf
#' @export



iamSummaryPDF <- function(input, check_results=NULL, file="summary.pdf", maxLinesOutput= 200, pdfStyle=NULL, ...) {

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

  if(is.environment(file)) {
    sw <- file
  } else {
    sw <- swopen(outfile = file, template = template)
    swlatex(sw,c("\\title{IAMC data check}","\\author{Aperture Science Enrichment Center}","\\maketitle","\\tableofcontents"))
    on.exit(swclose(sw, ...))
  }


  if(length(check_results)>0) {
    swlatex(sw,"\\clearpage")
    swlatex(sw,"\\part{Data checks - Results}")
    swlatex(sw,"\\section{Summary}")

    summarytable <- function(cr) {
      lcr <- unlist(cr,recursive = FALSE)

      .tmp <- function(l, regex) {
        lout <- l[grepl(regex,names(l))]
        names(lout) <- sub(regex,"",names(lout))
        return(lout)
      }
      faill <- .tmp(lcr, "\\.failed$")
      descr <- .tmp(lcr, "\\.message$")

      descr <- gsub(" +"," ",sub("%#","",descr))
      nfail <- sapply(faill,length)

      return(data.frame(warnings=nfail, description=descr))
    }

    swtable(sw,summarytable(check_results), align=c("l","c","l"))

    swlatex(sw,"\\section{Detailed Results}")
    for(chk in names(check_results)) {
      nfailed <- length(check_results[[chk]]$failed)
      swlatex(sw,paste0("\\subsection{",chk," (",nfailed, " Warnings)}"))
      swlatex(sw,paste0(sub("%#",nfailed,check_results[[chk]]$message)))
      out <- check_results[[chk]]$failed
      if (nfailed > maxLinesOutput)
      {
        out <- check_results[[chk]]$failed[1:maxLinesOutput]
        swlatex(sw,paste0("\n output reduced to the first ", maxLinesOutput, " examples:\n"))
       # swlatex(sw,"\n output reduced to the first 15 examples: \n")
      }
      swR(sw,cat,paste(out,collapse="\n"))

    }
  }

  swlatex(sw,"\\clearpage")

  if(!is.null(input$ref) & !is.null(input$x)) validationpdf(x=input$x, hist=input$ref, file = sw, prefix = "Validation - ", hideEmptySection = TRUE, show_stats=FALSE, pdfStyle = pdfStyle)

}
