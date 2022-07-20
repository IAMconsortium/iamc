#' Write file in specific project format
#'
#' Reads in a reporting.mif or uses a magpie object based on a read-in reporting.mif,
#' substitutes names of variables according to the mapping, multiplies reported
#' values by an optional factor in a column named "factor" of the mapping, and saves
#' the output in a new *.mif
#'
#'
#' @param mif Lists with magpie-objects or a magpie-object as created by read.report or a path to
#' a report.mif
#' @param mapping mapping of the variable names of the read-in mif. The header is used for
#' naming. The format of the mapping should be: 1st column the standard naming in PIK mif format.
#' X further columns that contain the indicator names in the reporting format. Can also contain
#' several indicator columns (e.g Variable and Item).
#' Optional columns with reserved names are unit, weight, factor, and spatial.
#' Factor is a number that the results will be multiplied with (e.g. to transform CO2 into C).
#' Weight is needed if several mif indicators shall be aggregated to one reporting indicator.
#' You always need a weight column if you have multiple mif to one reporting mappings. If you have
#' a weight column, you have to have values in it for all indicators. If NULL, the results are
#' added up; if you provide an indicator name (of a mif indicator), this indicator will be used for
#' the weighting of a weighted mean.
#' Spatial should be set to "glo" for mif indicators that shall only be reported globally and "reg" for only reporting locally.
#' The default is "reg+glo", which implies reporting on global and local level.
#' In the case of aggregation, contradicting entries in spatial column for the same reporting indicator will throw an error.
#' Unit is a name of the unit without ()
#'   Example:
#' "magpie";"agmip";"item";"unit";"weight";"factor"
#' "Nutrition|+|Calorie Supply (kcal/capita/day)";"CALO";"AGR";"kcal/capita/day";"NULL";1
#'
#' @param file name of the output file, default=NULL returns the output object
#' @param max_file_size maximum file size in MB; if size of file exceeds max_file_size reporting is split into multiple files
#' @param format available reporting formats: "default", "IAMC" and "AgMIP". "default" and "IAMC" are very similar (wide format for year) and differ only in the use of semi-colon (default) and comma (IAMC) as seperator. "AgMIP" is in long format.
#' @param append Logical which decides whether data should be added to an existing file or an existing file should be overwritten
#' @param missing_log name of logfile to record variables which are present in the mapping but missing in the mif file. By default, no logfile is produced
#' @param ... arguments passed to write.report
#' @author Christoph Bertram, Lavinia Baumstark, Anastasis Giannousakis, Florian Humpenoeder, Falk Benke, Benjamin Leon Bodirsky
#' @seealso \code{\link{write.report}}, \code{\link{RenameAndAggregate}}
#' @examples
#'
#' \dontrun{
#' write.reportProject("REMIND_generic_test.mif","Mapping_generic_ADVANCE.csv")
#' }
#'
#' @importFrom utils read.csv2 write.csv write.table
#' @importFrom magclass dimSums fulldim getCells getNames<- getSets getSets<- getYears
#' is.magpie mbind new.magpie read.report setNames write.report getNames getRegions
#' @importFrom reshape2 melt
#' @importFrom readxl read_excel
#' @importFrom writexl write_xlsx
#' @export

write.reportProject <- function(mif, mapping,
                                file=NULL, max_file_size=NULL,
                                format="default", append=FALSE, missing_log=NULL, ...){
  if(is.character(mif)){
    data <- read.report(mif,as.list=TRUE)
  } else if (is.list(mif)){
    data <- mif
  } else if (is.magpie(mif)){
    scenario <- getNames(mif,dim=1)
    model    <- getNames(mif,dim=2)
    data <- list()
    for (s in scenario){
      for (m in model) {
        data[[s]][[m]] <- collapseNames(mif[,,s][,,m])
      }
    }
   } else {
    stop("please provide either a path to a mif-file, a read in mif-file (in list-structure or as a magpie object)")
   }

  ### update logfile with current dataset name
  if(is.vector(mif) & !is.null(missing_log)){
    write(c(sprintf("#--- Writing report for %s ---#", mif), "\n"), missing_log, append=TRUE)
  }

  ### Here the actual magic takes place
  new_data = RenameAndAggregate(data=data,mapping=mapping,missing_log=missing_log)

  ### write the outputs
  if(!is.null(file)){
    # save project reporting
    if(!file.exists(file)) append <- FALSE
    if(format == "default") {
      if (grepl("(xls$|xlsx$)",file)){

        if (grepl("~",file)){
          stop("the sign '~' is not always supported by function write.xlsx. Please change file path")
        }
        a <- write.report(new_data,file=NULL,...)
        a <- do.call(rbind,do.call(rbind,a))
        if(append) {
          existing.data <- read_excel(file, sheet = "DATA")
          new.data <- rbind(existing.data, as.data.frame(a))
          write_xlsx(list(DATA = new.data), path = file)
        } else {
          write_xlsx(list(DATA = as.data.frame(a)), path = file)
        }

      } else
        write.report(new_data,file=file,append=append,...)
    } else if (format == "IAMC") {
      a <- write.report(new_data,file=NULL,...)
      a <- do.call(rbind,do.call(rbind,a))
      write.table(a,file,quote=FALSE,sep=",",row.names=FALSE,col.names=!append,append=append,eol="\n")
    } else if (format == "AgMIP") {
      a <- write.report(new_data,file=NULL,extracols = "Item",...)
      a <- do.call(rbind,do.call(rbind,a))
      b<-melt(a,id.vars = c("Model","Scenario","Region","Item","Variable","Unit"),variable.name = "Year",value.name="Value")
      b<-b[c("Model","Scenario","Region","Item","Variable","Year","Unit","Value")]
      write.table(b,file,quote=FALSE,sep=",",row.names=FALSE,col.names=!append,append=append,eol="\n")
    }

    if (!is.null(max_file_size)) {
      file_size <- file.size(file)/10^6 #file size in MB
      if(file_size > max_file_size) {
        x <- read.report(file,as.list=FALSE)
        scen <- getNames(x,dim=1)
        n_scen <- length(scen)
        n_files <- ceiling(file_size / max_file_size)
        if (n_files > n_scen) {
          n_files <- n_scen
          warning("Minimum is one scenario per file!")
        }
        scen_per_file <- floor(length(scen)/n_files)
        first_scen <- 1
        for (f in 1:n_files) {
          print(paste0("File ",f))
          #prepare scenario subset
          last_scen <- (first_scen+scen_per_file-1)
          if (last_scen > n_scen) last_scen <- n_scen
          scen_subset <- scen[first_scen:last_scen]
          print(scen_subset)
          #subset data
          tmp <- x[,,scen_subset]
          #prepare file name
          file_name <- unlist(strsplit(file,"\\."))
          last <- length(file_name)
          #write report
          if(format == "default") {
            write.report(tmp,file=paste0(file_name[1:last-1],"_part",f,".",file_name[last]),...)
          } else if (format == "IAMC") {
            a <- write.report(tmp,file=NULL,...)
            a <- do.call(rbind,do.call(rbind,a))
            write.csv(a,file=file,row.names = FALSE,quote = FALSE)
          } else if (format == "AgMIP") {
            a <- write.report(tmp,file=NULL,extracols = "Item",...)
            a <- do.call(rbind,do.call(rbind,a))
            b<-melt(a,id.vars = c("Model","Scenario","Region","Item","Variable","Unit"),variable.name = "Year",value.name="Value")
            b<-b[c("Model","Scenario","Region","Item","Variable","Year","Unit","Value")]
            write.csv(b,file=paste0(file_name[1:last-1],"_part",f,".",file_name[last]),row.names = FALSE,quote = FALSE)
          }
          #set counter for next loop
          first_scen <- first_scen+scen_per_file
        }
      }
    }
  } else {
    return(new_data)
  }
}

