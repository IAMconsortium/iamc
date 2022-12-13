#' @title RenameAndAggregate
#' @description Rename and aggregate data using a mapping
#'
#' Reads in a
#' substitutes names of variables according to the mapping, multiplies reported
#' values by an optional factor in a column named "factor" of the mapping, and saves
#' the output in a new *.mif
#'
#'
#' @param data Lists with list of magpie-objects (a magpie-object as created by read.report), first list containts scenarios, second list the models
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
#' @param missing_log name of logfile to record variables which are present in the mapping but missing in the mif file. By default, no logfile is produced
#' @author Christoph Bertram, Lavinia Baumstark, Anastasis Giannousakis, Florian Humpenoeder, Falk Benke, Benjamin Leon Bodirsky
#' @seealso \code{\link{write.report}}
#' @examples
#'
#' \dontrun{
#' RenameAndAggregate(list(model=list(scenario=population_magpie)),"Mapping_generic_ADVANCE.csv")
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom magclass dimSums ndim getCells getNames<- getSets getSets<- getYears
#' is.magpie mbind new.magpie read.report setNames getNames getRegions
#' @export


RenameAndAggregate <- function(data, mapping, missing_log=NULL) {

  map = .readMapping(mapping)
  # reorder mapping
  indicatorcols=which(!names(map) %in% c("factor","weight","unit", "spatial"))
  if(!1 %in% indicatorcols) {stop("first column has to be reporting mif output indicator name")}
  # merge multiple indicator columns
  map2 <- data.frame(map[,1],apply(map[,setdiff(indicatorcols,1),drop=FALSE],MARGIN=1,paste,collapse="."),map[,-indicatorcols],stringsAsFactors = FALSE)
  map2<-map2[apply(map[,setdiff(indicatorcols,1),drop=FALSE],MARGIN=1,paste,collapse="")!="",]
  names(map2)<-c(names(map)[1],paste(names(map[,setdiff(indicatorcols,1),drop=FALSE]),collapse="."),names(map)[-indicatorcols])
  map<-map2
  remove(map2)

  #check for duplicate rows
  if (length(rownames(map[duplicated(map),])) > 0) {
    warning(paste("Duplicate rows found in mapping in line(s)",rownames(map[duplicated(map),])))
    map <- map[!duplicated(map),]
    warning("Duplicate rows removed from mapping, otherwise unwrap would stop with an error.")
  }

  missingc <- c()
  # select variables and change names of reported variables
  new_data <- list()

  # without aggregation
  if(!"weight"%in%names(map)) {
    if(length(unique(map[,2]))>length(map[,2])){stop("There exist multiple mif entries for each indicator, but no weight for aggregation has been provided")}

    for (n in names(data)){   # n: scenarios
      for (m in names(data[[n]])){  # m: models
        ind <- which(map[,names(map)[1]]  %in% intersect(map[,names(map)[1]],getNames(data[[n]][[m]])))

        tmp <- setNames(mbind(
          lapply(
            map[ind, names(map)[1]],
            function(x) {
              r <- as.numeric(map[which(map[,names(map)[1],drop=F] == x), "factor"]) * data[[n]][[m]][,,x]
              spatial_x <- unique(map[which(map[,names(map)[1],drop=F] == x), "spatial"])
              GLO <- intersect(c("GLO","WLD","World","WORLD"),getRegions(r))[1]
              if (spatial_x == "reg") {
                r[GLO,,] <- NA
              } else if (spatial_x == "glo") {
                r[getRegions(r[GLO,,invert=T]),,] <- NA
              }
              return(r)
            }
          )
        ), map[ind, indicatorcols[-1]]
        )

        # correct sets for multicolumn objects
        getSets(tmp)<-c(getSets(tmp)[1:3],strsplit(names(map)[2],"[.]")[[1]][-1])
        new_data[[n]][[m]] <- tmp

        #Select names from mapping which are not NA
        map_names = map[stats::complete.cases(map[names(map)[1]]),]
        map_names = getElement(map_names,names(map)[1])
        #Which names in the mapping are missing in the data
        tmp <- setdiff(map_names,getNames(data[[n]][[m]]))
        if (length(tmp) !=0) {
          missingc <- c(missingc,tmp)
        }
      }
    }
  } else {
    # with aggregation

    for (n in names(data)){   # n: scenarios
      for (m in names(data[[n]])){  # m: models
        if (ndim(data[[n]][[m]], dim = 3) > 1) {stop("data has more than 1 data dimensions")}
        tmp<-new.magpie(cells_and_regions = getCells(data[[n]][[m]]), years = getYears(data[[n]][[m]]), names = unique(map[,2]))

        for (ind_x in unique(map[,2])){
          mapindex=which(map[,2]==ind_x)
          weight_x=map$weight[mapindex]
          factor_x=setNames(as.magpie(as.numeric(map$factor[mapindex])),map[mapindex,1])
          original_x=map[mapindex,1]
          spatial_x=unique(map$spatial[mapindex])
          GLO <- intersect(c("GLO","WLD","World","WORLD"),getRegions(data[[n]][[m]]))[1]
          if(spatial_x == "reg"){
            regions<-getRegions(data[[n]][[m]][GLO,,invert=T])
          }else if(spatial_x == "glo"){
            regions<-c(GLO)
          }else{
            regions<-getRegions(data[[n]][[m]])
          }

          if(all(original_x %in% getNames(data[[n]][[m]]))) {
            if (any(weight_x=="")){
              #wenn Gewicht "" dann error
              stop(paste0("empty weight for indicator "),ind_x)
            } else if (all(weight_x != "NULL")){
              #wenn Gewicht vorhanden dann average
              # average: by(data = b,INDICES = b[,2],FUN = function(x){sum(x$breaks*x$test)/sum(x$test)})
              tmp[regions,,ind_x]<-  dimSums(data[[n]][[m]][regions,,original_x]*factor_x*setNames(data[[n]][[m]][regions,,weight_x],original_x),dim=3.1)/dimSums(setNames(data[[n]][[m]][regions,,weight_x],original_x),dim=3.1)
            } else if (all(weight_x=="NULL")){
              #wenn gewicht NULL dann summation
              tmp[regions,,ind_x] <- dimSums(data[[n]][[m]][regions,,original_x]*factor_x,dim=3.1)
            } else {
              stop(paste0("mixture of weights between NULL and parameters for indicator "),ind_x)
              #wenn Gewicht mischung aus NULL und "" dann error
            }
          } else if (is.null(missing_log)){
            warning(paste0("Indicator ", original_x[which(!original_x%in%getNames(data[[n]][[m]]))]," missing in data but exists in mapping"))
          }
        }

        getSets(tmp)<-c(getSets(tmp)[1:3],strsplit(names(map)[2],"[.]")[[1]][-1])
        if("unit"%in%names(map)) {
          getNames(tmp)<-paste0(getNames(tmp)," (",map$unit[match(getNames(tmp),map[,2])],")")
        }
        new_data[[n]][[m]] <- tmp

        tmp<-setdiff(map[,names(map)[1]],getNames(data[[n]][[m]]))
        if (length(tmp) !=0) {
          missingc <- c(missingc,tmp)
        }
      }
    }
  }



  if (length(missingc) !=0){
    if (is.null(missing_log)){
      warning(
        paste0(
          "Following variables were not found in the generic data and were excluded: \"",
          paste(sort(unique(missingc)), collapse = "\", \""),"\""))
    }else{
      if (!is.null(missing_log)){
        write(c("#--- Variables missing in the mif file but present in the mapping ---#",
                sort(unique(missingc)), "\n"),
              missing_log, append = TRUE)
      }
    }
  }

  return(new_data)
}

.readMapping<-function(mapping){
  # read in mapping of the names of variables for the project, handle NAs
  map <- read.csv2(mapping,colClasses="character")
  map <- sapply(X = map,FUN = function(x) gsub("N/A","NA",x,fixed = T,useBytes = TRUE))
  map <- as.data.frame(map,stringsAsFactors = FALSE)

  # if non existent, add factor column
  if(!"factor" %in% names(map)) {
    map$factor<-1
  }
  # set missing values in factor column to 1
  map$factor[which(map$factor=="")]<-1

  # if non existent, add spatial column
  if(!"spatial" %in% names(map)) {
    map$spatial<-"reg+glo"
  }
  # set missing values in spatial column to "reg+glo"
  map$spatial[which(map$spatial=="")]<-"reg+glo"

  if(length(setdiff(unique(map$spatial), c("reg", "glo", "reg+glo")))>0){
    stop("error in mapping. spatial values must be eiter \"reg\", \"glo\", or \"reg+glo\"")
  }

  return(map)
}


