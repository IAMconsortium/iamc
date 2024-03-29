% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.reportProject.R
\name{write.reportProject}
\alias{write.reportProject}
\title{Write file in specific project format}
\usage{
write.reportProject(
  mif,
  mapping,
  file = NULL,
  max_file_size = NULL,
  format = "default",
  append = FALSE,
  missing_log = NULL,
  ...
)
}
\arguments{
\item{mif}{Lists with magpie-objects or a magpie-object as created by read.report or a path to
a report.mif}

\item{mapping}{mapping of the variable names of the read-in mif. The header is used for
naming. The format of the mapping should be: 1st column the standard naming in PIK mif format.
X further columns that contain the indicator names in the reporting format. Can also contain
several indicator columns (e.g Variable and Item).
Optional columns with reserved names are unit, weight, factor, and spatial.
Factor is a number that the results will be multiplied with (e.g. to transform CO2 into C).
Weight is needed if several mif indicators shall be aggregated to one reporting indicator.
You always need a weight column if you have multiple mif to one reporting mappings. If you have
a weight column, you have to have values in it for all indicators. If NULL, the results are
added up; if you provide an indicator name (of a mif indicator), this indicator will be used for
the weighting of a weighted mean.
Spatial should be set to "glo" for mif indicators that shall only be reported globally and "reg" for only reporting locally.
The default is "reg+glo", which implies reporting on global and local level.
In the case of aggregation, contradicting entries in spatial column for the same reporting indicator will throw an error.
Unit is a name of the unit without ()
  Example:
"magpie";"agmip";"item";"unit";"weight";"factor"
"Nutrition|+|Calorie Supply (kcal/capita/day)";"CALO";"AGR";"kcal/capita/day";"NULL";1}

\item{file}{name of the output file, default=NULL returns the output object}

\item{max_file_size}{maximum file size in MB; if size of file exceeds max_file_size reporting is split into multiple files}

\item{format}{available reporting formats: "default", "IAMC" and "AgMIP". "default" and "IAMC" are very similar (wide format for year) and differ only in the use of semi-colon (default) and comma (IAMC) as seperator. "AgMIP" is in long format.}

\item{append}{Logical which decides whether data should be added to an existing file or an existing file should be overwritten}

\item{missing_log}{name of logfile to record variables which are present in the mapping but missing in the mif file. By default, no logfile is produced}

\item{...}{arguments passed to write.report}
}
\description{
Reads in a reporting.mif or uses a magpie object based on a read-in reporting.mif,
substitutes names of variables according to the mapping, multiplies reported
values by an optional factor in a column named "factor" of the mapping, and saves
the output in a new *.mif
}
\examples{

\dontrun{
write.reportProject("REMIND_generic_test.mif","Mapping_generic_ADVANCE.csv")
}

}
\seealso{
\code{\link{write.report}}, \code{\link{RenameAndAggregate}}
}
\author{
Christoph Bertram, Lavinia Baumstark, Anastasis Giannousakis, Florian Humpenoeder, Falk Benke, Benjamin Leon Bodirsky
}
