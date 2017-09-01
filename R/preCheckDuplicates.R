#' getShiftFactor
#'
#' Method: Essentially computes order by the power of ten from a given positive number
#' The order value is then used to compute a shift factor, which is needed to uniquely represent the data values as integer in preCheckDuplicates.
#'
#' @param positiveNumber number >= 0
#' @return order of power of ten for the input value
#'
getShiftFactor<-function(positiveNumber)
{

  if ( positiveNumber < 0 )
    stop("Only positive numbers allowed")

  curPowerOfTen <-  1
  maxPowerOfTen <- 100
  while ( curPowerOfTen  < maxPowerOfTen )
  {
    if ( positiveNumber < 10^curPowerOfTen )
    {
      return (curPowerOfTen)
    }
    curPowerOfTen <- curPowerOfTen + 1
  }

  stop("function 'powerOfTen' terminated in an unexpected way")

}


#' codedAsInteger
#'
#' Encodes data as integer values(corresponding to their factor representatives).
#' NA values are exchanged by 0.
#'
#' @param input Input data that should be checked, provided as a object related to a data frame.
#' @return values encoded as integers without NAs.
codedAsInteger<-function(input)
{

  input <- as.integer(input)
  input[is.na(input)] <- 0
  return (input)

}


#' preCheckDuplicates
#'
#' Checks for duplicate entries in the data.
#' To optimize performance the single data values are encoded into integer values (corresponding to their factor representatives).
#' The encoded integer values are then compared if duplicates exist.
#'
#' @param x Input data that should be checked, provided as a quitte object \code{\link[quitte]{as.quitte}}
#' @return Two-dimensional list. 1) message with how many dpulicates 2) list of duplicates (in their integer representation)
#' @author Cornelia Auer

preCheckDuplicates <- function(x)
{

  if (!is.quitte(x))
    stop("Data not provided in the right format: quitte format required.")

  id <- 0
  exponent <- 0

  id <- codedAsInteger(x$model)
  exponent <- getShiftFactor(nlevels(factor(x$model)))

  id <- id + codedAsInteger(x$scenario)*10^exponent
  exponent <- exponent + getShiftFactor(nlevels(factor(x$scenario)))

  id <- id + codedAsInteger(x$region)*10^exponent
  exponent <- exponent + getShiftFactor(nlevels(factor(x$region)))

  id <- id + codedAsInteger(x$variable)*10^exponent
  exponent <- exponent + getShiftFactor(nlevels(factor(x$variable)))

  id <- id + (codedAsInteger(x$period)*(10^exponent))

  return(list(message="data contains %# duplicate entries.",
              failed=id[duplicated(id)]))

}
