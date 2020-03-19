# The next 3 functions are inherited by the 'ArgumentCheck' package, which is deprecated
# and not necessarily available on CRAN, however, I like the next 3 functions such that
# I've just added them here, unaltered.

newArgCheck <- function() {
  argcheck <- new.env()
  assign("n_warn", 0, envir = argcheck)
  assign("warn_msg", NULL, envir = argcheck)
  assign("n_error", 0, envir = argcheck)
  assign("error_msg", NULL, envir = argcheck)
  assign("n_message", 0, envir = argcheck)
  assign("message_msg", NULL, envir = argcheck)
  class(argcheck) <- c("ArgCheck", "environment")
  return(argcheck)
}

addError <- function(msg, argcheck){

  if (!"ArgCheck" %in% class(argcheck))
    stop("'argcheck' must be an object of class 'ArgCheck'")
  assign("n_error", get("n_error", envir = argcheck) + 1, envir = argcheck)
  assign("error_msg", c(get("error_msg", envir = argcheck), msg), envir = argcheck)
}

finishArgCheck <- function(argcheck){

  fn_call <- sys.call(-1)
  fn_call <- utils::capture.output(fn_call)
  if (!"ArgCheck" %in% class(argcheck)) stop("'argcheck' must be an object of class 'ArgCheck'")
  argcheck <- mget(ls(envir = argcheck), envir = argcheck)
  if (argcheck$n_warn > 0)
    warning(paste0(c("", fn_call, paste0(1:argcheck$n_warn, ": ", argcheck$warn_msg)), collapse = "\n"), call. = FALSE)
  if (argcheck$n_message > 0)
    message(paste0(c("", fn_call, paste0(1:argcheck$n_message, ": ", argcheck$message_msg)), collapse = "\n"))
  if (argcheck$n_error > 0)
    stop(paste0(c("", fn_call, paste0(1:argcheck$n_error, ": ", argcheck$error_msg)), collapse = "\n"), call. = FALSE)
}

#' Checking if an object is a data.table object and (optional) testing if some column names are valid for it
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param colNamesToBeChecked Character vector containing potential column names of the 'inputDT' argument. The default value is NULL.
#' @return No value is returned if all elements in the 'colNamesToBeChecked' argument, are valid column names of the 'inputDT' argument. In the absence of a value for the 'colNamesToBeChecked' argument, it is only tested if the 'inputDT' argument is a data.table object (is tested irrespective of the value for the 'colNamesToBeChecked' argument).
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#'
#' checkDT(inputDT)
#' checkDT(inputDT, c('x', 'y'))
#'
#' \donttest{checkDT(inputDT, c('x', 'y1'))
#' checkDT(inputDT, c('x', 'y1', 'z1'))
#' checkDT(inputDT, c('x1', 'y1', 'z1'))}

checkDT <- function(inputDT, colNamesToBeChecked = NULL){

  dataTableArgName = deparse(substitute(inputDT))
  if(!is.data.table(inputDT)) stop(paste(paste('The ',  dataTableArgName, sep = ''), ' argument should be a data.table object.', sep = ''))
  checkCharVec(list(colNamesToBeChecked))

  if(!is.null(colNamesToBeChecked)){
    badColNames <- colNamesToBeChecked[!colNamesToBeChecked %in% names(inputDT)]
    if(length(badColNames) == 1){
      stop(sprintf("%s is not a valid column name for the 'inputDT' argument.", badColNames))
    } else if (length(badColNames) > 1){
      if(length(badColNames) > 2) badColNames <- c(paste(badColNames[1:(length(badColNames) - 1)], collapse = ', '), badColNames[length(badColNames)])
      badColNamesTogether <- paste(badColNames, collapse = ' and ')
      stop(sprintf("%s are not valid column names for the 'inputDT' argument.", badColNamesTogether))
    }
  }
}

#' Testing if a set of columns of a data.table object corresponds to the factor data type
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param colNamesToBeChecked Character vector containing potential column names of the 'inputDT' argument. The default value is NULL.
#' @param returnNames Logical vector of length 1 indicating whether or not the column name of the selected factors should be returned. The default value is FALSE.
#' @return A logical vector of length the size of the 'colNamesToBeChecked' argument, or in the absence of a value the number of columns of the 'inputDT' argument, that is TRUE if the corresponding column of the 'inputDT' argument is a factor. If the 'returnNames' argument equals TRUE, then only those column names from the aforementioned selection of column of the 'inputDT' argument are returned that are a factor.
#'
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#'
#' isFactorDT(inputDT)
#' isFactorDT(inputDT, c('x', 'y'))
#' isFactorDT(inputDT, returnNames = TRUE)
#'
#' isFactorDT(inputDT, 'y')
#' \donttest{isFactorDT(inputDT, c('x', 'y1'))}

isFactorDT <- function(inputDT, colNamesToBeChecked = NULL, returnNames = FALSE){

  checkDT(inputDT)
  checkLogicVec(list(returnNames))
  checkLength(returnNames, 1)

  if(is.null(colNamesToBeChecked)){
    colNamesToBeChecked <- names(inputDT)
    indexColumns <- 1:ncol(inputDT)
  } else {
    checkCharVec(list(colNamesToBeChecked))
    checkDT(inputDT, colNamesToBeChecked)
    indexColumns <- sort(match(colNamesToBeChecked, names(inputDT)))
  }
  isFactor <- rep(NA, length(indexColumns))
  index <- 1
  for(iCol in indexColumns){
    isFactor[index] <- is.factor(inputDT[,.SD,.SDcols = iCol][[1]])
    index <- index + 1
  }
  if(returnNames){
    return(names(inputDT)[isFactor])
  } else{
    return(isFactor)
  }
}

#' Testing if a set of columns of a data.table object corresponds to the numeric data type
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param colNamesToBeChecked Character vector containing potential column names of the 'inputDT' argument. The default value is NULL.
#' @param returnNames Logical vector of length 1 indicating whether or not the column name of the selected numerics should be returned. The default value is FALSE.
#' @return A logical vector of length the size of the 'colNamesToBeChecked' argument, or in the absence of a value the number of columns of the 'inputDT' argument, that is TRUE if the corresponding column of the 'inputDT' argument is a numeric. If the 'returnNames' argument equals TRUE, then only those column names from the aforementioned selection of column of the 'inputDT' argument are returned that are a numeric.
#'
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#'
#' isNumericDT(inputDT)
#' isNumericDT(inputDT, c('x', 'y'))
#' isNumericDT(inputDT, returnNames = TRUE)
#'
#' isNumericDT(inputDT, 'x')
#' \donttest{isNumericDT(inputDT, c('x', 'y1'))}

isNumericDT <- function(inputDT, colNamesToBeChecked = NULL, returnNames = FALSE){

  checkDT(inputDT)
  checkLength(returnNames, 1)
  if(is.null(colNamesToBeChecked)){
    colNamesToBeChecked <- names(inputDT)
    indexColumns <- 1:ncol(inputDT)
  } else {
    checkCharVec(list(colNamesToBeChecked))
    checkDT(inputDT, colNamesToBeChecked)
    indexColumns <- sort(match(colNamesToBeChecked, names(inputDT)))
  }
  isNumeric <- rep(NA, length(indexColumns))
  index <- 1
  for(iCol in indexColumns){
    isNumeric[index] <- is.numeric(inputDT[,.SD,.SDcols = iCol][[1]])
    index <- index + 1
  }
  if(returnNames){
    return(names(inputDT)[isNumeric])
  } else{
    return(isNumeric)
  }
}

#' Testing if a set of columns of a data.table object corresponds to the integer data type
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param colNamesToBeChecked Character vector containing potential column names of the 'inputDT' argument. The default value is NULL.
#' @param returnNames Logical vector of length 1 indicating whether or not the column name of the selected integers should be returned. The default value is FALSE.
#' @return A logical vector of length the size of the 'colNamesToBeChecked' argument, or in the absence of a value the number of columns of the 'inputDT' argument, that is TRUE if the corresponding column of the 'inputDT' argument is an integer If the 'returnNames' argument equals TRUE, then only those column names from the aforementioned selection of column of the 'inputDT' argument are returned that are an integer.
#'
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = seq(1L, 20L, 2L), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#'
#' isIntegerDT(inputDT)
#' isIntegerDT(inputDT, c('x', 'y'))
#' isIntegerDT(inputDT, returnNames = TRUE)
#'
#' isIntegerDT(inputDT, 'x')
#' \donttest{isIntegerDT(inputDT, c('x', 'y1'))}

isIntegerDT <- function(inputDT, colNamesToBeChecked = NULL, returnNames = FALSE){

  checkDT(inputDT)
  checkLength(returnNames, 1)
  if(is.null(colNamesToBeChecked)){
    colNamesToBeChecked <- names(inputDT)
    indexColumns <- 1:ncol(inputDT)
  } else {
    checkCharVec(list(colNamesToBeChecked))
    checkDT(inputDT, colNamesToBeChecked)
    indexColumns <- sort(match(colNamesToBeChecked, names(inputDT)))
  }
  isInteger <- rep(NA, length(indexColumns))
  index <- 1
  for(iCol in indexColumns){
    isInteger[index] <- is.integer(inputDT[,.SD,.SDcols = iCol][[1]])
    index <- index + 1
  }
  if(returnNames){
    return(names(inputDT)[isInteger])
  } else{
    return(isInteger)
  }
}

#' Testing if a set of columns of a data.table object corresponds to the logical/boolean data type
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param colNamesToBeChecked Character vector containing potential column names of the 'inputDT' argument. The default value is NULL.
#' @param returnNames Logical vector of length 1 indicating whether or not the column name of the selected booleans should be returned. The default value is FALSE.
#' @return A logical vector of length the size of the 'colNamesToBeChecked' argument, or in the absence of a value the number of columns of the 'inputDT' argument, that is TRUE if the corresponding column of the 'inputDT' argument is a boolean. If the 'returnNames' argument equals TRUE, then only those column names from the aforementioned selection of column of the 'inputDT' argument are returned that are a boolean.
#'
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = rep(c(TRUE, FALSE), 5), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#'
#' isLogicalDT(inputDT)
#' isLogicalDT(inputDT, c('x', 'y'))
#' isLogicalDT(inputDT, returnNames = TRUE)
#'
#' isLogicalDT(inputDT, 'x')
#' \donttest{isLogicalDT(inputDT, c('x', 'y1'))}

isLogicalDT <- function(inputDT, colNamesToBeChecked = NULL, returnNames = FALSE){

  checkDT(inputDT)
  checkLength(returnNames, 1)
  if(is.null(colNamesToBeChecked)){
    colNamesToBeChecked <- names(inputDT)
    indexColumns <- 1:ncol(inputDT)
  } else {
    checkCharVec(list(colNamesToBeChecked))
    checkDT(inputDT, colNamesToBeChecked)
    indexColumns <- sort(match(colNamesToBeChecked, names(inputDT)))
  }
  isLogical <- rep(NA, length(indexColumns))
  index <- 1
  for(iCol in indexColumns){
    isLogical[index] <- is.logical(inputDT[,.SD,.SDcols = iCol][[1]])
    index <- index + 1
  }
  if(returnNames){
    return(names(inputDT)[isLogical])
  } else{
    return(isLogical)
  }
}

#' Testing if a set of columns of a data.table object corresponds to the character/string data type
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param colNamesToBeChecked Character vector containing potential column names of the 'inputDT' argument. The default value is NULL.
#' @param returnNames Logical vector of length 1 indicating whether or not the column name of the selected strings should be returned. The default value is FALSE.
#' @return A logical vector of length the size of the 'colNamesToBeChecked' argument, or in the absence of a value the number of columns of the 'inputDT' argument, that is TRUE if the corresponding column of the 'inputDT' argument is a string If the 'returnNames' argument equals TRUE, then only those column names from the aforementioned selection of column of the 'inputDT' argument are returned that is a string.
#'
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = rep(c(TRUE, FALSE), 5), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#'
#' isCharacterDT(inputDT)
#'
#' inputDT2 <- as.data.table(data.frame(y = LETTERS[1:10]))
#'
#' isCharacterDT(inputDT2)
#' \donttest{isCharacterDT(inputDT2, c('x', 'y'))}
#' isCharacterDT(inputDT2, returnNames = TRUE)

isCharacterDT <- function(inputDT, colNamesToBeChecked = NULL, returnNames = FALSE){

  checkDT(inputDT)
  checkLength(returnNames, 1)
  if(is.null(colNamesToBeChecked)){
    colNamesToBeChecked <- names(inputDT)
    indexColumns <- 1:ncol(inputDT)
  } else {
    checkCharVec(list(colNamesToBeChecked))
    checkDT(inputDT, colNamesToBeChecked)
    indexColumns <- sort(match(colNamesToBeChecked, names(inputDT)))
  }
  isCharacter <- rep(NA, length(indexColumns))
  index <- 1
  for(iCol in indexColumns){
    isCharacter[index] <- is.character(inputDT[,.SD,.SDcols = iCol][[1]])
    index <- index + 1
  }
  if(returnNames){
    return(names(inputDT)[isCharacter])
  } else{
    return(isCharacter)
  }
}

#' Forcing the character/string data type on a selected set of columns of a data.table object
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param colNamesToBeTransformed Character vector containing potential column names of the 'inputDT' argument. The default value is NULL.
#' @return No value is returned. Note that a valid value needs to be supplied to the 'colNamesToBeTransformed' argument in order to make this function work.
#'
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#'
#' \donttest{asCharacterDT(inputDT)}
#' asCharacterDT(inputDT, c('x', 'y'))
#'
#' # First looking at the result, followed by testing if the transformation worked!
#'
#' inputDT
#' isCharacterDT(inputDT, c('x', 'y'))
#' isFactorDT(inputDT, c('x', 'y'))

asFactorDT <- function(inputDT, colNamesToBeTransformed = NULL){

  if(is.null(colNamesToBeTransformed)) stop('Please provide the column names to the "colNamesToBeTransformed" argument of which the values of the whole columns need to be transformed to a numeric value.')
  checkCharVec(list(colNamesToBeTransformed))
  checkDT(inputDT, colNamesToBeTransformed)
  indexColumns <- sort(match(colNamesToBeTransformed, names(inputDT)))
  for(iCol in indexColumns){
    set(inputDT, NULL, as.integer(iCol), as.factor(inputDT[,.SD,.SDcols = iCol][[1]]))
  }
}

#' Forcing the numeric data type on a selected set of columns of a data.table object
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param colNamesToBeTransformed Character vector containing potential column names of the 'inputDT' argument. The default value is NULL.
#' @return No value is returned. Note that a valid value needs to be supplied to the 'colNamesToBeTransformed' argument in order to make this function work.
#'
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
#'
#' \donttest{asNumericDT(inputDT)}
#' asNumericDT(inputDT, c('x', 'y'))
#'
#' # First looking at the result, followed by testing if the transformation worked!
#'
#' inputDT
#' isNumericDT(inputDT, c('x', 'y'))
#' isIntegerDT(inputDT, c('x', 'y'))

asNumericDT <- function(inputDT, colNamesToBeTransformed = NULL){

  if(is.null(colNamesToBeTransformed)) stop('Please provide the column names to the "colNamesToBeTransformed" argument of which the values of the whole columns need to be transformed to a numeric value.')
  checkCharVec(list(colNamesToBeTransformed))
  checkDT(inputDT, colNamesToBeTransformed)
  indexColumns <- sort(match(colNamesToBeTransformed, names(inputDT)))
  for(iCol in indexColumns){
    set(inputDT, NULL, as.integer(iCol), as.numeric(inputDT[,.SD,.SDcols = iCol][[1]]))
  }
}

#' Forcing the integer data type on a selected set of columns of a data.table object
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param colNamesToBeTransformed Character vector containing potential column names of the 'inputDT' argument. The default value is NULL.
#' @return No value is returned. Note that a valid value needs to be supplied to the 'colNamesToBeTransformed' argument in order to make this function work.
#'
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#'
#' \donttest{asIntegerDT(inputDT)}
#' asIntegerDT(inputDT, c('x', 'y'))
#'
#' # First looking at the result, followed by testing if the transformation worked!
#'
#' inputDT
#' isIntegerDT(inputDT, c('x', 'y'))
#'
#' # Note the following behavior that also holds for the as.integer() base R function.
#' isNumericDT(inputDT, c('x', 'y'))

asIntegerDT <- function(inputDT, colNamesToBeTransformed = NULL){

  if(is.null(colNamesToBeTransformed)) stop('Please provide the column names to the "colNamesToBeTransformed" argument of which the values of the whole columns need to be transformed to a numeric value.')
  checkCharVec(list(colNamesToBeTransformed))
  checkDT(inputDT, colNamesToBeTransformed)
  indexColumns <- sort(match(colNamesToBeTransformed, names(inputDT)))
  for(iCol in indexColumns){
    set(inputDT, NULL, as.integer(iCol), as.integer(inputDT[,.SD,.SDcols = iCol][[1]]))
  }
}

#' Forcing the logical/boolean data type on a selected set of columns of a data.table object
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param colNamesToBeTransformed Character vector containing potential column names of the 'inputDT' argument. The default value is NULL.
#' @return No value is returned. Note that a valid value needs to be supplied to the 'colNamesToBeTransformed' argument in order to make this function work.
#'
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#'
#' \donttest{asLogicalDT(inputDT)}
#' asLogicalDT(inputDT, c('x', 'y'))
#'
#' # First looking at the result, followed by testing if the transformation worked!
#'
#' inputDT
#' isLogicalDT(inputDT, c('x', 'y'))
#'
#' # Notice the 'funny' side effect for the 'F' character value of column y...
#' # This behavior is also observed for the as.logical() base R function.

asLogicalDT <- function(inputDT, colNamesToBeTransformed = NULL){

  if(is.null(colNamesToBeTransformed)) stop('Please provide the column names to the "colNamesToBeTransformed" argument of which the values of the whole columns need to be transformed to a numeric value.')
  checkCharVec(list(colNamesToBeTransformed))
  checkDT(inputDT, colNamesToBeTransformed)
  indexColumns <- sort(match(colNamesToBeTransformed, names(inputDT)))
  for(iCol in indexColumns){
    set(inputDT, NULL, as.integer(iCol), as.logical(inputDT[,.SD,.SDcols = iCol][[1]]))
  }
}

#' Forcing the character/string data type on a selected set of columns of a data.table object
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param colNamesToBeTransformed Character vector containing potential column names of the 'inputDT' argument. The default value is NULL.
#' @return No value is returned. Note that a valid value needs to be supplied to the 'colNamesToBeTransformed' argument in order to make this function work.
#'
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#'
#' \donttest{asCharacterDT(inputDT)}
#' asCharacterDT(inputDT, c('x', 'y'))
#'
#' # First looking at the result, followed by testing if the transformation worked!
#'
#' inputDT
#' isCharacterDT(inputDT, c('x', 'y'))
#' isFactorDT(inputDT, c('x', 'y'))

asCharacterDT <- function(inputDT, colNamesToBeTransformed = NULL){

  if(is.null(colNamesToBeTransformed)) stop('Please provide the column names to the "colNamesToBeTransformed" argument of which the values of the whole columns need to be transformed to a numeric value.')
  checkCharVec(list(colNamesToBeTransformed))
  checkDT(inputDT, colNamesToBeTransformed)
  indexColumns <- sort(match(colNamesToBeTransformed, names(inputDT)))
  for(iCol in indexColumns){
    set(inputDT, NULL, as.integer(iCol), as.character(inputDT[,.SD,.SDcols = iCol][[1]]))
  }
}

#' Detecting which levels of which factor of a data.table object contain non-alpha numeric characters (including whitespace) characters
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @return No value is returned. Note that a valid value needs to be supplied to the 'colNamesToBeChecked' argument in order to make this function work.
#'
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2)))
#' detectWeirdLevelNamesDT(inputDT)
#'
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#' detectWeirdLevelNamesDT(inputDT)
#'
#' inputDT <- as.data.table(data.frame(x = c(rep('test_', 5), rep('test@', 5)),
#' y = c(rep('test_', 5), rep('test@', 5))))
#' asFactorDT(inputDT, c('x', 'y'))
#' \donttest{detectWeirdLevelNamesDT(inputDT)}

detectWeirdLevelNamesDT <- function(inputDT){

  checkDT(inputDT)
  indexFactors <- which(isFactorDT(inputDT, NULL, FALSE))
  if(length(indexFactors) > 0){
    checkList <- newArgCheck()
    for(iFact in 1:length(indexFactors)){
      selectedFactor <- inputDT[,.SD,.SDcols = indexFactors[iFact]]
      levelsFactor <- levels(selectedFactor[[1]])
      badLevels <- !grepl("^[A-Za-z0-9]+$", levelsFactor)
      if(sum(badLevels) == 1) stop(paste(paste(paste(paste('The level ', paste(levelsFactor[badLevels], collapse = ', '), sep = ''), 'of the variable ', sep = ''), names(inputDT)[iFact], sep = ''), ' contains non-alpha numeric characters (including whitespace). Please remove this.', sep = ''))
      if(sum(badLevels) > 1){
        selectedBadLevels <- levelsFactor[badLevels]
        if(length(badLevels) > 2) selectedBadLevels <- c(paste(selectedBadLevels[1:(length(selectedBadLevels) - 1)], collapse = ', '), selectedBadLevels[length(selectedBadLevels)])
        selectedBadLevels <- paste(selectedBadLevels, collapse = ' and ')
        addError(msg = paste(paste(paste(paste('The levels ', selectedBadLevels, sep = ''), 'of the variable ', sep = ''), names(inputDT)[iFact], sep = ''), ' contain non-alpha numeric characters (including whitespace). Please remove this.', sep = ''), argcheck = checkList)
      }
    }
    finishArgCheck(checkList)
  }
}

#' Extracting the levels of all or a selected set of the factor columns of a data.table object
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param categoricalVar Character vector containing potential column names of the 'inputDT' argument. The default value is NULL.
#' @return A named list is returned, with as names the different valid factor column names, either of the whole 'inputDT' argument, either of the factor variables of which the names are listed in 'categoricalVar' argument, containing a character vector with the different levels of the respective factor. In case that the 'categoricalVar' argument contains column names that aren't factors, a warning is thrown. An empty is list is returned when no valid factors (with or without the 'categoricalVar' selection turned on) are found.
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = LETTERS[11:20], y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('x', 'y'))
#'
#' extractLevelDT(inputDT)
#' extractLevelDT(inputDT, c('x', 'y'))
#' \donttest{extractLevelDT(inputDT, c('x', 'y1'))}
#'
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#' extractLevelDT(inputDT)
#'
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = seq(2, 21, 2)))
#' extractLevelDT(inputDT)
#' \donttest{extractLevelDT(inputDT, c('x', 'y'))}

extractLevelDT <- function(inputDT, categoricalVar = NULL){

  if(is.null(categoricalVar)) categoricalVar <- isFactorDT(inputDT, NULL, TRUE)
  checkCharVec(list(categoricalVar))
  checkDT(inputDT, categoricalVar)
  indexFactors <- which(names(inputDT) %in% categoricalVar)
  levelsFactor <- list()
  if(length(indexFactors)){
    notFactors <- !isFactorDT(inputDT[,.SD,.SDcols = indexFactors], NULL, FALSE)
    if(sum(notFactors) == 1) warning(paste(paste('The variable ', paste(categoricalVar[notFactors], collapse = ', '), sep = ''), ' of the "inputDT" argument is a not factor.', sep = ''))
    if(sum(notFactors) > 1){
      selectedNotCatVar <- categoricalVar[notFactors]
      if(length(notFactors) > 2) selectedNotCatVar <- c(paste(selectedNotCatVar[1:(length(selectedNotCatVar) - 1)], collapse = ', '), selectedNotCatVar[length(selectedNotCatVar)])
      selectedNotCatVar <- paste(selectedNotCatVar, collapse = ' and ')
      warning(paste(paste('The variables ', selectedNotCatVar, sep = ''), ' of the "inputDT" argument are not factors.', sep = ''))
    }
    indexFactors <- indexFactors[!notFactors]
    if(length(indexFactors)){
      length(levelsFactor) <- length(indexFactors)
      for(iFact in 1:length(indexFactors)){
        selectedFactor <- inputDT[,.SD,.SDcols = indexFactors[iFact]]
        levelsFactor[[iFact]] <- levels(selectedFactor[[1]])
      }
      names(levelsFactor) <- names(inputDT[,.SD,.SDcols = indexFactors])
    }
  }
  return(levelsFactor)
}

#' Extracting the reference level of all or a selected set of the factor columns of a data.table object
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param categoricalVar Character vector containing potential column names of the 'inputDT' argument. The default value is NULL.
#' @return A named list is returned, with as names the different valid factor column names, either of the whole 'inputDT' argument, either of the factor variables of which the names are listed in 'categoricalVar' argument, containing a character vector of length 1 with the reference level of the respective factor. In case that the 'categoricalVar' argument contains column names that aren't factors, a warning is thrown. An empty is list is returned when no valid factors (with or without the 'categoricalVar' selection turned on) are found.
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = LETTERS[11:20], y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('x', 'y'))
#'
#' extractRefLevelDT(inputDT)
#' \donttest{extractRefLevelDT(inputDT, c('x', 'y'))
#' extractRefLevelDT(inputDT, c('x', 'y1'))}
#'
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#' extractRefLevelDT(inputDT)
#'
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = seq(2, 21, 2)))
#' extractRefLevelDT(inputDT)
#' \donttest{extractRefLevelDT(inputDT, c('x', 'y'))}

extractRefLevelDT <- function(inputDT, categoricalVar = NULL){

  levelsFactor <- extractLevelDT(inputDT, categoricalVar)
  refLevels <- list()
  length(refLevels) <- length(levelsFactor)
  names(refLevels) <- names(levelsFactor)
  refLevels <- llply(levelsFactor, function(xx) xx[1])
  return(refLevels)
}

#' Setting the reference level of all or a selected set of the factor columns of a data.table object
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param categoricalVar Character vector containing potential column names of the 'inputDT' argument. This is an obligatory argument, without default value.
#' @param referenceLevel Character vector containing the new reference levels. This is an obligatory argument, without default value.
#' @return No value is returned. Note that the 'categoricalVar' and 'referenceLevel' should match up, meaning that they should be of the same length and the ith element should refer to the same variable.
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = LETTERS[11:20], y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('x', 'y'))
#'
#' \donttest{setRefLevelDT(inputDT)}
#'
#' levels(inputDT$x)[1]
#' levels(inputDT$y)[1]
#' setRefLevelDT(inputDT, c('x', 'y'), c('L', 'C'))
#' levels(inputDT$x)[1]
#' levels(inputDT$y)[1]
#'
#' \donttest{setRefLevelDT(inputDT, c('x', 'y'), c('bla', 'bla'))}
#'
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#' levels(inputDT$y)[1]
#' setRefLevelDT(inputDT, 'y', 'E')
#' levels(inputDT$y)[1]

setRefLevelDT <- function(inputDT, categoricalVar, referenceLevel){

  checkCharVec(list(categoricalVar, referenceLevel))
  checkDT(inputDT, categoricalVar)
  indexFactors <- which(names(inputDT) %in% categoricalVar)
  notFactors <- !isFactorDT(inputDT[,.SD,.SDcols = indexFactors], NULL, FALSE)
  if(sum(notFactors) != 0) stop(paste(paste('The variables ', paste(categoricalVar[notFactors], collapse = ', '), sep = ''), ' of the "inputDT" argument are not factors.', sep = ''))

  for(iFact in 1:length(indexFactors)){
    selectedFactor <- inputDT[,.SD,.SDcols = indexFactors[iFact]]
    if(!isPresentCharVec(levels(selectedFactor[[1]]), referenceLevel[iFact])) stop(sprintf("Element %d of the 'referenceLevel' argument, '%s', does not correspond to a valid level of '%s', stored in element %d of the 'categoricalVar' argument.", iFact, referenceLevel[iFact], categoricalVar[iFact], iFact))
    set(inputDT, NULL, as.integer(indexFactors[iFact]), relevel(selectedFactor[[1]], ref = referenceLevel[iFact]))
  }
}

# Helper function, not important enough to be oxygenized ;-)

isPresentCharVec <- function(inputVector, value2BeChecked){
  if(!is.character(inputVector) & !is.factor(inputVector)){
    stop("The 'inputVector' argument should be a factor or a character vector.")
  }
  if(!is.character(value2BeChecked) & !is.factor(value2BeChecked)){
    stop("The 'value2BeChecked' argument should be a factor or a character vector.")
  }
  if(length(which(inputVector == value2BeChecked)) > 0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Remove empty levels from all the factor columns of a data.table object
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @return No value is returned.
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#' levels(inputDT$y)
#' removeEmptyLevelsDT(inputDT)
#' levels(inputDT$y)
#' removeEmptyLevelsDT(inputDT[x < 10])
#' levels(inputDT$y)
#'
#' # You need to define a new data.table object
#' # in order to make the 'removeEmptyLevelsDT' function work.
#' reducedDT <- inputDT[x < 10]
#' levels(reducedDT$y)
#' removeEmptyLevelsDT(reducedDT)
#' levels(reducedDT$y)

removeEmptyLevelsDT <- function(inputDT){

  checkDT(inputDT)
  indexFactor <- which(isFactorDT(inputDT))
  for(iFactor in indexFactor){
    selectedVar <- inputDT[,.SD,.SDcols = iFactor]
    selectedVar[[1]] <- droplevels(selectedVar[[1]])
    set(inputDT, NULL, as.integer(iFactor), selectedVar)
  }
}

#' Transform levels of all the factor columns of a data.table object to missing if too little observations pertain to a given level of it.
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param minNumberLevel Numeric vector of length 1 that indicates the minimal number of observations of a given level that should be observed to avoid that that level will be deleted from the list of possible levels for that factor and the value of its observations will be turned into missing values.
#' @return No value is returned. The level that was not underpopulated is also removed from the levels of the respective categorical variable.
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#' levels(inputDT$y)
#' lowFreqLevel2MissingDT(inputDT, 2)
#' levels(inputDT$y)
#'
#' inputDT <- as.data.table(data.frame(x = seq(1, 40, 2),
#' y = c(LETTERS[1:10], LETTERS[1:10])))
#' asFactorDT(inputDT, c('y'))
#' levels(inputDT$y)
#' lowFreqLevel2MissingDT(inputDT, 1)
#' levels(inputDT$y)

lowFreqLevel2MissingDT <- function(inputDT, minNumberLevel = NULL){

  checkDT(inputDT)
  if(is.null(minNumberLevel)) stop("The 'minNumberLevel' argument should be a numerical vector of length 1 in which a strictly positive whole number is stored.")
  checkNumVec(list(minNumberLevel))
  if(!(minNumberLevel > 0 & minNumberLevel == round(minNumberLevel, digits = 0))) stop("The 'minNumberLevel' argument should be a numerical vector of length 1 in which a strictly positive whole number is stored.")
  for(iCol in 1:ncol(inputDT)){
    selectedVar <- inputDT[,.SD,.SDcols = iCol]
    if(is.factor(selectedVar[[1]])){
      freqTableFactor <- table(selectedVar[[1]])
      levelsFactor <- names(freqTableFactor)
      index2BeTransformed <- which(freqTableFactor <= minNumberLevel)
      if(length(index2BeTransformed) > 0){
        for(iMiss in 1:length(index2BeTransformed)){
          index <- which(selectedVar[[1]] == levelsFactor[index2BeTransformed[iMiss]])
          set(selectedVar, as.integer(index), 1L, NA)
        }
        selectedVar[[1]] <- droplevels(selectedVar[[1]])
      }
    }
    set(inputDT, NULL, as.integer(iCol), selectedVar)
  }
}

#' Glueing, not merging, two data.table objects together, by matching column names
#'
#' @param topDT data.table object 1. Its values will be placed at the top of the returned data.table object. This is an obligatory argument, without default value.
#' @param bottomDT data.table object 2. Its values will be placed at the bottom of the returned data.table object. This is an obligatory argument, without default value.
#' @return The glued data.table object. Matching column names of 'topDT' and 'bottomDT' will be identified and its values will be placed in one column in the returned data.table object, the values of the 'topDT' argument on top of the values of the 'bottomDT' argument. Non-matching columns will be have missing values for the rows in the returned data.table object that correspond to the input data.table object in which the column name was not found.
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10], z = LETTERS[1:10]))
#' inputDT2 <- as.data.table(data.frame(p = seq(1, 40, 2), x = c(LETTERS[1:10], LETTERS[1:10]),
#' l = c(LETTERS[1:10], LETTERS[1:10]), m = c(LETTERS[1:10], LETTERS[1:10])))
#' asFactorDT(inputDT, c('y', 'z'))
#' asFactorDT(inputDT2, c('x', 'l', 'm))
#'
#' rbindDT(inputDT, inputDT2)

rbindDT <- function(topDT, bottomDT){

  checkDT(topDT)
  checkDT(bottomDT)

  nLeft <- nrow(topDT)
  nRight <- nrow(bottomDT)
  nTotal <- nLeft + nRight

  namesTopDT <- names(topDT)
  namesBottomDT <- names(bottomDT)
  inBoth <- intersect(namesBottomDT, namesTopDT)

  indexMatchesTopDT <- match(inBoth, namesTopDT)
  indexNoMatchesTopDT <- setdiff(1:NCOL(topDT), indexMatchesTopDT)
  indexMatchesBottomDT <- match(inBoth, namesBottomDT)
  indexNoMatchesBottomDT <- setdiff(1:NCOL(bottomDT), indexMatchesBottomDT)

  if(length(indexMatchesTopDT)){
    matchedPart <- rbind(topDT[,.SD,.SDcols = indexMatchesTopDT], bottomDT[,.SD,.SDcols = indexMatchesBottomDT])
  }
  if(length(indexNoMatchesTopDT)){
    nonMatchedPartA <- rbind(topDT[,.SD,.SDcols = indexNoMatchesTopDT], data.table(rep(NA, nTotal - nLeft)), fill = T)
    nonMatchedPartA <- nonMatchedPartA[,.SD,.SDcols = 1:(ncol(nonMatchedPartA)-1)]
  }
  if(length(indexNoMatchesBottomDT)){
    nonMatchedPartB <- rbind(data.table(rep(NA, nTotal - nRight)), bottomDT[,.SD,.SDcols = indexNoMatchesBottomDT], fill = T)
    nonMatchedPartB <- nonMatchedPartB[,.SD,.SDcols = 2:(ncol(nonMatchedPartB))]
  }

  if(length(indexMatchesTopDT) & length(indexNoMatchesTopDT) & length(indexNoMatchesBottomDT)){
    result <- cbind(matchedPart, nonMatchedPartA, nonMatchedPartB)
  } else if(length(indexMatchesTopDT) & length(indexNoMatchesTopDT) & !length(indexNoMatchesBottomDT)){
    result <- cbind(matchedPart, nonMatchedPartA)
  } else if(length(indexMatchesTopDT) & !length(indexNoMatchesTopDT) & length(indexNoMatchesBottomDT)){
    result <- cbind(matchedPart, nonMatchedPartB)
  } else if(length(indexMatchesTopDT) & !length(indexNoMatchesTopDT) & !length(indexNoMatchesBottomDT)){
    result <- matchedPart
  } else if(!length(indexMatchesTopDT) & length(indexNoMatchesTopDT) & length(indexNoMatchesBottomDT)){
    result <- cbind(nonMatchedPartA, nonMatchedPartB)
  } else if(!length(indexMatchesTopDT) & !length(indexNoMatchesTopDT) & length(indexNoMatchesBottomDT)){
    result <- nonMatchedPartB
  } else if(!length(indexMatchesTopDT) & length(indexNoMatchesTopDT) & !length(indexNoMatchesBottomDT)){
    result <- nonMatchedPartA
  }

  return(result)

}

#' Order the rows of a data.table object by index
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param rowIndices Integer vector that contains the row indices according to which the 'inputDT' object should be ordered. This is an obligatory argument, without default value.
#' @return The 'inputDT' data.table object, ordered according to the 'rowIndices' argument. This function assumes that the length of the 'rowIndices' argument is correspond to the number of rows of the 'inputDT' argument. If the length of the 'rowIndices' argument is smaller than the number of rows of the 'inputDT' argument, the values of the 'rowIndices' argument are recycled until the as many indices as number of rows of the 'inputDT' argument is obtained.
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = 10:1, y = LETTERS[1:10]))
#' asFactorDT(inputDT, c('y'))
#' inputDT
#' sortByRowIndexDT(inputDT, 10:1)
#' inputDT

sortByRowIndexDT <- function(inputDT, rowIndices){
  checkDT(inputDT)
  checkIntVec(list(rowIndices))
  if(min(rowIndices) <= 0 | max(rowIndices) > nrow(inputDT)) stop("The 'rowIndices' arguments contains values that are not permitted (index should be strictly positive and smaller or equal to the number of rows of the 'inputDT' argument.")
  indexCol <- NULL
  setorder(inputDT[, indexCol := rowIndices], indexCol)[, indexCol := NULL]
}
