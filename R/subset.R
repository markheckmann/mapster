#//////////////////////////////////////////////////////////////////////////////
#
#                   FUNCTIONS TO SUBSETTING DATAFRAMES
#
#//////////////////////////////////////////////////////////////////////////////


#' \code{which} with different treatment of NULL
#' 
#' \code{which} function that returns \code{integer(0)} when \code{x=NULL}.
#' @param x A logical vector.
#' @export
#' @keywords internal
#' @examples
#' which2(c(T,F,T))
#' which2(NULL)
#' 
which2 <- function(x)
{
  x <- unname(unlist(x)) 
  if (is.null(x))
    return(integer(0))
  which(x)
}


#' Extract variable names used in logical expression
#' 
#' The left part of possibly concatenated logical expressions are extracted. Use
#' is intended for variable names in dataframes. Alphanumerics, dots and
#' underscores are allowed as variable names.
#' 
#' @param e Logical expression as a string or wrappd in \code{plyr::.} function.
#' @return A character vector containing the variable names.
#' @keywords internal
#' @examples
#' vars_in_expression(.(a.1==1 & b==2))
#' 
vars_in_expression <- function(e=.())
{
  e <- as.character(e)
  conds <- stringr::str_split(e, "[|&]")[[1]]        # split on operators connecting logical expressions (xor is exluded)
  conds <- stringr::str_trim(conds)                  # trim leading spaces
  stringr::str_extract(conds, "^([:alnum:]|[._])+")  # varnames may have alphanums plus dots and underscores in them
}


#' Stop if not all variables in logical expression exist in dataframe
#' 
#' Function checks if all variables used in logical subset expression are
#' contained in dataframe. If not an error is thrown and the missing variables
#' are listed.
#' 
#' @param  x A dataframe.
#' @inheritParams vars_in_expression
#' @keywords internal
#' @examples \dontrun{
#' stop_if_not_all_subset_vars_in_df(mtcars, .(mpg > 20))
#' stop_if_not_all_subset_vars_in_df(mtcars, .(mpg2 > 20))
#' }
#' 
stop_if_not_all_subset_vars_in_df <- function(x, e)
{
  vars <- vars_in_expression(e)
  ii <- !vars %in% names(x)
  not.contained <- vars[ii]
  if (length(not.contained) > 0)
    stop("subset variables not found in dataframe:", paste(" ", not.contained), call. = FALSE)
  
}


#' working horse for \code{subset_indexes}
#' 
#' Get dataframe indexed matching logical expression.
#' 
#' @inheritParams subset2
#' @keywords internal
#' 
subset_indexes <- function(x, subset=.())
{ 
  n <- nrow(x)
  ss <- as.quoted(subset)
  if (length(ss) == 0)                      # return all indexed if no subset is supplied
    return(seq_len(n))
  if (length(ss) > 1) 
    stop("Only one logical condition can be specified. Do not use commas inside .()")
  
  stop_if_not_all_subset_vars_in_df(x, ss)  # check if all variables exist in dataframe
  ii <- eval.quoted(ss, envir=x, try=TRUE)  # if FALSE throws an error if variables undefined defined
  which2(ii)                                # return indexes of matching rows or integer(0)
}


#' Subsetting with some special features
#' 
#' @param x A dataframe.
#' @param subset Logical condition to select subset. Either as a string or using
#'   the \code{\link{plyr::.}} function.
#' @export
#' @examples
#' subset2(mtcars, .(mpg > 20))
#' subset2(mtcars, "mpg > 20 & gear == 4") 
#' 
subset2 <- function(x, subset=.(), i=NULL)
{
  ii <- subset_indexes(x, subset=subset)
  x[ii, , drop=FALSE]
}


