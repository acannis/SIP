#' This function recombines the fixed data and permuted data
#' into a permuted sample-by-variable data frame.
#'
#' @param fix.df,perm.df Data frames
#' @param perm.idx Numeric vector
#' @returns Data frame

get_permDF <- function(fix.df, perm.df, perm.idx) {
  pdf <- cbind(fix.df,perm.df[perm.idx,])
  return(pdf)
}
