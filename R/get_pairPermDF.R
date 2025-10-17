#' This function recombines the fixed data and permuted data
#' in the paired-permutation function (sipPair.R) into a permuted
#' sample-by-variable data frame.
#'
#' @param fix.df,perm.df,perm.map Data frames
#' @param id.var String
#' @returns Data frame

get_pairPermDF <- function(fix.df, perm.df, perm.map, id.var = id.var) {
  pdf1 <- fix.df[match(perm.map$fixid, fix.df[[id.var]]),]
  pdf2 <- perm.df[match(perm.map$permid, perm.df[[id.var]]),]
  pdf2[[id.var]] <- NULL
  pdf <- cbind(pdf1, pdf2)
  return(pdf)
}
