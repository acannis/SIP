#' This function subsets the sample-by-variable data frame
#' to the permutable data.
#' This subset should include phenotypes and phenotypic covariates.
#'
#' @param df Data frame
#' @param id.var String
#' @param pheno.vars Character vector
#' @returns Data frame

get_permute <- function(df, id.var = id.var, pheno.vars = pheno.vars) {
  pp <- as.data.frame(df)[c(id.var,pheno.vars)]
  return(pp)
}
