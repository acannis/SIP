#' This function subsets the sample-by-variable dataframe
#' to the "fixed" (i.e., non-permuted) data.
#' This subset should include ID variables, sex, and genotypic covariates.
#'
#' @param df Data frame
#' @param id.var,sex.var String
#' @param geno.vars Character vector
#' @returns Data frame

get_fixed <- function(df, id.var = id.var, sex.var = sex.var,
                       geno.vars = geno.vars) {
  if (unique(is.na(geno.vars))) {
    fix <- as.data.frame(df)[c(id.var,sex.var)]
  } else {
    fix <- as.data.frame(df)[c(id.var,sex.var,geno.vars)]
  }
  return(fix)
}
