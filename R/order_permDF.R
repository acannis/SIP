#' This function reorders the permuted sample-by-variable data frame
#' to match the ID order of the primary sample-by-variable data frame.
#'
#' @param df,perm Data frames
#' @param id.var String
#' @returns Data frame

order_permDF <- function(df = df, perm = perm, id.var = id.var, return.perm.pairs = return.perm.pairs) {
  if (return.perm.pairs == TRUE) {
    perm <- perm[, c(colnames(df),"fixed_data_id","permuted_data_id")]
  } else {
    perm <- perm[, colnames(df)]
  }
  perm <- perm[match(df[[id.var]], perm[[id.var]]), ]
  return(perm)
}
