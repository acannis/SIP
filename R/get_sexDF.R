#' This function divides the sample-by-variable data frame into males
#' and females prior to permutation within sexes.
#'
#' @param df Data frame
#' @param sex.var String
#' @param sex.val String or integer
#' @returns Data frame

get_sexDF <- function(df = df, sex.var = sex.var, sex.val = sex.val) {
  sdf <- subset(df, df[[sex.var]] == sex.val)
  return(sdf)
}
