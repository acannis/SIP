#' This function infers relatedness categories in the paired-permutation
#' function (sip_pair.R) prior to permutation.
#'
#' @param pairs Data frame
#' @param rid.vars Character vector
#' @param ibd.var String
#' @returns List of data frames

get_relCat <- function(pairs, rid.vars = rid.vars, ibd.var = ibd.var) {
  pairs$degree <- ifelse(pairs[[ibd.var]] > 1 / (2^0.5), "MZ",
    ifelse(pairs[[ibd.var]] <= 1 / (2^0.5) &
      pairs[[ibd.var]] > 1 / (2^1.5), "1st",
    ifelse(pairs[[ibd.var]] <= 1 / (2^1.5) &
      pairs[[ibd.var]] > 1 / (2^2.5), "2nd",
    ifelse(pairs[[ibd.var]] <= 1 / (2^2.5) &
      pairs[[ibd.var]] > 1 / (2^3.5), "3rd",
    ifelse(pairs[[ibd.var]] <= 1 / (2^3.5), "UN", NA)
    )
    )
    )
  )


  if (TRUE %in% is.na(pairs$degree)) {
    stop("Error: One of more pairs of individuals do not have a valid
         inferred degree of relatedness.")
  }

  deglst <- list()
  udeg <- unique(pairs$degree)

  for (i in seq(length(udeg))) {
    curr <- subset(pairs, pairs$degree == udeg[i])

    if (nrow(curr) > 1) {
      deglst[[i]] <- curr
    }
  }

  return(deglst)
}
