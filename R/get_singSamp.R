#' This function checks for uneven numbers of individuals within relatedness
#' category and creates a data set for permuting single samples that will not
#' be pair permuted (sip_pair.R).
#'
#' @param df Data frame
#' @param deglst List of data frames
#' @param id.var String
#' @returns Data frame

get_singSamp <- function(df, deglst, id.var = id.var) {
  pex <- subset(df, !(df[[id.var]] %in% c(
    unlist(lapply(deglst, "[[", 1)),
    unlist(lapply(deglst, "[[", 2))
  )))

  if (nrow(pex) == 1) {
    deglst[[length(deglst)]] <-
      deglst[[length(deglst)]][-nrow(deglst[[length(deglst)]]), ]
  }

  if (nrow(deglst[[length(deglst)]]) == 1) {
    deglst[[length(deglst)]] <- NULL
  }

  pex <- subset(df, !(df[[id.var]] %in% c(
    unlist(lapply(deglst, "[[", 1)),
    unlist(lapply(deglst, "[[", 2))
  )))
  return(pex)
}
