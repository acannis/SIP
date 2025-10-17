#' This function samples indices for permuting phenotype vectors in
#' the basic permutation function (sip.R) and for permuting leftover
#' unpaired individuals in the paired-permutation function (sipPair.R).
#'
#' @param df Data frame
#' @param seed Number
#' @returns Numeric vector

get_permIdx <- function(df, seed = seed) {

  # save the current RNG state #
  og_seed <- .Random.seed

  # set on.exit() #
  on.exit({
    .Random.seed <<- og_seed
  })

  # set seed and permute #
  set.seed(seed)
  pidx <- sample(seq(nrow(df)), nrow(df), replace = FALSE)
  return(pidx)
}
