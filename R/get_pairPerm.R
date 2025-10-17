#' This function samples individuals by pairs for permuting phenotype
#' vectors in the paired-permutation function (sipPair.R)
#'
#' @param deglst List of data frames
#' @param rid.vars Character vector
#' @param ibd.var String
#' @param seed Number
#' @returns Data frame

get_pairPerm <- function(deglst, rid.vars = rid.vars, ibd.var = ibd.var,
                         seed = seed) {

  deglst <- deglst[!sapply(deglst, is.null)]

  didx <- list()
  for (i in seq(length(deglst))) {

    didx[[i]] <- data.frame(pairIdx=sample(seq(nrow(deglst[[i]])),
                                           nrow(deglst[[i]]), replace = FALSE),
                            sampIdx1=sample(1:2, nrow(deglst[[i]]),
                                            replace = TRUE))
    didx[[i]]$sampIdx2 <- ifelse(didx[[i]]$sampIdx1==1, 2, 1)
  }

  rmtch <- data.frame(fixid=character(), permid=character())
  for (i in seq(length(deglst))) {

    mtch <- rbind(cbind(data.frame(fixid=deglst[[i]][[rid.vars[1]]],
                                   sampIdx=didx[[i]]$sampIdx1),
                        deglst[[i]][didx[[i]]$pairIdx,]),
                  cbind(data.frame(fixid=deglst[[i]][[rid.vars[2]]],
                                   sampIdx=didx[[i]]$sampIdx2),
                        deglst[[i]][didx[[i]]$pairIdx,]))

    mtch$permid <- ifelse(mtch$sampIdx == 1, mtch[[rid.vars[1]]],
                          mtch[[rid.vars[2]]])
    mtch <- mtch[,c("fixid","permid")]

    rmtch <- rbind(rmtch, mtch)
  }

  return(rmtch)
}
