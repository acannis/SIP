#' This function pairs individuals in the paired-permutation function
#' (sip_pair.R) into the most highly-related pairs possible.
#'
#' @param df,rel.df Data frames
#' @param id.var,ibd.var Strings
#' @param rid.vars Character vector
#' @returns Data frame

get_relPairs <- function(df, id.var = id.var, rel.df = rel.df,
                         rid.vars = rid.vars, ibd.var = ibd.var) {

  srel <- as.data.frame(subset(rel.df,
                               (rel.df[[rid.vars[1]]] %in% df[[id.var]])
                               & (rel.df[[rid.vars[2]]] %in% df[[id.var]])))
  srel <- srel[order(srel[[ibd.var]], decreasing = TRUE),]

  par <- data.frame(matrix(nrow=0, ncol=3))
  colnames(par) <- c(rid.vars[1], rid.vars[2], ibd.var)
  while(nrow(srel) > 0) {
    par <- rbind(par, srel[c(rid.vars[1], rid.vars[2], ibd.var)][1,])
    srel <- subset(srel, !(srel[[rid.vars[1]]] %in%
                             c(par[[rid.vars[1]]], par[[rid.vars[2]]]))
                   & !(srel[[rid.vars[2]]] %in%
                         c(par[[rid.vars[1]]], par[[rid.vars[2]]])))
  }

  return(par)
}
