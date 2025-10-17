#' @title Single-iteration permutation phenotype and covariate data
#'
#' @description A sample-by-variable data frame with IDs,
#' covariates, and phenotypes as column names.
#'
#' @format A data frame with 10,000 rows and 511 variables:
#' \describe{
#'   \item{FID}{Family ID.}
#'   \item{IID}{Individual ID.}
#'   \item{ANCESTRY}{Sample ancestry.}
#'   \item{SEX}{Sample biological sex.}
#'   \item{AGE}{Sample age.}
#'   \item{BATCH}{Genotyping batch.}
#'   \item{STUDY}{Study contributing sample data.}
#'   \item{PCs}{4 genetic principal components.}
#'   \item{PHENOs}{500 simulated phenotypes.}
#' }
#' @source <https://doi.org/10.21203/rs.3.rs-873449/v1>
"sip_exampleData"

#' @title Single-iteration paired permutation phenotype and covariate data
#'
#' @description A sample-by-variable data frame with IDs,
#' covariates, and phenotypes as column names.
#'
#' @format A data frame with 10,000 rows and 309 variables:
#' \describe{
#'   \item{FID}{Family ID.}
#'   \item{IID}{Individual ID.}
#'   \item{BATCH}{Genotyping batch.}
#'   \item{SEX}{Sample biological sex.}
#'   \item{AGE}{Sample age.}
#'   \item{STUDY}{Study contributing sample data.}
#'   \item{PCs}{4 genetic principal components.}
#'   \item{PHENOs}{300 simulated phenotypes.}
#' }
#' @source <https://doi.org/10.21203/rs.3.rs-873449/v1>
"sipPair_exampleData"

#' @title Single-iteration paired permutation relatedness data
#'
#' @description A sample-by-variable data frame with IDs
#' and identity-by-descent data between samples.
#'
#' @format A data frame with 9886 rows and 4 variables:
#' \describe{
#'   \item{FID}{Family ID.}
#'   \item{IID1}{Individual ID for one sample.}
#'   \item{IID2}{Iindividual ID for a second sample.}
#'   \item{PropIBD}{Identity-by-descent proportion of genome shared
#'   between the two samples.}
#' }
#' @source <https://doi.org/10.21203/rs.3.rs-873449/v1>
"sipPair_relatednessData"
