#' Predefined Stock Metadata Map
#'
#' A predefined list containing metadata about various fish stocks. The list includes
#' information such as `code`, `months`, `strata`, `spec_name`, and `stock_name`.
#'
#' @format A named list with elements for each stock:
#' \describe{
#'   \item{code}{Numeric code describing the stock.}
#'   \item{months}{Vector of relevant months.}
#'   \item{strata}{Strata for the stock, either numeric or character.}
#'   \item{spec_name}{Species name.}
#'   \item{stock_name}{Stock name, often combining geographic and biological information.}
#' }
#' @examples
#' # Access the stock map
#' stock_map
"stock_map"