#' Title read data "african_crises(1).csv"
#'
#' @param data: data.frame
#'
#' @return lire_data: data of african_crises(1).csv file
#'         s: summary
#' @export
#'
#' @examples
readdata <- function(data){
  lire_data <- head(data)
  s <- summary(data)
  return(list(lire_data,s))
}
