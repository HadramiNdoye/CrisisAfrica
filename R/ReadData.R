#' Title read data "african_crises.csv"
#'
#' @param data: is data.frame
#'
#' @return lire_data: data of african_crises.csv file
#'         s: summary
#' @export
#'
#' @examples
ReadData <- function(data){
  lire_data <- head(data)
  s <- summary(data)
  return(list(lire_data,s))
}
