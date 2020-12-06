#' Title: create function cleandata
#'
#' @param data:is a data frame
#'
#' @return data
#' @export
#'
#' @examples
cleandata <- function(data){
  data$banking_crisis[data$banking_crisis=="no_crisis"] <- 0
  data$banking_crisis[data$banking_crisis=="crisis"] <- 1
  data$banking_crisis <- as.numeric(data$banking_crisis)
  return(data)
}
