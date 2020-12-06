#' Title correlation
#'
#' @param data:data.frame
#'
#' @return p: scatterplot
#'        c: corrplot
#' @export
#'
#' @examples
correlation <- function(data){
  dtfc <- data[4:14]
  p <- pairs(dtfc)
  corr <- cor(dtfc,method="pearson")
  c <- corrplot(corr, type="upper", order="hclust", tl.col="black", tl.srt=45)
}
