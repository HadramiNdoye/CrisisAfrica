#' Title correlation
#'
#' @param data: is a data.frame
#'
#' @return c: corrplot
#' @export
#'
#' @examples
Correlation <- function(data){
  dtfc <- data[4:14]
  corr <- cor(dtfc,method="pearson")
  c <- corrplot(corr, type="upper", order="hclust", tl.col="black", tl.srt=45,
                title="Matrice de corrélation (corrélogramme)", mar=c(0,0,5,0),
                tl.offset = 1)
  return(c)

}
