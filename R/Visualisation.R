#' Title Visualisation
#'
#' @param d1 : data.frame
#'
#' @return g1,w1,w2,w3,q1: plots
#' @export
#'
#' @examples
Visualisation <- function(d1){
  tab <- table(d1$country)
  dftab <- data.frame(tab)
  g1 <- ggplot(data = dftab) +
    geom_bar(stat="identity",mapping = aes(x = Var1,y=Freq,fill=Var1)) +
    ggtitle("Diagramme en bâton représentant les pays") + xlab("Pays")
  g1 + theme (plot.title = element_text(size=11,face="bold",hjust = 0.5))
  #on remarque La dette totale en défaut vis-à-vis du PIB est bcp plus entre les années 1950 et 2000
  w1 <- with(d1, qplot(year, gdp_weighted_default))
  w2 <- with(d1, qplot(year, inflation_annual_cpi))
  # Le taux de change du pays vis-à-vis de l'USD augmente à partire des anné 40
  w3 <- with(d1, qplot(year, exch_usd))
  q1 <- qplot(data = d1,x=year, y=exch_usd,geom = "line") +
    facet_wrap(~ country)
  return (list(g1,w1,w2,w3,q1))
}
