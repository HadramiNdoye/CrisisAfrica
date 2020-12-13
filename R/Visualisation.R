#' Title Visualisation
#'
#' @param d1 : data.frame
#'
#' @return g1,g2,g3,w1,w2,w3,q1: plots
#' @export
#'
#' @examples
Visualisation <- function(d1){
  tab1 <- table(d1$country)
  dftab1 <- data.frame(tab1)
  Pays <- dftab1$Var1
  g1 <- ggplot(data = dftab1) +
    geom_bar(stat="identity",mapping = aes(x = Var1,y=Freq,fill=Pays)) +
    ggtitle("Diagramme en bâton représentant les pays") + xlab("Pays")
  g1 + theme (plot.title = element_text(size=11,face="bold",hjust = 0.5))

  # visualisation total of systemic_crisis
  tabcs <- table(d1$systemic_crisis)
  dftabcs <- data.frame(tabcs)
  crisis_syst<- dftabcs$Var1
  g2 <- ggplot(data = dftabcs) +
    geom_bar(stat="identity",mapping = aes(x = Var1,y = Freq,fill = crisis_syst)) +
    ggtitle("Diagramme en bâton représentant la crise systemique") + xlab("crise systemique")
  g2 + theme (plot.title = element_text(size=11,face="bold",hjust = 0.5))

  # Visualisation of systemic_crisis
  tab2<-table(d1$country,d1$banking_crisis)
  dftab2 <- data.frame(tab2)
  crise_systemique <- dftab2$Var2
  g3 <- ggplot(data = dftab2) +
    geom_bar(stat="identity",mapping = aes(x = Var1,y=Freq,fill=crise_systemique)) +
    ggtitle("Crise systemique en fonction des pays") + xlab("Pays")
  g3 + theme (plot.title = element_text(size=11,face="bold",hjust = 0.5))

  #on remarque La dette totale en défaut vis-à-vis du PIB est bcp plus entre les années 1950 et 2000
  w1 <- with(d1, qplot(year, gdp_weighted_default))+
    ggtitle("La dette totale en défaut vis-à-vis du PIB en fonction des années") +
    theme (plot.title = element_text(size=11,face="bold",hjust = 0.5))

  w2 <- with(d1, qplot(year, inflation_annual_cpi))+
    ggtitle("Le taux d'inflation annuel de l'IPC en fonction des années") +
    theme (plot.title = element_text(size=11,face="bold",hjust = 0.5))

  # Le taux de change du pays vis-à-vis de l'USD augmente à partire des anné 40
  w3 <- with(d1, qplot(year, exch_usd))+
    ggtitle("Le taux de change des pays vis-à-vis de l'USD en fonction des années") +
    theme (plot.title = element_text(size=11,face="bold",hjust = 0.5))

  q1 <- qplot(data = d1,x=year, y=exch_usd,geom = "line") +
    facet_wrap(~ country)+
    ggtitle("Évolution du taux de change des pays vis-à-vis de l'USD en fonction de chaque pays") +
    theme (plot.title = element_text(size=11,face="bold",hjust = 0.5))

  return (list(g1,g2,g3,w1,w2,w3,q1))
}
