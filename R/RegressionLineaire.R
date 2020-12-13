#' Title linear_Model
#'
#' @param data: is a data.frame
#'
#' @return list(scaterrplot,lm,summary,anova,stepAIC)
#' @export
#'
#' @examples
RegressionLineaire <- function(data){

  systemic_crisis <- data$systemic_crisis
  exch_usd <- data$exch_usd
  domestic_debt_in_default <- data$domestic_debt_in_default
  sovereign_external_debt_default <- data$sovereign_external_debt_default
  gdp_weighted_default <- data$gdp_weighted_default
  inflation_annual_cpi <- data$inflation_annual_cpi
  independence <- data$independence
  currency_crises <- data$currency_crises
  inflation_crises <- data$inflation_crises
  banking_crisis <- data$banking_crisis

  # matrice de scatter plot
  scartter <- graphics::pairs(data.frame(systemic_crisis, exch_usd, domestic_debt_in_default,
                                         sovereign_external_debt_default, gdp_weighted_default,
                                         inflation_annual_cpi, independence,
                                         currency_crises, inflation_crises,
                                         banking_crisis), pch = 1, lower.panel=panel.smooth,
                              col = "blue", main = "Matrice de Scatter plot")
  # Regression lineaire
  ml <- stats::lm(systemic_crisis~exch_usd+domestic_debt_in_default+sovereign_external_debt_default
                  +gdp_weighted_default+inflation_annual_cpi+independence+
                    currency_crises+inflation_crises+banking_crisis, data)

  # test sur les paramètres
  resum <- base::summary(ml)
  # analyse des variances
  anov <- stats::anova(ml)

  # Sélection des variables : pas à pas de façon automatique :
  selec <-  MASS::stepAIC(lm(systemic_crisis~1, data),systemic_crisis~exch_usd+
                            domestic_debt_in_default+sovereign_external_debt_default+
                            gdp_weighted_default+inflation_annual_cpi+independence+
                            currency_crises+inflation_crises+banking_crisis, direction="both")
  sortie <- base::list(scatterplot = scartter, regression_lineaire = ml, resume = resum,
                       anova = anov, selection_variable = selec)
  return(sortie)


}
