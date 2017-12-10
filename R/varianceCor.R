#'Variance Correlation between X (independent) and Y (dependent) variables
#'@param X The independent variable
#'@param Y The dependent variable
#'@return The amount of variance in Y which can be explained by X
#'@note This is *not* a causal relationship, just a statement of variance correlation
#'@export
#'

"VarianceCor" <- function( x_dep, y_indep) {
  
  return( (cor.test(x_dep,y_indep,method="pearson")$estimate)^2 )
}