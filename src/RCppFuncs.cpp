#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/

// For more on using Rcpp click the Help button on the editor toolbar
//
// setwd(<project))
//   ..within package directory
// compileAttributes(pkgdir = ".", verbose = getOption("verbose"))
// build()
// install()
//
// [[Rcpp::export]]
NumericVector computeKAMA(NumericVector alpha, NumericVector alphaComplement, NumericVector KAMA, NumericVector price) {
  int n = price.size();
  for(int i=1; i<n; i++) {
    KAMA[i] = alpha[i]*price[i] + alphaComplement[i]*KAMA[i-1];
  }
  return KAMA;
}

// [[Rcpp::export]]
NumericVector computeFRAMA(NumericVector alpha, NumericVector alphaComplement, NumericVector FRAMA, NumericVector price) {
  int n = price.size();
  for(int i=1; i<n; i++) {
    FRAMA[i] = alpha[i]*price[i] + alphaComplement[i]*FRAMA[i-1];
  }
  return FRAMA;
}

// [[Rcpp::export]]
NumericVector computeItrend(NumericVector price, NumericVector iTrend, NumericVector alpha) {
  int n=price.size();
  for(int i=6; i<n; i++) {
    iTrend[i] = (alpha[i]-alpha[i]*alpha[i]/4)*price[i] + .5*alpha[i]*alpha[i]*price[i-1] - 
      (alpha[i]-.75*alpha[i]*alpha[i])*price[i-1] + 2*(1-alpha[i])*iTrend[i-1] -
      (1-alpha[i])*(1-alpha[i])*iTrend[i-2];
  }
  return iTrend;
}

// [[Rcpp::export]]
NumericVector computeVIDA(NumericVector k, NumericVector price, NumericVector filt, NumericVector consts){
  int n = price.size();
  for(int i=1; i<n; i++) {
    filt[i] = consts[i]*k[i]*price[i]+(1-consts[i]*k[i])*filt[i-1];
  }
  return filt;
}
// [[Rcpp::export]]
NumericVector Ver(NumericVector x) {
  return x;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//


/*** R
cat 'FinMarkets ver. '
Ver(1)
*/
