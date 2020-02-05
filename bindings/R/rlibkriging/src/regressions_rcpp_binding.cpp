#include <RcppArmadillo.h>

#include "libKriging/LinearRegression.hpp"

// [[Rcpp::export]]
Rcpp::List linear_regression(arma::vec y, arma::mat X) {
  LinearRegression rl;
  rl.fit(y, X);
  Rcpp::List obj;
  obj.attr("object") = rl;
  obj.attr("class") = "LinearRegression";
  return obj;
}


// [[Rcpp::export]]
Rcpp::List linear_regression_predict(Rcpp::List linearRegression, arma::mat X) {
  if (! linearRegression.inherits("LinearRegression")) Rcpp::stop("Input must be a LinearRegression object.");
  LinearRegression rl = linearRegression.attr("object");
  auto pred = rl.predict(X);
  return Rcpp::List::create(Rcpp::Named("y") = std::get<0>(pred),
                            Rcpp::Named("stderr") = std::get<1>(pred));
}


#include "libKriging/OrdinaryKriging.hpp"

// [[Rcpp::export]]
Rcpp::List ordinary_kriging(arma::vec y, arma::mat X,arma::vec theta) {
    OrdinaryKriging ok;
    auto ans = ok.fit(y, X, theta);
    return Rcpp::List::create(Rcpp::Named("class") = "OrdinaryKriging",
                              Rcpp::Named("gamma") = std::get<0>(ans),
                              Rcpp::Named("theta") = std::get<1>(ans));
}
