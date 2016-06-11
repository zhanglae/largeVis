// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// searchTrees
arma::imat searchTrees(const int& threshold, const int& n_trees, const int& K, const int& max_recursion_degree, const int& maxIter, const arma::mat& data, const std::string& distMethod, bool verbose);
RcppExport SEXP largeVis_searchTrees(SEXP thresholdSEXP, SEXP n_treesSEXP, SEXP KSEXP, SEXP max_recursion_degreeSEXP, SEXP maxIterSEXP, SEXP dataSEXP, SEXP distMethodSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const int& >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< const int& >::type n_trees(n_treesSEXP);
    Rcpp::traits::input_parameter< const int& >::type K(KSEXP);
    Rcpp::traits::input_parameter< const int& >::type max_recursion_degree(max_recursion_degreeSEXP);
    Rcpp::traits::input_parameter< const int& >::type maxIter(maxIterSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type distMethod(distMethodSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    __result = Rcpp::wrap(searchTrees(threshold, n_trees, K, max_recursion_degree, maxIter, data, distMethod, verbose));
    return __result;
END_RCPP
}
// searchTreesCSparse
arma::mat searchTreesCSparse(const int& threshold, const int& n_trees, const int& K, const int& max_recursion_degree, const int& maxIter, const arma::uvec& i, const arma::uvec& p, const arma::vec& x, const std::string& distMethod, bool verbose);
RcppExport SEXP largeVis_searchTreesCSparse(SEXP thresholdSEXP, SEXP n_treesSEXP, SEXP KSEXP, SEXP max_recursion_degreeSEXP, SEXP maxIterSEXP, SEXP iSEXP, SEXP pSEXP, SEXP xSEXP, SEXP distMethodSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const int& >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< const int& >::type n_trees(n_treesSEXP);
    Rcpp::traits::input_parameter< const int& >::type K(KSEXP);
    Rcpp::traits::input_parameter< const int& >::type max_recursion_degree(max_recursion_degreeSEXP);
    Rcpp::traits::input_parameter< const int& >::type maxIter(maxIterSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type i(iSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type p(pSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type distMethod(distMethodSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    __result = Rcpp::wrap(searchTreesCSparse(threshold, n_trees, K, max_recursion_degree, maxIter, i, p, x, distMethod, verbose));
    return __result;
END_RCPP
}
// searchTreesTSparse
arma::mat searchTreesTSparse(const int& threshold, const int& n_trees, const int& K, const int& max_recursion_degree, const int& maxIter, const arma::uvec& i, const arma::uvec& j, const arma::vec& x, const std::string& distMethod, bool verbose);
RcppExport SEXP largeVis_searchTreesTSparse(SEXP thresholdSEXP, SEXP n_treesSEXP, SEXP KSEXP, SEXP max_recursion_degreeSEXP, SEXP maxIterSEXP, SEXP iSEXP, SEXP jSEXP, SEXP xSEXP, SEXP distMethodSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const int& >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< const int& >::type n_trees(n_treesSEXP);
    Rcpp::traits::input_parameter< const int& >::type K(KSEXP);
    Rcpp::traits::input_parameter< const int& >::type max_recursion_degree(max_recursion_degreeSEXP);
    Rcpp::traits::input_parameter< const int& >::type maxIter(maxIterSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type i(iSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type j(jSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type distMethod(distMethodSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    __result = Rcpp::wrap(searchTreesTSparse(threshold, n_trees, K, max_recursion_degree, maxIter, i, j, x, distMethod, verbose));
    return __result;
END_RCPP
}
// fastDistance
arma::vec fastDistance(const NumericVector is, const NumericVector js, const arma::mat& data, const std::string& distMethod, bool verbose);
RcppExport SEXP largeVis_fastDistance(SEXP isSEXP, SEXP jsSEXP, SEXP dataSEXP, SEXP distMethodSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const NumericVector >::type is(isSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type js(jsSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type distMethod(distMethodSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    __result = Rcpp::wrap(fastDistance(is, js, data, distMethod, verbose));
    return __result;
END_RCPP
}
// fastCDistance
arma::vec fastCDistance(const arma::vec& is, const arma::vec& js, const arma::uvec& i_locations, const arma::uvec& p_locations, const arma::vec& x, const std::string& distMethod, bool verbose);
RcppExport SEXP largeVis_fastCDistance(SEXP isSEXP, SEXP jsSEXP, SEXP i_locationsSEXP, SEXP p_locationsSEXP, SEXP xSEXP, SEXP distMethodSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const arma::vec& >::type is(isSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type js(jsSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type i_locations(i_locationsSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type p_locations(p_locationsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type distMethod(distMethodSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    __result = Rcpp::wrap(fastCDistance(is, js, i_locations, p_locations, x, distMethod, verbose));
    return __result;
END_RCPP
}
// fastSDistance
arma::vec fastSDistance(const arma::vec& is, const arma::vec& js, const arma::uvec& i_locations, const arma::uvec& j_locations, const arma::vec& x, const std::string& distMethod, bool verbose);
RcppExport SEXP largeVis_fastSDistance(SEXP isSEXP, SEXP jsSEXP, SEXP i_locationsSEXP, SEXP j_locationsSEXP, SEXP xSEXP, SEXP distMethodSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const arma::vec& >::type is(isSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type js(jsSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type i_locations(i_locationsSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type j_locations(j_locationsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type distMethod(distMethodSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    __result = Rcpp::wrap(fastSDistance(is, js, i_locations, j_locations, x, distMethod, verbose));
    return __result;
END_RCPP
}
// distMatrixTowij
arma::sp_mat distMatrixTowij(const NumericVector is, const NumericVector js, const NumericVector xs, const NumericVector sigmas, const int N, bool verbose);
RcppExport SEXP largeVis_distMatrixTowij(SEXP isSEXP, SEXP jsSEXP, SEXP xsSEXP, SEXP sigmasSEXP, SEXP NSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const NumericVector >::type is(isSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type js(jsSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type xs(xsSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type sigmas(sigmasSEXP);
    Rcpp::traits::input_parameter< const int >::type N(NSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    __result = Rcpp::wrap(distMatrixTowij(is, js, xs, sigmas, N, verbose));
    return __result;
END_RCPP
}
// sigFunc
double sigFunc(const double& sigma, const NumericVector& x_i, const double& perplexity);
RcppExport SEXP largeVis_sigFunc(SEXP sigmaSEXP, SEXP x_iSEXP, SEXP perplexitySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const double& >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type x_i(x_iSEXP);
    Rcpp::traits::input_parameter< const double& >::type perplexity(perplexitySEXP);
    __result = Rcpp::wrap(sigFunc(sigma, x_i, perplexity));
    return __result;
END_RCPP
}
// sgd
arma::mat sgd(arma::mat coords, arma::ivec& is, const IntegerVector js, const IntegerVector ps, const NumericVector ws, const double gamma, const double rho, const double minRho, const bool useWeights, const long nBatches, const int M, const double alpha, bool verbose);
RcppExport SEXP largeVis_sgd(SEXP coordsSEXP, SEXP isSEXP, SEXP jsSEXP, SEXP psSEXP, SEXP wsSEXP, SEXP gammaSEXP, SEXP rhoSEXP, SEXP minRhoSEXP, SEXP useWeightsSEXP, SEXP nBatchesSEXP, SEXP MSEXP, SEXP alphaSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::mat >::type coords(coordsSEXP);
    Rcpp::traits::input_parameter< arma::ivec& >::type is(isSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type js(jsSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type ps(psSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< const double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< const double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< const double >::type minRho(minRhoSEXP);
    Rcpp::traits::input_parameter< const bool >::type useWeights(useWeightsSEXP);
    Rcpp::traits::input_parameter< const long >::type nBatches(nBatchesSEXP);
    Rcpp::traits::input_parameter< const int >::type M(MSEXP);
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    __result = Rcpp::wrap(sgd(coords, is, js, ps, ws, gamma, rho, minRho, useWeights, nBatches, M, alpha, verbose));
    return __result;
END_RCPP
}
