#include <RcppArmadillo.h>
// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <stdlib.h>
using namespace Rcpp;

double relDist(const arma::vec& i, const arma::vec& j) {
  const int lim = i.n_elem;
  double cnt = 0;
  for (int idx = 0; idx < lim; idx++) cnt += ((i[idx] - j[idx]) * (i[idx] - j[idx]));
  return cnt;
}

double dist(const arma::vec& i, const arma::vec& j) {
  return sqrt(relDist(i,j));
}

// Stolen directly from Erik B's annoylib.h
double cosDist(const arma::vec& i, const arma::vec& j) {
  int lim = i.n_elem;
  double pp = 0, qq = 0, pq = 0;
  for (int z = 0; z < lim; z++) {
    pp += (i[z]) * (i[z]);
    qq += (j[z]) * (j[z]);
    pq += (i[z]) * (j[z]);
  }
  double ppqq = pp * qq;
  if (ppqq > 0) return 2.0 - 2.0 * pq / sqrt(ppqq);
  else return 2.0; // cos is 0
}

double sparseDist(const arma::sp_mat& i, const arma::sp_mat& j) {
  return arma::as_scalar(sqrt(sum(square(i - j))));
}

double sparseCosDist(const arma::sp_mat& i, const arma::sp_mat& j) {
  return 1 - (arma::as_scalar((dot(i,j)) / arma::as_scalar(arma::norm(i,2) * arma::norm(j,2))));
}
