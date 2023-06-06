#include <Rcpp.h>
using namespace Rcpp;

//' @name u_index
//' @title Compute an index of community
//' @author P. Chevallier - April 2023
//' @param nz length of the concatenated time-series
//' @param yd initial vector of datetimes (in sec)
//' @details the function compute an index, which the number of apparition of the same datetime
//' in a time-series
//' @return vector of indexes

// [[Rcpp::export]]
IntegerVector u_index(int nz, IntegerVector zd) {
  IntegerVector ze = zd;
  IntegerVector zi(nz);

  for (int i = 0; i < nz; ++i){
    for (int j = 0; j < nz; ++j) {
      if (ze[j] == zd[i]) zi[i] += 1;
    }
  }
  return zi;
}

