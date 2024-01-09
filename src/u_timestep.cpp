#include <chrono>
#include <thread>
#include <Rcpp.h>
using namespace Rcpp;

//' @name u_timestep
//' @title Compute values in a time-series with a fixed timestep
//' @author P. Chevallier - June / Oct 2023
//' @param te time end (in sec)
//' @param yd initial vector of datetimes (in sec)
//' @param yv initial vector of values
//' @param tst timestep (in mn)
//' @param iop operation index
//' @details iop = 1 for sum; 0 for mean; -2 for min and +2 for max
//' @return vector of values with fixed timestep

//[[Rcpp::depends(Rcpp)]]
// [[Rcpp::export]]
NumericVector u_timestep (double te, NumericVector yd, NumericVector yv, int tst, int iop) {
	Rcout.precision(2);
  int m = yd.size();
  int n = te/(tst*60);

  NumericVector xd(n), l(n);
  NumericVector xv(n);
  NumericVector yyv(m);

  for(int k=0; k<n; ++k) xd[k] = k*tst*60;
  xv[0]=NA_REAL;

  for(int k=1; k<n; ++k) {
    l[k]=0;
    for(int i=0; i<m; ++i) if((yd[i] > xd[k-1]) && (yd[i] <= xd[k])) {
      l[k]=l[k]+1;
      yyv[i] = yv[i];
    }
    else yyv[i] = NA_REAL;

    if(l[k]==0) xv[k]=NA_REAL;
    else {
      NumericVector v = yyv[! is_na(yyv)];
      if(iop==1)  xv[k]=sum(v);
      if(iop==0)  xv[k]=mean(v);
      if(iop==-2) xv[k]=min(v);
      if(iop==+2) xv[k]=max(v);
    }

    if(k == n - 1) {
    	Rcout << "100 %" << std::endl;
    } else if(k % 100 == 0) {
    	Rcout << "                 ";
    	Rcout << "\r";
    	Rcout << static_cast<double>(k+1) / static_cast<double>(n) * 100.0 << "%";
    	Rcout << "\r";
    }

  }
  return xv;
}
