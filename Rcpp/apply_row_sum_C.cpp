#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]

NumericVector apply_row_sum_C(NumericMatrix x){
  int n=x.nrow();
  NumericVector output(n);
  
  for(int i=0; i<n; i++){
      output[i]=sum(x(i, _));
  }
  return(output);
}