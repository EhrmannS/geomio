#include <Rcpp.h>
using namespace Rcpp;

//' "Reduce" a list of matrices (c++)
//'
//' C++ function that uses a binary function that successively combines a list
//' of matrices.
//' @param lMat [list(matrix)][list]\cr list of matrices with equal dimensions.
//' @param f [function(1)][function]\cr binary function to combine the matrices.
//' @family matrix modify functions
//' @return a single matrix of the same dimensions as all the input matrices.
//' @export
// [[Rcpp::export]]
NumericMatrix matReduceCpp(List lMat, Function f) {
  int n =  lMat.size();
  NumericMatrix out = lMat[0], mat, mat2;
  NumericVector theValue;

  for(int i = 1; i < n; i++){
    mat = as<NumericMatrix>(lMat[i]);

    for(int x = 0; x < out.ncol(); x++){
      for(int y = 0; y < out.nrow(); y++){
        NumericVector toAdd;
        toAdd.push_back(out(y, x));
        toAdd.push_back(mat(y, x));

        if(is_true(all(is_na(toAdd)))){
          out(y, x) = NA_REAL;
        } else{
          theValue = f(na_omit(toAdd));
          out(y, x) = theValue[0];
        }
      }
    }
  }

  return(out);
}
