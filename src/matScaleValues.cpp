#include <Rcpp.h>
using namespace Rcpp;

//' ... (c++)
//'
//' C++ function that ...
//' @param mat [matrix(numeric)][matrix]\cr ...
//' @param range [numeric(.)][numeric]\cr ...
//' @family matrix modify functions
//' @return ...
//' @export
// [[Rcpp::export]]
NumericMatrix scaleMatrixCpp(NumericMatrix mat, NumericVector range){
  int mRows = mat.nrow(), mCols = mat.ncol();
  NumericMatrix out = clone(mat);
  double minVal = min(mat);
  double maxVal = max(mat);

  if(minVal == maxVal){
    return(out);
  }

  for(int y = 0; y < mRows; y++){
    for(int x = 0; x < mCols; x++){
      out(y, x) = (mat(y, x) - minVal) * (range[1] - range[0]) / (maxVal - minVal) + range[0];
    }
  }

  return(out);
}