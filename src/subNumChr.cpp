#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

//' Replace numeric values with character values (c++)
//'
//' C++ function that replaces numeric values with character values in a matrix.
//' @param mat [matrix(numeric)][matrix]\cr the object in which to replaces
//'   numeric values.
//' @param replace [numeric(.)][numeric]\cr values to replace.
//' @param with [character(.)][character]\cr values that replace the old values
//'   (must have same length as \code{replace}).
//' @return an character matrix where values in \code{replace} have been
//'   replaced with values in \code{with}.
//' @family substitute functions
//' @export
// [[Rcpp::export]]
CharacterMatrix subNumChrCpp(NumericMatrix &mat, NumericVector &replace, CharacterVector with){
  int mRows = mat.nrow(), mCols = mat.ncol(), posFocal;
  NumericVector theValue;
  CharacterMatrix out(mRows, mCols);

  for(int x = 0; x < mCols; x++){
    for(int y = 0; y < mRows; y++){

      if(any(mat(y, x) == replace).is_true()){
        theValue = mat(y, x);
        posFocal = match(theValue, replace)[0];
        out(y, x) = with[posFocal-1];
      } else{
        out(y, x) = NA_STRING;
      }

    }
  }
  return(out);
}

