#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

//' Replace character values with integer values (c++)
//'
//' C++ function that replaces character values with integer values in a matrix.
//' @param mat [matrix(character)][matrix]\cr the object in which to replaces
//'   character values.
//' @param replace [character(.)][character]\cr values to replace.
//' @param with [integerish(.)][integer]\cr values that replace the old values
//'   (must have same length as \code{replace}).
//' @return an integer matrix where values in \code{replace} have been replaced
//'   with values in \code{with}.
//' @family substitute functions
//' @export
// [[Rcpp::export]]
IntegerMatrix subChrIntCpp(CharacterMatrix &mat, CharacterVector &replace, IntegerVector &with){
  int mRows = mat.nrow(), mCols = mat.ncol(), posFocal;
  CharacterVector theValue;
  IntegerMatrix out(mRows, mCols);
  theValue = as<CharacterVector>(mat(0, 0));

  for(int x = 0; x < mCols; x++){
    for(int y = 0; y < mRows; y++){

      theValue = as<CharacterVector>(mat(y, x));
      if(!CharacterVector::is_na(theValue[0])){
        posFocal = match(theValue, replace)[0];
        out(y, x) = with[posFocal-1];
      } else{
        out(y, x) = NA_INTEGER;
      }
    }
  }

  return(out);
}
