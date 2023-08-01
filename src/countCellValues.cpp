#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

//' Count unique cell values in a matrix (c++)
//'
//' C++ function that counts the number of unique values in a matrix
//' @param mat [matrix(numeric)][matrix]\cr the object in which to count unique
//'   values
//' @family count functions
//' @return A data.frame of the unique values and their number
//' @export
// [[Rcpp::export]]
DataFrame countCellValuesCpp(NumericMatrix &mat) {
  int mRows = mat.nrow(), mCols = mat.ncol(), elements;

  IntegerVector values, theValue, position;
  values = sort_unique(mat);
  elements = values.size();

  IntegerVector cells(elements);

  for(int y = 0; y < mRows; y++){
    for(int x = 0; x < mCols; x++){

      theValue = mat(y, x);
      if(!any(mat(y, x) == values).is_true()){
        continue;
      }

      int position = match(theValue, values)[0];

      cells[position-1] += 1;

    }
  }

  DataFrame out = DataFrame::create(Named("value")=values,
                                    Named("cells")=cells);

  return(out);
}
