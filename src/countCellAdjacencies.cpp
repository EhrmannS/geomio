#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

//' Count cell adjacencies in a matrix (c++)
//'
//' C++ function that counts the number of unique values in a matrix
//' @param mat [matrix(numeric)][matrix]\cr the object in which to count
//'   adjacencies
//' @param doublecount [logical(1)][logical]\cr whether or not to tally up each
//'   adjacency for both neighbouring cells, or only once.
//' @family count functions
//' @return A data.frame of values*values with their adjacencies
//' @export
// [[Rcpp::export]]
NumericMatrix countCellAdjacenciesCpp(NumericMatrix &mat, bool doublecount) {
   int mRows = mat.nrow(), mCols = mat.ncol(), elements;

   IntegerVector values, theValue, rightValue, leftValue, topValue, bottomValue, position;
   int posFocal, posRight, posLeft, posBottom;
   values = sort_unique(na_omit(mat));
   elements = values.size();
   NumericMatrix out(elements, elements);

   for(int y = 0; y < mRows; y++){
     for(int x = 0; x < mCols; x++){

       theValue = mat(y, x);
       if(mat(y, x) != mat(y, x)){
         continue;
       }
       posFocal = match(theValue, values)[0];

       if(x != mCols-1){
         rightValue = mat(y, x+1);
         if(!any(is_na(rightValue)).is_true()){
           posRight = match(rightValue, values)[0];
           out(posFocal-1, posRight-1) += 1;
         }
       }
       if(y != mRows-1){
         bottomValue = mat(y+1, x);
         if(!any(is_na(bottomValue)).is_true()){
           posBottom = match(bottomValue, values)[0];
           out(posFocal-1, posBottom-1) += 1;
         }
       }

       if(doublecount){
         if(x != 0){
           leftValue = mat(y, x-1);
           if(!any(is_na(leftValue)).is_true()){
             posLeft = match(leftValue, values)[0];
             out(posFocal-1, posLeft-1) += 1;
           }
         }
         if(y != 0){
           topValue = mat(y-1, x);
           if(!any(is_na(topValue)).is_true()){
             posBottom = match(topValue, values)[0];
             out(posFocal-1, posBottom-1) += 1;
           }
         }
       }

     }
   }

   return(out);
 }