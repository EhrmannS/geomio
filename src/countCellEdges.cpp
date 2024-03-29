#include <Rcpp.h>
using namespace Rcpp;

//' Count cell edges in a matrix (c++)
//'
//' C++ function that counts the number of edges in a matrix
//' @param mat [matrix(numeric)][matrix]\cr the object in which to count edges
//' @family count functions
//' @return A data.frame of the unique values and their edges
//' @export
// [[Rcpp::export]]
DataFrame countCellEdgesCpp(NumericMatrix &mat) {
   int mRows = mat.nrow(), mCols = mat.ncol(), elements;

   IntegerVector values, theValue, position;
   values = sort_unique(mat);
   elements = values.size();

   IntegerVector edgesX(elements), edgesY(elements);

   for(int y = 0; y < mRows; y++){
     for(int x = 0; x < mCols; x++){

       theValue = mat(y, x);
       if(!any(mat(y, x) == values).is_true()){
         continue;
       }

       int position = match(theValue, values)[0];

       // making use of the fact that NaN values are not equal to themselves, hence filter out boundaries with NA.
       if(mat(y, x+1) == mat(y, x+1)){
         if(any(theValue != mat(y, x+1)).is_true()){
           if(x != mCols-1){
             edgesX[position-1] += 1;
           }
         }
       }

       if(mat(y+1, x) == mat(y+1, x)){
         if(any(theValue != mat(y+1, x)).is_true()){
           if(y != mRows-1){
             edgesY[position-1] += 1;
           }
         }
       }

       if(mat(y, x-1) == mat(y, x-1)){
         if(any(theValue != mat(y, x-1)).is_true()){
           if(x != 0){
             edgesX[position-1] += 1;
           }
         }
       }

       if(mat(y-1, x) == mat(y-1, x)){
         if(any(theValue != mat(y-1, x)).is_true()){
           if(y != 0){
             edgesY[position-1] += 1;
           }
         }
       }

     }
   }

   DataFrame out = DataFrame::create(Named("value")=values,
                                     Named("edgesX")=edgesX,
                                     Named("edgesY")=edgesY);

   return(out);
 }