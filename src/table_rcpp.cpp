#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector table_rcpp(CharacterVector x) {
    return table(x);
}
