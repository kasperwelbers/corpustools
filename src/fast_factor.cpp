#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE>
IntegerVector fast_factor_template( const Vector<RTYPE>& x, Vector<RTYPE> levs) {
  if (levs.size() == 0) {
    levs = sort_unique(x);
  }
  Vector<RTYPE> levs_nona = na_omit(levs);
  IntegerVector out = match(x, levs_nona);
  out.attr("levels") = as<CharacterVector>(levs_nona);
  out.attr("class") = "factor";
  return out;
}

// [[Rcpp::export]]
SEXP fast_factor_cpp( SEXP x, SEXP levs) {
  switch( TYPEOF(x) ) {
  case INTSXP: return fast_factor_template<INTSXP>(x, levs);
  case REALSXP: return fast_factor_template<REALSXP>(x, levs);
  case STRSXP: return fast_factor_template<STRSXP>(x, levs);
  }
  return R_NilValue;
}
