#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

// TABLE FUNCTION

// [[Rcpp::export]]
std::map<String, int> count(SEXP &x) {
  CharacterVector v = as<CharacterVector>(x);

  std::map<String, int> t;
  int n = v.size();
  for (int i = 0; i < n; i++) {
    t[v[i]]++;
  }
  return t;
}

// get map keys
CharacterVector keys( std::map<String, int> &t) {
  CharacterVector k(t.size());
  int i = 0;
  for ( auto it = t.begin(); it != t.end(); ++it){
    k[i] = it->first;
    i++;
  }
  return k;
}

// get map values
IntegerVector values( std::map<String, int> &t) {
  IntegerVector v(t.size());
  int i = 0;
  for ( auto it = t.begin(); it != t.end(); ++it){
    v[i] = it->second;
    i++;
  }
  return v;
}


// GET UNIQUE SETS (used for getting hit_ids in proximity search)

// [[Rcpp::export]]
IntegerVector full_set_ids( SEXP x) {
  std::map<String, int> t = count(x);
  int nsets = min(values(t));

  CharacterVector v = as<CharacterVector>(x);
  int n = v.size();
  std::map<String, int> counter;
  IntegerVector out(n);
  for (int i = 0; i < n; i++) {
    counter[v[i]]++;
    if ( counter[v[i]] >= nsets ) {
      out[i] = nsets;
      // alternatively, output NA, so that values that do not form a full set are ignored (currently they are added to the last set)
    } else {
    out[i] = counter[v[i]];
    }
  }
  return out;
}


