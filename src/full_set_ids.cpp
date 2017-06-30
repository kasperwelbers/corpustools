#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
IntegerVector full_set_ids( CharacterVector v, int nsets) {
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
