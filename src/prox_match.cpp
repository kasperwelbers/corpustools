#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

bool is_match(int x, IntegerVector Y, IntegerVector window) {
  int windowsize = window.size();
  int leftw = min(window);
  int rightw = max(window);

  for (int i = 0; i < Y.size(); i++) {
    for (int w = leftw; w <= rightw; w++){
      if (Y[i] == (w+x)) return true;
    }
  }
  return false;
}

IntegerVector get_matches(int x, IntegerVector Y, IntegerVector window) {
  return match(x + window, Y);
}

Rcpp::DataFrame proximity_match(List X, List Y, IntegerVector window) {
  int leftw = min(window);
  int rightw = max(window);

  CharacterVector Xnames = X.names();
  CharacterVector Ynames = Y.names();
  int n = Xnames.size();
  std::vector<std::string> doc_ids;
  std::vector<int> token_i;
  IntegerVector match_i = Rcpp::match(Xnames, Ynames);

  for (int i = 0; i < n; i++) {
    int y_i = match_i[i];
    if (IntegerVector::is_na(y_i)) continue;

    std::string doc_id = Rcpp::as<std::string>(Xnames[i]);
    IntegerVector Xtokens = X[i];
    IntegerVector Ytokens = Y[y_i-1];
    int ntokens = Xtokens.size();
    for (int j = 0; j < ntokens; j++) {
      if (is_match(Xtokens[j], Ytokens, window)) {
        doc_ids.push_back(doc_id);
        token_i.push_back(Xtokens[j]);
      }
    }
  }
  return Rcpp::DataFrame::create(Rcpp::Named("doc_id") = doc_ids,
                                 Rcpp::Named("token_i") = token_i);
}


