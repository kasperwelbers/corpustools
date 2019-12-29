#include <Rcpp.h>
#include <progress.hpp>
#include <progress_bar.hpp>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]


int terms_i_binsearch(std::vector<double> n, const double& value) {
  // assumes list is named and has unique values

  if (n.size() == 0) Rcpp::stop("2");

  std::vector<double>::iterator low;
  low = std::lower_bound(n.begin(), n.end(), value);

  int i = low - n.begin();

  if ((unsigned)i >= n.size())
    return(-1);
  if (n[i] == value)
    return(i);
  return(-1);
}

void match_dictionary(NumericVector& code_vec, NumericVector& hit_id, int hit_id_counter, NumericVector& f, std::vector<int>& context, int i, List dict) {
  double feat;
  int li;
  int nterms = 0;
  int code = -1;

  for (int j=0; j < 100; j++) {
    if (dict.containsElementNamed("code"))
      code = as<int>(dict["code"]);

    if ((i+j) >= int(f.size())) break;
    feat = f[i+j];

    if (j > 0) {
      if (context[i+j] != context[i+j-1]) break;
    }

    if (dict.containsElementNamed("terms")){
      std::vector<double> test = dict["terms_i"];
      li = terms_i_binsearch(dict["terms_i"], feat);
      if (li < 0) break;

      dict = dict["terms"];
      dict = dict[li];

    } else {
      break;
    }
    nterms++;
  }

  if (code >= 0) {
    for (int code_i = 0; code_i < nterms; code_i++) {
      code_vec[(i+code_i)] = code;
      hit_id[(i+code_i)] = hit_id_counter;
    }
  }
}

// [[Rcpp::export]]
DataFrame do_code_dictionary(NumericVector feature, std::vector<int> context, NumericVector which, List dict, bool verbose) {
  // note that feature is a numeric vector with factor levels.
  NumericVector dict_i(feature.size());
  NumericVector hit_id(feature.size());

  Progress p(which.size(), verbose);

  int hit_id_counter = 1;
  for(NumericVector::iterator i = which.begin(); i != which.end(); ++i) {
    if (dict_i[(*i-1)] > 0) continue;

    match_dictionary(dict_i, hit_id, hit_id_counter, feature, context, (*i-1), dict);
    if (dict_i[(*i-1)] > 0) hit_id_counter++;

    if (Progress::check_abort())
      stop("Aborted");

    p.increment(1);
  }
  DataFrame df = DataFrame::create(Named("hit_id") = hit_id,
                                   Named("dict_i") = dict_i);

  return df;
}

