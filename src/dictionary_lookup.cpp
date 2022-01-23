#include <Rcpp.h>
#include <progress.hpp>
#include <progress_bar.hpp>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]


int terms_i_binsearch(std::vector<double> n, const double& value) {
  // assumes list is named and has unique values
  std::vector<double>::iterator low;
  low = std::lower_bound(n.begin(), n.end(), value);

  int i = low - n.begin();

  if ((unsigned)i >= n.size())
    return(-1);
  if (n[i] == value)
    return(i);
  return(-1);
}

void add_code(std::vector<int>& dict_i, std::vector<int>& hit_id, std::vector<int>& feat_i, std::vector<int>& nterms, int& hit_id_counter, int i, int count_terms, int code_i) {
  for (int pos_i = 0; pos_i < count_terms; pos_i++) {
    dict_i.push_back(code_i);
    hit_id.push_back(hit_id_counter);
    feat_i.push_back(i+pos_i+1);
    nterms.push_back(count_terms);
    
    if (dict_i.size() == dict_i.capacity()) {
      dict_i.reserve(dict_i.size()*2);
      hit_id.reserve(hit_id.size()*2);
      feat_i.reserve(feat_i.size()*2);
      nterms.reserve(nterms.size()*2);
    };
  }
  hit_id_counter++;
}

void match_dictionary(std::vector<int>& dict_i, std::vector<int>& hit_id, std::vector<int>& feat_i, std::vector<int>& nterms, int& hit_id_counter, NumericVector& f, std::vector<int>& context, std::vector<int>& token_id, int i, List dict) {
  double feat;
  int li;
  int count_terms = 0;

  for (int j=0; j < 100; j++) {   // 100 is max nr of words in dict term, which I think is fair
    if (dict.containsElementNamed("code")) {
      // the dictionary is a tree of terms. When the level of a dict contains 'code' it means on term (which can be multiple words) is finished.
      // (but we continue, because if there are also 'terms' at this level, it means there is another longer term)
      NumericVector code = as<NumericVector>(dict["code"]);
      for(NumericVector::iterator code_i = code.begin(); code_i != code.end(); ++code_i) 
        add_code(dict_i, hit_id, feat_i, nterms, hit_id_counter, i, count_terms, *code_i);
    }

    if ((i+j) >= int(f.size())) break;
    feat = f[i+j];

    if (j > 0) {
      if (context[i+j] != context[i+j-1]) break;
      if ((token_id[i+j] - token_id[i+j-1]) > 1) break;
    }

    if (dict.containsElementNamed("terms")){
      li = terms_i_binsearch(dict["terms_i"], feat);
      if (li < 0) break;

      dict = dict["terms"];
      dict = dict[li];

    } else {
      break;
    }
    count_terms++;
  }
}

// [[Rcpp::export]]
DataFrame do_code_dictionary(NumericVector feature, std::vector<int>& context, std::vector<int>& token_id, NumericVector which, List dict, int hit_id_offset, bool verbose) {
  // note that feature is a numeric vector with factor levels.
  std::vector<int> dict_i;
  std::vector<int> hit_id;
  std::vector<int> feat_i;
  std::vector<int> nterms;
  int startsize = which.size()*3;   // we don't know beforehand how many matches we'll find (note the capacity update in match_dictionary)
  dict_i.reserve(startsize);
  hit_id.reserve(startsize);
  feat_i.reserve(startsize);
  nterms.reserve(startsize);
  
  Progress p(which.size(), verbose);

  int hit_id_counter = hit_id_offset;
  for(NumericVector::iterator i = which.begin(); i != which.end(); ++i) {
    match_dictionary(dict_i, hit_id, feat_i, nterms, hit_id_counter, feature, context, token_id, (*i-1), dict);

    if (Progress::check_abort())
      stop("Aborted");

    p.increment(1);
  }
  DataFrame df = DataFrame::create(Named("hit_id") = hit_id,
                                   Named("dict_i") = dict_i,
                                   Named("feat_i") = feat_i,
                                   Named("nterms") = nterms);

  return df;
}

