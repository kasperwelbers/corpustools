#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<std::string> collapse_terms_cpp(std::vector<std::string>& term, LogicalVector& collapse, std::string sep = " ", std::string sep2 = " ") {
  // returns a shorter vector, in which all TRUE terms are collapsed with the most recent FALSE term.
  // the first TRUE after a FALSE will be connected with the string in sep, but consecutive TRUE's use sep2
  // sep2 is currently used to collapse multiple empty space character (\s,\t,\n,\r\n), for the optional "space" column in tcorpus.
  int n = term.size();
  if (n != collapse.size()) stop("term and collapse arguments need to be vectors of the same size");

  std::vector<std::string> out(sum(!collapse));
  int pos = 0;
  bool use_sep2 = false;
  for (int i = 0; i < n; i++) {
    if (!collapse[i] or i == 0) {
      out[pos] = term[i];
      pos++;
      use_sep2 = false;
    } else {
      if (use_sep2) {
        out[pos-1] = out[pos-1] + sep2 + term[i];
      } else {
        out[pos-1] = out[pos-1] + sep + term[i];
      }
      use_sep2 = true;
    }
  }
  return(out);
}

// [[Rcpp::export]]
std::map<std::string,std::vector<std::string>> uncollapse_terms_cpp(std::vector<std::string>& term, std::string sep = " ") {
  // split by first occurence of sep. Uses first occurence only so that sep symbol used for collapsing is only forbidden in left part
  // modifies the input vector by reference to contain the left part, and return a vector with the right part
  int n = term.size();
  std::vector<std::string> right(term.size());
  std::string t;
  int split_i;
  for (int i = 0; i < n; i++) {
    t = term[i];
    split_i = t.find(sep);

    if (split_i > 0) {
      term[i] = t.substr(0,split_i);
      right[i] = t.substr(split_i+1, t.npos);
    }
  }
  std::map<std::string,std::vector<std::string>> out;
  out["left"] = term;
  out["right"] = right;
  return(out);
}
