#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

bool match_shortest(std::string &x, std::set<std::string> &group_set){
  for (const auto &y : group_set) {
    unsigned int length = x.size();
    if (y.size() < length) length = y.size();
    if (x.substr(0,length) == y.substr(0,length)) return true;
  }
  return false;
}

// [[Rcpp::export]]
NumericVector AND_hit_ids_cpp(NumericVector con, NumericVector subcon, NumericVector term_i, double n_unique, std::vector<std::string> group_i, LogicalVector replace, bool feature_mode) {
  double n = con.size();
  bool use_subcon = subcon.size() > 0;  // use the fact that as.Numeric(NULL) in R returns a vector of length 0 (NULL handling in Rcpp is cumbersome)
  bool new_assign;
  NumericVector out(n);

  std::map<int,std::set<int> > tracker;       // keeps track of new unique term_is and their position. When n_unique is reached: returns hit_id and resets
  std::map<int,std::set<std::string> > group_tracker;  // in AND, if one term of a group is found, every term must be true (because we search nested)

  int iw = 0;
  int hit_id = 1;
  for (int i = 0; i < n; i++) {
    for (iw = i; iw < n; iw++) {
      if (con[iw] != con[i]) break;                // break if different (next) context
      if (use_subcon) {
        if (subcon[iw] != subcon[i]) break;
      }

      if (!replace[iw] and !feature_mode) {
        if (out[iw] > 0) continue;                                      // skip already assigned
        if (tracker.count(term_i[iw])) {                                // skip if unique term_i already observed...
          if (group_i[iw] == "") continue;                              // but only if there's no group_id...
          //if (group_tracker[term_i[iw]].count(group_i[iw])) continue; // or if group_i is already observed
          if (match_shortest(group_i[iw], group_tracker[term_i[iw]])) continue; // alternative: match on higher level (prevent double counting)
        }
      }

      tracker[term_i[iw]].insert(iw);
      if (group_i[iw] != "") group_tracker[term_i[iw]].insert(group_i[iw]);

      if (!replace[iw] and !feature_mode) {
        if ((group_tracker.size() == 0) && (tracker.size() == n_unique)) break;
      }
    }

    if (tracker.size() == n_unique) {       // if a full set was observed
      new_assign = false;
      for (const auto &positions : tracker){
        for (const auto &position : positions.second) {
          if (out[position] == 0) new_assign = true;
          out[position] = hit_id; // assign hit_id for positions stored in tracker
        }
      }
      if (!feature_mode) {
        hit_id ++;                                 // up counter and reset the tracker
        if (replace[i] and new_assign) i--;    // if term is a replaceable term, repeat the loop until no new hits are found
      }
    }
    tracker.clear();
    group_tracker.clear();
  }
  return out;
}
