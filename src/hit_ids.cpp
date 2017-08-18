#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]



// [[Rcpp::export]]
IntegerVector proximity_hit_ids(IntegerVector con, IntegerVector subcon, IntegerVector pos, IntegerVector value, double n_unique, double window, NumericVector group_id, bool assign_once) { // note that double is required for is_na()
  int n = pos.size();
  bool use_subcon = subcon.size() > 0;  // use the fact that as.integer(NULL) in R returns a vector of length 0 (NULL handling in Rcpp is cumbersome)
  bool use_group = group_id.size() > 0;
  IntegerVector out(n);

  std::map<int,std::set<int>> tracker;       // keeps track of new unique values and their position. When n_unique is reached: returns hit_id and resets
  //std::map<int,std::map<int,int>> sub_tracker;   // if a value
  std::map<int,std::set<int>>::iterator it;

  int iw = 0;
  int hit_id = 1;
  for (int i = 0; i < n; i++) {
    for (iw = i; iw < n; iw++) {
      if (con[iw] != con[i]) break;                // break if different (next) context
      if (!NumericVector::is_na(window)) {         // if window not specified, it's basically an AND search
        if (pos[iw] - pos[i] > window) break ;     // break if position out of window
      }
      if (use_subcon) {                            // check first value
        if (subcon[iw] != subcon[i]) break;
      }

      if (assign_once) {
        if (out[iw] > 0) continue;            // skip already assigned
        it = tracker.find(value[iw]);
        if(it != tracker.end()) continue;     // skip if unique value already observed
      }

      tracker[value[iw]].insert(iw);

      if (use_group) {
        if (!NumericVector::is_na(group_id[iw])) {
          NumericVector this_id = group_id[iw];
          IntegerVector gi = match(this_id, group_id);
          tracker[value[iw]].insert(gi.begin(), gi.end());
        }
      }

      if (assign_once) {
        if (tracker.size() == n_unique) break;
      }
    }

    if (tracker.size() == n_unique) {       // if a full set was observed
      for (const auto &positions : tracker){
        for (const auto &position : positions.second) {
          out[position] = hit_id; // assign hit_id for positions stored in tracker
        }
      }
      hit_id ++;                          // up counter and reset the tracker
    }
    tracker.clear();
  }
  return out;
}


// [[Rcpp::export]]
IntegerVector sequence_hit_ids(IntegerVector con, IntegerVector subcon, IntegerVector pos, IntegerVector value, double length) {
  int n = pos.size();
  bool use_subcon = subcon.size() > 0;  // use the fact that as.integer(NULL) in R returns a vector of length 0 (NULL handling in Rcpp is cumbersome)
  IntegerVector out(n);

  int seq_i;
  int fill_i;
  int hit_id = 1;
  for (int i = 0; i < n; i++) {
    for (seq_i = 0; seq_i < length; seq_i++) {
      if (pos[i+seq_i] - pos[i] > 2 | con[i+seq_i] != con[i]) break;      // there cant be a gap (or same context)
      if (use_subcon) {                            // check first value
        if (subcon[i+seq_i] != subcon[i]) break;
      }

      if (out[i+seq_i] > 0) continue;            // skip already assigned
      if (value[i+seq_i] != seq_i+1) continue;   // seq_i (starting at 0) should match the number of the word in the sequence (starting at 1)

      if (seq_i == length-1) {
        for (fill_i = 0; fill_i < length; fill_i++) {
          out[i+fill_i] = hit_id;
        }
        hit_id++;
        break;
      }
    }
  }
  return out;
}
