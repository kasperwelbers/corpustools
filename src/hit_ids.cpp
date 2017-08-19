#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

std::set<int> get_sequence(int &iw, NumericVector &seq_i){
  std::set<int> seq_i_set;
  int lag_seq_i = 0;
  for (int si = iw; si < seq_i.size(); si++) {
    if (NumericVector::is_na(seq_i[si])) break;
    if (seq_i[si] <= lag_seq_i) break;
    seq_i_set.insert(si);
    lag_seq_i = seq_i[si];
  }
  return seq_i_set;
}

// [[Rcpp::export]]
NumericVector proximity_hit_ids(NumericVector con, NumericVector subcon, NumericVector pos, NumericVector value, double n_unique, double window, NumericVector seq_i, bool assign_once) { // note that double is required for is_na()
  double n = pos.size();
  bool use_subcon = subcon.size() > 0;  // use the fact that as.Numeric(NULL) in R returns a vector of length 0 (NULL handling in Rcpp is cumbersome)
  bool use_seq = seq_i.size() > 0;
  NumericVector out(n);

  std::map<int,std::set<int>> tracker;       // keeps track of new unique values and their position. When n_unique is reached: returns hit_id and resets
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

      if (use_seq) {
        if (!NumericVector::is_na(seq_i[iw])) {
          std::set<int> seq_i_set = get_sequence(iw, seq_i);   // a simple get sequence that doesn't check for context/window (this is already done to get the seq_i)
          tracker[value[iw]].insert(seq_i_set.begin(), seq_i_set.end());
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
NumericVector sequence_hit_ids(NumericVector con, NumericVector subcon, NumericVector pos, NumericVector value, double length) {
  double n = pos.size();
  bool use_subcon = subcon.size() > 0;  // use the fact that as.Numeric(NULL) in R returns a vector of length 0 (NULL handling in Rcpp is cumbersome)
  NumericVector out(n);

  double seq_i;
  double fill_i;
  double hit_id = 1;
  for (double i = 0; i < n; i++) {
    for (seq_i = 0; seq_i < length; seq_i++) {
      if (out[i+seq_i] > 0) continue;            // skip already assigned
      if (value[i+seq_i] != seq_i+1) break;   // seq_i (starting at 0) should match the number of the word in the sequence (starting at 1)

      if (pos[i+seq_i] - pos[i] > 2 | con[i+seq_i] != con[i]) break;      // there cant be a gap (or same context)
      if (use_subcon) {                            // check first value
        if (subcon[i+seq_i] != subcon[i]) break;
      }

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
