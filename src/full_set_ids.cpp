#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
IntegerVector proximity_hit_ids(IntegerVector pos, IntegerVector value, int n_unique, int window) {
  int n = pos.size();
  IntegerVector out(n);
  std::map<int,int> tracker;
  std::map<int,int>::iterator it;

  int iw = 0;
  int hit_id = 1;
  for (int i = 0; i < n; i++) {
    for (iw = i; iw < n; iw++) {
      if (pos[iw] - pos[i] > window) {      // OR doc[iw] != doc[i] ... // implement in future: prevents need for global_i.
        tracker.clear();                    // break if position out of window
        break;
      }
      if (out[iw] > 0) continue;            // skip already assigned

      it = tracker.find(value[iw]);
      if(it != tracker.end()) continue;     // skip if unique value already observed

      tracker[value[iw]] = iw;
      if (tracker.size() == n_unique) {     // if a full set is observed
        for(std::map<int,int>::iterator get = tracker.begin(); get != tracker.end(); ++get) {
          out[get->second] = hit_id;        // assign hit_id for positions stored in tracker
        }
        hit_id ++;                          // up counter and reset the tracker
        tracker.clear();
        break;                              // and restart
      }
    }
  }
  return out;
}


// [[Rcpp::export]]
IntegerVector sequence_hit_ids(IntegerVector pos, IntegerVector value, int length) {
  int n = pos.size();
  IntegerVector out(n);

  int seq_i;
  int fill_i;
  int hit_id = 1;
  for (int i = 0; i < n; i++) {
    for (seq_i = 0; seq_i < length; seq_i++) {
      if (pos[i+seq_i] - pos[i] > 2) break;      // there cant be a gap         // OR doc[i+seq_i] != doc[i] ... // implement in future: prevents need for global_i.
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
