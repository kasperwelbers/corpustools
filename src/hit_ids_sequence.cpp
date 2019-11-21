#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
NumericVector sequence_hit_ids_cpp(NumericVector con, NumericVector subcon, NumericVector pos, NumericVector term_i, double length) {
  double n = pos.size();
  bool use_subcon = subcon.size() > 0;  // as.Numeric(NULL) in R returns a vector of length 0
  NumericVector out(n);

  double seq_i = 0;
  double fill_i = 0;
  double hit_id = 1;
  for (double i = 0; i < n; i++) {
    for (seq_i = 0; seq_i < length; seq_i++) {
      if ((i+seq_i) >= n) break;
      if (out[i+seq_i] > 0) continue;            // skip already assigned
      if (term_i[i+seq_i] != seq_i+1) break;     // seq_i (starting at 0) should match the number of the word in the sequence (starting at 1)

      if ((pos[i+seq_i] - pos[i] > (0 + seq_i)) | (con[i+seq_i] != con[i])) break;      // there cant be a gap (or same context)
      if (use_subcon) {
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
