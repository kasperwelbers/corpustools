#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
NumericVector coref_ids(NumericVector x, NumericVector y, int n) {
  NumericVector out;
  out = Rcpp::Range(1,n);
  
  for (int i = 0; i < x.size(); i++) {
    out[y[i]] = out[x[i]];  
  }
  return(out);
}

bool found_candidate(int i, int j, NumericVector& coref_id, double& top_score, CharacterVector& gn, CharacterVector& so) {
  // changes coref_id and top_score by reference
  if (coref_id[j] == 0) return false;
  if (gn[i] != gn[j]) return false;   // hard criterion
  double score = 0;
  if (so[i] == so[i]) score += 1;
  if (score > top_score) {
    coref_id[i] = coref_id[j];
    top_score = score;
  }
  return(top_score == 1);     // if the top score is the max score we can stop. Currently this is silly, but we might add more sophisticated scoring.
}

// [[Rcpp::export]]
NumericVector coref_candidate_select(CharacterVector doc_id, CharacterVector gn, CharacterVector so, NumericVector pos, NumericVector coref_id, int lag, int lead) {
  NumericVector out = clone(coref_id);
  for (int i = 0; i < doc_id.size(); i++) {
    double top_score = 0;
    if (out[i] == 0) {
      for (int j = i; j >= 0; j--) {
        if (doc_id[i] != doc_id[j]) break;
        if ((pos[i] - pos[j]) > lag) break;
        if (found_candidate(i,j,out,top_score,gn,so)) break;
      }
    }
    if (out[i] == 0) {
      for (int j = i; j < doc_id.size(); j++) {
        if (doc_id[i] != doc_id[j]) break;
        if ((pos[j] - pos[i]) > lead) break;
        if (found_candidate(i,j,out,top_score,gn,so)) break;
      }
    }
  }  
  return(out);
}


