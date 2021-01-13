#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
NumericVector group_coref_ids(NumericVector x, NumericVector y, int n) {
  NumericVector out;
  out = Rcpp::Range(1,n);
  
  for (int i = 0; i < x.size(); i++) {
    if (x[i] > y[i]) 
      out[x[i]] = out[y[i]];  
    else 
      out[y[i]] = out[x[i]];
  }
  return(out);
}

bool found_candidate(int i, int j, NumericVector& coref_id, double& top_score, CharacterVector& gender, CharacterVector& number, CharacterVector& so, CharacterVector& person, CharacterVector& pt, LogicalVector& noun, LogicalVector& pronoun) {
  // changes coref_id, unassigned and top_score by candidate
  
  // lets call i the current token and j the candidate token.
  if (!pronoun[i]) return(true); // current token must be a pronoun (if it isn't, break the loop in coref_candidate_select)
  if (noun[j]) return(true);     // candidate must not be a noun (debatable, but currently nouns give too much trouble)
  if (pronoun[j]) {                   // if the candidate is also a pronoun 
    if (person[i] != person[j]) return(false);  // person (1st, 2nd, 3rd) need to match
    if (pt[i] != pt[j]) return(false);          // pronoun type needs to match
    if (gender[i] != gender[j]) return(false);  // gender must match
  } else {                            // otherwise (if candidate is NOUN or PROPN)  
    if (person[i] != "3") return false; // only continue if current token is third person.
  }
  if ((number[i] == "Plur") != (number[j] == "Plur")) return(false);    // If current pronoun is plural, candidate must also be plural 
  if ((gender[i] == "Neut") & ((gender[j] == "Masc") | (gender[j] == "Fem"))) return(false);
  if ((gender[i] == "Masc") & (gender[j] == "Fem")) return(false);
  if ((gender[i] == "Fem") & (gender[j] == "Masc")) return(false);
  
  // from here we have several optional conditions, and we keep a score to determine priority
  double score = 1;
  if (gender[i] != "?") {
    if (gender[j] == "?") score += 0.5;
    if (gender[i] == gender[j]) score += 1;
  } 
  if ((abs(i-j) < 20) && (so[i] == so[j])) score += 1; // short distance with identical subject / object positions has priority
  if (j < i) score += 1;                               // lag has priority over lead
  score += (1 / pow(abs(i-j),0.5));
  if (!pronoun[j]) score += 1;                    // non-pronouns have priority 
  
  if (score > top_score) {
    coref_id[i] = coref_id[j];
    top_score = score;
  }
  return (top_score == 6); // if the top score is the max score we can stop. Currently this is a  bit silly, but we might add more sophisticated scoring.
}

// [[Rcpp::export]]
NumericVector coref_candidate_select(LogicalVector needs_coref, CharacterVector doc_id, CharacterVector gender, CharacterVector number, CharacterVector so, CharacterVector person, CharacterVector pt, LogicalVector noun, NumericVector pos, NumericVector id, LogicalVector pronoun, int lag, int lead) {
  NumericVector coref_id = clone(id);
  for (int i = 0; i < doc_id.size(); i++) {
    double top_score = 0;
    bool unassigned = needs_coref[i];
    if (unassigned) {
      for (int j = i-1; j >= 0; j--) {
        if (doc_id[i] != doc_id[j]) break;
        if ((pos[i] - pos[j]) > lag) break;
        if (found_candidate(i,j,coref_id,top_score,  gender,number,so,person,pt,noun,pronoun)) {
          unassigned = false;
          break;
        }
      }
    }
    if (unassigned) {
      for (int j = i+1; j < doc_id.size(); j++) {
        if (doc_id[i] != doc_id[j]) break;
        if ((pos[j] - pos[i]) > lead) break;
        if (found_candidate(i,j,coref_id,top_score,  gender,number,so,person,pt,noun,pronoun)) break;
      }
    }
  }  
  return(coref_id);
}


