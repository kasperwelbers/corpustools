#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
CharacterVector ngrams_cpp(CharacterVector tokens, CharacterVector group, int n, std::string sep, std::string empty)
{
  int len = tokens.size();
  CharacterVector out(len);
  std::string last_group = std::string(group[0]);
  int local_i = 0;

  for (int i = 0; i < len; i++)
  {
    if (last_group != std::string(group[i])) local_i = 0;
    last_group = std::string(group[i]);

    out[i] = tokens[i];
    if (CharacterVector::is_na(out[i])) {
      local_i += 1;
      continue;
    }

    for (int j = 1; j < n; j++)
    {
      if ((local_i - j) < 0){
        out[i] = empty + sep + std::string(out[i]);
      }
      else {
        out[i] = std::string(tokens[i - j]) + sep + std::string(out[i]);
      }
    }
    local_i += 1;
  }

  return out;
}

