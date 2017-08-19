#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

bool is_lpar(char x) {return x == '(';}
bool is_rpar(char x) {return x == ')';}
bool is_space(char x) {return x == ' ';}
bool is_lquote(char x) {return x == '<';}
bool is_rquote(char x) {return x == '>';}
bool is_quote(char x) {return x == '"' or x == '<' or x == '>';}
bool is_break(char x) {return (is_lpar(x) or is_rpar(x) or is_space(x) or is_quote(x));}
bool is_flag(char x) {return x == '~';}

bool is_and(std::string x) {return x == "AND";}
bool is_or(std::string x) {return x == "OR";}
bool is_not(std::string x) {return x == "NOT";}
bool is_bool(std::string x) {return (is_and(x) or is_or(x) or is_not(x));}

char get_first_char(std::string &x) {
  char xi = x[0];
  x.erase(0,1);
  return(xi);
}

std::string get_till_break(std::string &x) {
  std::string out = "";
  while (x.size() > 0) {
    out = out + get_first_char(x);
    if (is_break(x[0])) break;
  }
  return out;
}

bool char_in_string(std::string s, char c) {
  int i = s.find(c);
  return (i >= 0);
}

int get_number(std::string &x) {
    std::string numchar = "";
    for (auto xi : x) {
      if (isdigit(xi)) numchar.push_back(xi);
    }
    return atoi(numchar.c_str());
}

// Travis breaks on regex_replace, probably due to older gcc compiler (which is probably a good reason not to use this)
//std::string escape_special(std::string x) {
//  std::regex e("([^0-9a-zA-Z])");
//  return std::regex_replace(x, e, "\\$1");
//}
//std::string wildcard_as_regex(std::string x){
//  std::regex e("([*?])");
//  return std::regex_replace(x, e, ".$1");
//}

std::string extract_flag(std::string &x){
  int i = x.find("~");
  std::string flag = "";
  if (i >= 0) {
    flag = x.substr(i, x.size() - i);
    x.erase(i, x.size() - i);
  }
  return flag;
}

void validate_query(std::string x){
  int lpar = std::count(x.begin(), x.end(), '(');
  int rpar = std::count(x.begin(), x.end(), '(');
  int lquote = std::count(x.begin(), x.end(), '<');
  int rquote = std::count(x.begin(), x.end(), '>');
  int quote = std::count(x.begin(), x.end(), '"');

  if (!(quote % 2 == 0)) stop("Number of quotes is not even (can't have a quote without an unquote)");
  if (lpar != rpar) stop("Number of opening and closing parentheses does not match");
  if (lquote != rquote) stop("Number of opening and closing angle brackets (<>) does not match");
}

std::string get_bool_operator(List terms) {
  // returns the boolean operator being used, and tests whether only one boolean operator is used
  std::string out = "";
  std::string b = "";
  bool lag_bool = true;
  int n = terms.size();
  for (int i = 0; i < n; i++) {
    if (TYPEOF(terms[i]) == STRSXP) { // terms[i] is either a string or a nested query
      b = Rcpp::as<std::string>(terms[i]);
    } else b = " ";   // if a nested query, just give any non boolean value

    if (is_bool(b)) {
      if (i == 0) stop("cannot start with boolean operators (AND, OR, NOT)");
      if (i == n-1) stop("cannot end with boolean operators (AND, OR, NOT)");
      if (lag_bool) stop("cannot have adjacent boolean operators (AND, OR, NOT)");
      lag_bool = true;
    } else {
      if (not lag_bool) {
        b = "OR";   // having no boolean operator (between non-boolean terms) is interpreted as an OR
      } else {
        lag_bool = false;
        continue;
      }
    }

    if (out != "") {
      if (out != b) stop("Cannot have more than one type of boolean operator at the same level. Use parentheses to explicitly nest. For example, use 'a OR (b AND c)' or '(a OR b) AND c'");
    } else out = b;
  }
  return out;
}

List parse_terms(List terms) {
  List out;
  int n = terms.size();
  for (int i = 0; i < n; i++) {
    if (TYPEOF(terms[i]) == STRSXP) {  // non-STRSXP are the nested queries
      std::string term = terms[i];
      if (is_bool(term)) continue;
      //std::string reg = term;
      //reg = escape_special(reg);
      //reg = wildcard_as_regex(reg);
      std::string flag = extract_flag(term); // also removes flag from term
      if (term == "") continue;

      List tlist;
      tlist["case_sensitive"] = char_in_string(flag, 's');
      tlist["invisible"] = char_in_string(flag, 'i');
      tlist["term"] = term;
      //tlist["regex"] = "\\b" + reg + "\\b";
      out.push_back(tlist);

    } else {
      out.push_back(terms[i]);
    }
  }
  return out;
}

List end_quoted(List out, std::string &x){
  if (is_flag(x[0])) {              // if next char is a flag, this is a proximity query, with the flag specifying the direction and window size
    out["relation"] = "proximity";
    std::string flag = get_till_break(x);
    out["directed"] = char_in_string(flag, 'd');  // the first char is the flag symbol, which can be <, > or ~ (left, right or both)
    out["window"] = get_number(flag);    // convert int in string form to int
  } else {                          // otherwise, this is a sequence
    out["relation"] = "sequence";
  }
  return out;
}

List get_nested_terms(std::string &x, int in_quote = 0) {
  List out;
  List terms;
  std::string term = "";

  bool lag_space = true;
  while (x.size() > 0) {
    char xi = get_first_char(x);

    // skip double spaces
    if (is_space(xi)) {
      if (lag_space) continue;
      lag_space = true;
    } else lag_space = false;

    // nesting parts within parentheses
    if (is_lpar(xi)) {
      terms.push_back(get_nested_terms(x)); // note that x is passed by reference, so that nested get_first_char affects x at all levels
      continue;
    }
    if (is_rpar(xi)) break;

    // nesting parts within quotes
    if (is_quote(xi)) {
      if (in_quote > 0 and !(is_lquote(xi))) {
        out = end_quoted(out, x);
        if (in_quote > 1 and as<std::string>(out["relation"]) == "proximity") stop("Cannot nest a proximity search inside of quotes");
        break;
      } else {
        terms.push_back(get_nested_terms(x, in_quote+1)); // note that in_quote changes the behaviour in the recursion
        continue;
      }
    }

    if (is_break(xi)) {      // if a break at this point, this indicates the end of a term
      if (term.size() == 0) continue;
      if (in_quote > 0 and (is_and(term) or is_not(term))) stop("Cannot use AND or NOT operators inside of quotes");
      terms.push_back(term);
      term = "";
    } else {
      if (not is_space(xi)) term = term + xi;      // unless a space, add char xi to term.
    }
  }
  if (term.size() > 0) terms.push_back(term);

  if (terms.size() == 1) return terms[0];
  out["terms"] = parse_terms(terms);
  if (in_quote == 0) out["relation"] = get_bool_operator(terms);
  return out;
}

// [[Rcpp::export]]
List parse_query(std::string x){
  validate_query(x);
  return get_nested_terms(x);
}


