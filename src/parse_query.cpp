#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

// QUERY ITERATOR CLASS

class QueryIter {
  std::string query;
  int position;
public:
  QueryIter (std::string);
  void next () {if (position < query.size()) position++;}
  char pop ();                             // get char at current position and move position ahead
  char get_i (int i) {return query[i];}    // get char at specific position
  int get_position () {return position;}   // get position
  bool done () {return position >= query.size();} // not more chars left
  bool is (char);                          // check if char at current position is [argument]
  bool is_in (std::string);                // check if char at current position is in [argument]
  bool is_break () {return is_in("() \"<>");}
  std::string get_from (int);              // get string from given position till current position (used for error messages)
  std::string pop_till_break ();           // get string till next break (spaces, parentheses, angle brackets, quotes)
  std::string pop_flag ();                 // get string till next break if its a flag (i.e. starts with ~)
  void validate_query();                   // check for common errors in query syntax
};

QueryIter::QueryIter (std::string x) {
  query = " " + x;  // start with empty space to prevent issues with nested_i
  position = 0;
}

char QueryIter::pop() {
  char c = query[position];
  next();
  return c;
}

bool QueryIter::is (char c) {
  if (position == query.length()-1) return false;
  char x = query[position];
  return x == c;
}

bool QueryIter::is_in (std::string breaks) {
  char c = query[position];
  for (char b: breaks){
    if (c == b) return true;
  }
  return false;
}

std::string QueryIter::get_from(int from) {
  return query.substr(from, position);
}

std::string QueryIter::pop_till_break () {
  std::string out = "";
  while (!done()) {
    if (is_break()) break;
    out.push_back(pop());
  }
  return out;
}

std::string QueryIter::pop_flag () {
  if (is('~')) {
    next();
    return pop_till_break();
  }
  return "";
}

void QueryIter::validate_query(){

  int lpar = std::count(query.begin(), query.end(), '(');
  int rpar = std::count(query.begin(), query.end(), ')');
  int lquote = std::count(query.begin(), query.end(), '<');
  int rquote = std::count(query.begin(), query.end(), '>');
  int quote = std::count(query.begin(), query.end(), '"');

  if (!(quote % 2 == 0)) stop("Number of quotes is not even (can't have a quote without an unquote)");
  if (lpar != rpar) stop("Number of opening and closing parentheses does not match");
  if (lquote != rquote) stop("Number of opening and closing angle brackets (<>) does not match");
}

// PARSE QUERY

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

    if (b == "AND" or b == "OR" or b == "NOT") {
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

List parse_terms(List terms, std::vector<std::string> flags) {
  List out;
  int n = terms.size();
  for (int i = 0; i < n; i++) {
    if (TYPEOF(terms[i]) == STRSXP) {  // non-STRSXP are the nested queries
      std::string term = terms[i];
      if (term == "AND" or term == "OR" or term == "NOT") continue;
      std::string flag = flags[i];
      if (term == "") continue;

      List tlist;
      tlist["case_sensitive"] = char_in_string(flag, 's');
      tlist["ghost"] = char_in_string(flag, 'g');

      tlist["term"] = term;
      out.push_back(tlist);

    } else {
      out.push_back(terms[i]);
    }
  }
  return out;
}

void add_term(QueryIter &q, List &terms, std::vector<std::string> &term_flags, std::string &term) {
  terms.push_back(term);
  std::string term_flag = q.pop_flag();
  term_flags.push_back(term_flag);
  term = "";
}

void add_nested(QueryIter &q, List &terms, std::vector<std::string> &term_flags, List nested) {
  terms.push_back(nested);
  term_flags.push_back(""); // no flags for nested terms
}

List get_nested_terms(QueryIter &q, int nested_i = 0, int in_quote = 0) {
  List out;
  List terms;
  std::vector<std::string> term_flags;
  std::string term = "";
  bool all_sensitive = false; // case sensitive
  bool all_ghost = false;  // ghost terms  are taken into account in the query (e.g., x and y~i) but are not returned in the results

  std::string relation = "";

  bool lag_space = true;
  while (!q.done()) {
    char x = q.pop();

    // skip double spaces
    if (x == ' ') {
      if (lag_space) {
        continue;
      }
      lag_space = true;
    } else lag_space = false;

    // nesting parts within parentheses
    if (x == '(') {
      if (term.size() > 0) add_term(q, terms, term_flags, term);
      List nested = get_nested_terms(q, q.get_position()-1, in_quote);
      add_nested(q, terms, term_flags, nested);
      continue;
    }
    if (x == ')') {
      if (!q.get_i(nested_i) == '(') stop("mismatch in parentheses: " + q.get_from(nested_i));
      std::string flag = q.pop_flag();
      all_ghost = char_in_string(flag, 'g');
      all_sensitive = char_in_string(flag, 's');
      break;
    }

    // nesting parts within quotes
    if ((x == '"' and (q.get_i(nested_i) == '"')) or x == '>') {
      if (x == '>' and !q.get_i(nested_i) == '<') stop("mismatch in quotes / angle brackets: " + q.get_from(nested_i));
      std::string flag = q.pop_flag();
      int window = get_number(flag); // gets number from string. if no number present, returns zero
      all_ghost = char_in_string(flag, 'g');
      all_sensitive = char_in_string(flag, 's');
      if (window == 0) {
        relation = "sequence";  // if no window is given, its a sequence query
      } else {
        relation = "proximity"; // otherwise, a proximity query
        out["directed"] = char_in_string(flag, 'd');
        out["window"] = window;    // convert int in string form to int
      }
      break;
    }

    // ending quotes
    if ((x == '"' and !(q.get_i(nested_i) == '"')) or x == '<') {
      List nested = get_nested_terms(q, q.get_position()-1, in_quote+1);
      add_nested(q, terms, term_flags, nested);
      continue;
    }

    // a space at this point indicates the end of a term
    if (x == ' ') {
      if (term.size() == 0) continue;
      add_term(q, terms, term_flags, term);
    } else {
      term = term + x;
    }

    // if next in line is a flag, this is also the end of a term
    if (q.is('~')) {
      add_term(q, terms, term_flags, term);
    }

  }
  if (term.size() > 0) add_term(q, terms, term_flags, term);


  out["all_case_sensitive"] = all_sensitive;
  out["all_ghost"] = all_ghost;
  out["terms"] = parse_terms(terms, term_flags);
  if (relation == "") relation = get_bool_operator(terms);
  out["relation"] = relation;

  if (in_quote > 0) {
    if (relation == "AND") stop("Cannot use AND inside of quotes");
    if (relation == "NOT") stop("Cannot use NOT inside of quotes");
  }
  if (in_quote > 1) {
    if (relation == "proximity") stop("Cannot nest a proximity search inside of quotes");
  }

  if (as<List>(out["terms"]).size() != 2 and as<std::string>(out["relation"]) == "NOT") stop("NOT operator can only be used with one term before and one term after. Note that these can be nested, for example: (a AND b) NOT (c OR d)");

  return out;
}

// [[Rcpp::export]]
List parse_query(std::string x) {
  QueryIter q(x);
  q.validate_query();
  return get_nested_terms(q);
}


/*** R
#parse_query('"renewable fuel" AND better')
*/
