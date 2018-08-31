#include "QueryIter.h"



List get_nested_terms(QueryIter &q, int nested_i, int in_quote, bool in_quotemark) ;

// [[Rcpp::export]]
List parse_query_cpp(std::string x) {
  QueryIter q(x);
  return get_nested_terms(q, 0, 0, false);
}



bool char_in_flag(std::string flag, char c) {
  bool opened = false; // for ignoring flag between curly brackets (used for sub query)
  for (char &flag_c : flag) {
    if (flag_c == '{') opened = true;
    if (flag_c == '}') opened = false;
    if (!opened and flag_c == c) return (true);
  }
  return (false);
}

int get_number(std::string &x) {
  std::string numchar = "";
  for (auto xi : x) {
    if (isdigit(xi)) numchar.push_back(xi);
  }
  return atoi(numchar.c_str());
}

void rstrip(std::string &x){
  while (x[x.size()-1] == ' ') x.erase(x.size()-1);
}

List get_flag_query(std::string flag) {
  // the sub query allows the user to specify additional conditions for a term based on any available column (e.g., POS tag, dependency relation)
  // the format is {column1: query1, column2: query2}, where the second query is optional.
  // the query can only contain OR statemens.
  // (note that QueryIter is not used here)
  std::map<std::string, std::vector<std::string> > out;

  std::string name = "";
  std::string term = "";

  int part = 1;
  bool opened = false;
  for (char &x: flag) {
    if (x == '{') {
      if (opened) stop("Trying to open second sub-query (with '{') before closing the first:\n\t" + flag);
      opened = true;
      continue;
    }
    if (!opened) {
      continue;
    }
    if (x == ':') {
      if (part == 2) stop("double separater : between column name and sub-query:\n\t" + flag);
      rstrip(name);
      part = 2;
      continue;
    }
    if (part == 1) {
      if (x == '}') stop("sub query not properly defined: forat is {column1: query1, column2: query2}.\n\t" + flag);
      if (x == ' ' and name == "") continue;
      name.push_back(x);
    }
    if (part == 2) {
      if (x == ' ' or x == '}' or x == ',') {
        if (term == "OR") term = "";
        if (term == "AND") stop("sub query cannot contain Boolean operator AND");
        if (term == "NOT") stop("sub query cannot contain Boolean operator NOT. For matching everything except a certain term, use the ! symbol as a prefix. e.g. {column: query1 !query2}");
        if (term != "") out[name].push_back(term);
        term = "";
      } else {
        term.push_back(x);
      }
      if (x == '}' or x == ',') {
        if (!opened) stop("Trying to close sub-query (with '}') before opening:\n\t" + flag);
        if (name == "" or out[name].size() == 0) stop("sub query not properly defined: forat is {column1: query1, column2: query2}.\n\t" + flag);
        name = "";
        part = 1;
        if (x == '}') opened = false;
      }
    }
  }
  return wrap(out);
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
      tlist["case_sensitive"] = char_in_flag(flag, 's');
      tlist["ghost"] = char_in_flag(flag, 'g');
      tlist["flag_query"] = get_flag_query(flag);

      tlist["term"] = term;
      out.push_back(tlist);

    } else {
      out.push_back(terms[i]);
    }
  }
  return out;
}

void add_term(QueryIter &q, List &terms, std::vector<std::string> &term_flags, std::string &term) {
  if (term.size() > 0) {
    terms.push_back(term);
    std::string term_flag = q.pop_flag();
    term_flags.push_back(term_flag);
    term = "";
  }
}

void add_nested(QueryIter &q, List &terms, std::vector<std::string> &term_flags, List nested) {
  terms.push_back(nested);
  term_flags.push_back(""); // no flags for nested terms
}

void mismatch_error(QueryIter q, int nested_i, std::string end_char) {
  std::string mismatch = q.get_from(nested_i);
  stop("incorrect nesting: trying to close with '" + end_char + "' but opened with '" + mismatch.substr(0,1) + "'\n  -->\t" + mismatch);
}

List get_nested_terms(QueryIter &q, int nested_i = 0, int in_quote = 0, bool in_quotemark = false) {
  List out;
  List terms;
  std::vector<std::string> term_flags;
  std::string term = "";
  bool all_sensitive = false; // case sensitive
  bool all_ghost = false;  // ghost terms  are taken into account in the query (e.g., x and y~i) but are not returned in the results
  List all_flag_query;

  std::string relation = "";
  std::string feature = "";

  bool lag_space = true;
  while (true) {
    if (q.done()) {
      if (q.get_i(nested_i) == '"') mismatch_error(q, nested_i, " ");
      if (q.get_i(nested_i) == '(') mismatch_error(q, nested_i, " ");
      if (q.get_i(nested_i) == '<') mismatch_error(q, nested_i, " ");
      break;
    }
    char x = q.pop();

    // escape next char
    if (x == '\\') {
      term = term + q.pop();
      continue;
    }

    // skip double spaces
    if (x == ' ') {
      if (lag_space) {
        continue;
      }
      lag_space = true;
    } else lag_space = false;

    // nesting within parentheses
    if (x == '(') {
      add_term(q, terms, term_flags, term); // in case that there is not space between a term and the parentheses
      List nested = get_nested_terms(q, q.get_position()-1, in_quote, in_quotemark);
      add_nested(q, terms, term_flags, nested);
      continue;
    }
    // ending part between parentheses
    if (x == ')') {
      if (q.get_i(nested_i) != '(') mismatch_error(q, nested_i, ")");
      std::string flag = q.pop_flag();
      all_ghost = char_in_flag(flag, 'g');
      all_sensitive = char_in_flag(flag, 's');
      all_flag_query = get_flag_query(flag);
      break;
    }

    // nesting within quotes
    if ((x == '"' and !(q.get_i(nested_i) == '"')) or x == '<') {
      add_term(q, terms, term_flags, term);
      bool quotemark = x == '"';  // when using quotemarks (that do not have separate open and close symbols) remember opening
      List nested = get_nested_terms(q, q.get_position()-1, in_quote+1, quotemark);
      add_nested(q, terms, term_flags, nested);
      continue;
    }
    // ending part between quotes
    if ((x == '"' and in_quotemark) or x == '>') {
      if (x == '"' and (q.get_i(nested_i) != '"'))  mismatch_error(q, nested_i, "\"");
      if (x == '>' and (q.get_i(nested_i) != '<')) mismatch_error(q, nested_i, ">");
      std::string flag = q.pop_flag();
      int window = get_number(flag); // gets number from string. if no number present, returns zero
      all_ghost = char_in_flag(flag, 'g');
      all_sensitive = char_in_flag(flag, 's');
      all_flag_query = get_flag_query(flag);
      if (window == 0) {
        relation = "sequence";  // if no window is given, its a sequence query
      } else {
        relation = "proximity"; // otherwise, a proximity query
        out["directed"] = char_in_flag(flag, 'd');
        out["window"] = window;    // convert int in string form to int
      }
      break;
    }

    if (x == ':') {
      if (terms.size() != 0 or term == "") stop("Manual feature column has to be specified at the beginning of query or nested query, as: 'column: ...'. To use double dot regularly, escape it with \\: (double slash if typed in R)");
      feature = term;
      term = "";
      continue;
    }

    // a space at this point indicates the end of a term
    if (x == ' ') {
      add_term(q, terms, term_flags, term);
    } else {
      term = term + x;
    }

    // if next in line is a flag, this is a term with a flag
    if (q.is('~')) {
      add_term(q, terms, term_flags, term);
    }

  }
  add_term(q, terms, term_flags, term);

  out["all_case_sensitive"] = all_sensitive;
  out["all_ghost"] = all_ghost;
  out["all_flag_query"] = all_flag_query;
  out["terms"] = parse_terms(terms, term_flags);
  if (relation == "") relation = get_bool_operator(terms);
  out["relation"] = relation;
  out["feature"] = feature;

  if (in_quote > 0) { // current level is within quotes
    if (relation == "AND") stop("Cannot use AND inside of quotes");
    if (relation == "NOT") stop("Cannot use NOT inside of quotes");
  }
  if (in_quote > 1) { // current level is nested within quotes
    if (relation == "proximity") stop("Cannot nest a proximity search inside of quotes");
  }

  if (as<List>(out["terms"]).size() != 2 and as<std::string>(out["relation"]) == "NOT") stop("NOT operator can only be used with one term before and one term after. Note that these can be nested, for example: (a AND b) NOT (c OR d)");

  return out;
}

/*** R
x = parse_query('test# A')
*/
