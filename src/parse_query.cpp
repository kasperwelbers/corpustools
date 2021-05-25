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
  if (numchar.length() == 0) return -1;
  return atoi(numchar.c_str());
}

void rstrip(std::string &x){
  while (x[x.size()-1] == ' ') x.erase(x.size()-1);
}


List get_flag_query(std::string flag) {
  std::map<std::string, std::vector<std::string> > out;
  
  bool tokenexpr = false;
  bool metaexpr = false;
  std::string txt = "";
  for (char &x: flag) {
    if (x == '{') {
      if (tokenexpr) stop("Incorrect flag (~): Trying to open another { before closing it first");
      if (metaexpr) stop("Incorrect flag (~): Trying to open { before closing current [ ]");
      tokenexpr = true;
      continue;
    }
    if (x == '[') {
      if (metaexpr) stop("Incorrect flag (~): Trying to open another [ before closing it first");
      if (tokenexpr) stop("Incorrect flag (~): Trying to open [ before closing current { }");
      metaexpr = true;
      continue;
    }
    if (x == '}') {
      if (!tokenexpr) stop("Incorrect flag (~): Trying to close } but have not yet opened it");
      if (metaexpr) stop("Incorrect flag (~): Trying to close } but currently within [ ]");
      tokenexpr = false;
      out["tokenexpr"].push_back(txt);
      txt = "";
      continue;
    }
    if (x == ']') {
      if (!metaexpr) stop("Incorrect flag (~): Trying to close ] but have not yet opened it");
      if (tokenexpr) stop("Incorrect flag (~): Trying to close ] but currently within {}");
      metaexpr = false;
      out["metaexpr"].push_back(txt);
      txt = "";
      continue;
    }
    
    if (tokenexpr) txt.push_back(x);
    if (metaexpr) txt.push_back(x);
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
  std::string meta_expr = "";
  
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

    // skip double spaces
    if (x == ' ') {
      if (lag_space) {
        continue;
      }
      lag_space = true;
    } else lag_space = false;
    
    // escape next char
    if (x == '\\') {
      x = q.pop();
      if (x == '*') {
        term = term + "\\*";
      } else if (x == '?') {
        term = term + "\\?";
      } else {
        term= term + x;
      }
      continue;
    }
    
    // add verbatim term
    if (x == '{') {
      x = q.pop();
      bool escape_verbatim_close = false;
      while ((x != '}') &! escape_verbatim_close) {
        if (x == '{') stop("Trying to open a verbatim term within a verbatim term. In other words: opening another { before closing the current {}");
        if (x == '*') {
          term = term + "\\*";
        } else if (x == '?') {
          term = term + "\\?";
        } else {
          term= term + x;
        }
        x = q.pop();
        if (x == '\\') {
          escape_verbatim_close = true;
          x = q.pop();
        } else escape_verbatim_close = false;
        if (q.done()) stop("Did not close a verbatim term. i.e. forgot to close a { } part.");
      }
      continue;
    }

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
      if (window < 0) {
        relation = "sequence";  // if no window is given, its a sequence query
      } else {
        relation = "proximity"; // otherwise, a proximity query
        out["directed"] = char_in_flag(flag, 'd');
        out["window"] = window;    // convert int in string form to int
      }
      break;
    }

    if (x == ':') {
      if (terms.size() != 0 or term == "") stop("The colon symbol ':' is used to specify a manual feature column, but this has to be at the start of a query or directly after an open parenthesis '('. If you wanted to use a colon regularly, escape it with \\: (double slash if typed in R)");
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
x = parse_query_cpp('(this~{test}[that])~[ok]')
#x
#parse_query('(this~{test}[that])~[ok]')
#tc$tokens[eval(parse(text=x$terms[[2]]$flag_query)),]
*/
