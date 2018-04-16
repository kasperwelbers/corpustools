#ifndef __QUERYITER_H_INCLUDED__
#define __QUERYITER_H_INCLUDED__

#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

class QueryIter {
  std::string query;
  unsigned int position;
public:
  QueryIter (std::string);
  void next ();                            // move to next character
  char pop ();                             // get char at current position and move position ahead
  char get_i (int);                        // get char at specific position
  int get_position ();                     // get position
  bool done ();                            // not more chars left
  bool is (char);                          // check if char at current position is [argument]
  bool is_in (std::string);                // check if char at current position is in [argument]
  bool is_break ();                        // any term/context breaking character used in query parser
  std::string get_from (int);              // get string from given position till current position (used for error messages)
  std::string pop_till_break ();           // get string till next break (spaces, parentheses, angle brackets, quotes)
  std::string pop_flag ();                 // get string till next break if its a flag (i.e. starts with ~)
};

#endif
