#include "QueryIter.h"

QueryIter::QueryIter (std::string x) {
  query = " " + x + " ";  // start with empty space to prevent issues with nested_i
  position = 0;
}

void QueryIter::next () {if (position < query.size()) position++;}

char QueryIter::pop() {
  char c = query[position];
  next();
  return c;
}

char QueryIter::get_i (int i) {return query[i];}

int QueryIter::get_position () {return position;}

bool QueryIter::done () {return position >= query.size();}

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

bool QueryIter::is_break () {return is_in("() \"<>");}

std::string QueryIter::get_from(int from) {
  return query.substr(from, position-from);
}

std::string QueryIter::pop_till_break () {
  std::string out = "";
  bool escaped1 = false;
  bool escaped2 = false;
  while (!done()) {
    if (is('{')) escaped1 = true;
    if (is('[')) escaped2 = true;
    if (is_break() and !escaped1 and !escaped2) break;
    if (is('}')) escaped1 = false;
    if (is(']')) escaped2 = false;
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
