#include "MatrixColIter.h"

int range_end(std::vector<int>& v, int start_position, int max_position, int max_value) {
  // given a vector v and start_position, move forward till either the max_position is reached or v > max_value
  for (std::vector<int>::iterator it = v.begin() + start_position; it - v.begin() <= max_position; ++it) {
    if (*it > max_value) return(it - v.begin() -1);
  }
  return(max_position);
}

int range_start(std::vector<int>& v, int start_position, int min_position, int min_value) {
  // Given a vector v and a start position, move backward till either the min_position is reached or v < min_value
  for (std::vector<int>::iterator it = v.begin() + start_position; it - v.begin() >= min_position; --it) {
    if (*it < min_value) return(it - v.begin() -1);
  }
  return(min_position);
}

MatrixColIter::MatrixColIter (std::vector<int> group, std::vector<int> order, int lwindow = 0, int rwindow = 0) {
  n = group.size();
  if (n != order.size()) stop("Length of order and group has to be the same");
  if (lwindow < 0 or rwindow < 0) stop("window values (lwindow, rwindow) cannot be negative");

  int group_end, order_end, order_end_window;
  for (int group_start = 0; group_start < n; group_start = group_end+1){
    group_end = range_end(group, group_start, n-1, group[group_start]);
    for (int order_start = (group_start); order_start <= group_end; order_start = order_end+1) {
      order_end = range_end(order, order_start, group_end, order[order_start]);

      group_v.push_back(group[group_start]);
      order_v.push_back(order[order_start]);

      if (lwindow != 0) {
        int order_start_window = range_start(order, order_start, group_start, order[order_start] - lwindow);
        start.push_back(order_start_window);
      } else start.push_back(order_start);
      if (rwindow != 0) {
        int order_end_window = range_end(order, order_start, group_end, order[order_start] + rwindow);
        end.push_back(order_end_window);
      } else end.push_back(order_end);
    }
  }
  position = 0;
  n_positions = group_v.size();
}

int MatrixColIter::get_group () {return(group_v[position]);}
int MatrixColIter::get_order () {return(order_v[position]);}
int MatrixColIter::get_start () {return(start[position]);}
int MatrixColIter::get_end () {return(end[position]);}
bool MatrixColIter::is_done () {return(position >= n_positions);}
double MatrixColIter::pct_done () {return(double(start[position]) / n);}
void MatrixColIter::next_position () {position++;}

bool MatrixColIter::move_to_position(int group, int order) {
  if (position >= n_positions) return(false);
  while (group > group_v[position]) {
    position++;
    if (position >= n_positions) return(false);
  }
  while (order > order_v[position] and group == group_v[position]) {
    position++;
    if (position >= n_positions) return(false);
  }
  return(group == group_v[position] and order == order_v[position]);
}

Eigen::SparseMatrix<double> MatrixColIter::subset_matrix(Eigen::SparseMatrix<double>& m) {
  Eigen::SparseMatrix<double> subset;
  subset = m.middleCols(start[position], end[position] - start[position] +1);
  return(subset);
}



