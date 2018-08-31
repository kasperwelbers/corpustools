#ifndef __MATRIXCOLITER_H_INCLUDED__
#define __MATRIXCOLITER_H_INCLUDED__

// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

class MatrixColIter {
  int position;
  int n;
  int n_positions;
  std::vector<int> group_v;
  std::vector<int> order_v;
  std::vector<int> start;
  std::vector<int> end;
public:
  MatrixColIter (std::vector<int>, std::vector<int>, int, int);
  int get_group ();
  int get_order ();
  int get_start ();
  int get_end ();
  bool is_done ();
  double pct_done ();
  void next_position ();
  bool move_to_position (int, int);
  Eigen::SparseMatrix<double> subset_matrix (Eigen::SparseMatrix<double>&);
};

#endif
