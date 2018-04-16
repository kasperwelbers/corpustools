#include "MatrixColIter.h"


void divide_by_colsum(Eigen::SparseMatrix<double>& cp, Eigen::SparseMatrix<double>& m) {
  if (cp.cols() != m.cols()) {
    Rcout << cp.cols() << " " << m.cols() << std::endl;
    stop("nee, fout");
  }
  double sum;
  for (int k=0; k < m.outerSize(); ++k) {
    sum = 0;
    for (Eigen::SparseMatrix<double>::InnerIterator it(m, k); it; ++it) {
      sum += it.value();
    }
    for (Eigen::SparseMatrix<double>::InnerIterator it(cp, k); it; ++it) {
      it.valueRef() = it.value() / sum;
    }
  }
}

void matrix_norm_scores(Eigen::SparseMatrix<double>& m) {
  // divide non-zero cells by column norm: sqrt(sum(j^2))
  double norm;
  for (int k=0; k < m.outerSize(); ++k) {
    norm = 0;
    for (Eigen::SparseMatrix<double>::InnerIterator it(m, k); it; ++it) {
      norm += pow(it.value(), 2);
    }
    norm = sqrt(norm);
    for (Eigen::SparseMatrix<double>::InnerIterator it(m, k); it; ++it) {
      it.valueRef() = it.value() / norm;
    }
  }
}

void matrix_dicho_scores(Eigen::SparseMatrix<double>& m) {
  // make non-zero cells 1
  for (int k=0; k < m.outerSize(); ++k) {
    for (Eigen::SparseMatrix<double>::InnerIterator it(m, k); it; ++it) {
      it.valueRef() = 1;
    }
  }
}


void fill_triples(std::vector<Eigen::Triplet<double>>& tl, Eigen::SparseMatrix<double>& cp, int row_offset, int col_offset, double min_value, bool upper = true) {
  int row, col;
  for (int k=0; k < cp.outerSize(); ++k) {
    col = k + col_offset;
    //Rcout << col << std::endl;
    for (Eigen::SparseMatrix<double>::InnerIterator it(cp, k); it; ++it) {
      row = it.row() + row_offset;
      if (it.value() < min_value) continue;
      if (upper) {
        if (row >= col) break;
      } else {
        if (row == col) continue;
      }
      //Rcout << "  " << row << std::endl;
      tl.push_back(Eigen::Triplet<double>(col, row, it.value()));
    }
  }
}

void calc_sim(std::vector<Eigen::Triplet<double>>& tl, Eigen::SparseMatrix<double>& m1, int m1_offset, Eigen::SparseMatrix<double>& m2, int m2_offset, std::string measure, double min_value) {
  if (measure == "overlap_pct") {
    matrix_dicho_scores(m2);
    Eigen::SparseMatrix<double> cp = m2.transpose() * m1;
    divide_by_colsum(cp, m1);
    fill_triples(tl, cp, m2_offset, m1_offset, min_value, false);
  }
  if (measure == "cosine") {
    Eigen::SparseMatrix<double> cp = m1.transpose() * m2;
    fill_triples(tl, cp, m1_offset, m2_offset, min_value, true);
  }
}


void manage_capacity(std::vector<Eigen::Triplet<double>>& tl, double pct_done) {
  if (pct_done > 0){
    double est_required = ceil((1/pct_done) * tl.size() * 1.1);
    if (est_required > tl.capacity()) {
      if (est_required * 5 > tl.capacity()) est_required = tl.capacity() * 10;  // limit to 5 times current capacity
      tl.reserve(est_required);
    }
  }
}

// [[Rcpp::export]]
Eigen::SparseMatrix<double> compare_documents_cpp(Eigen::SparseMatrix<double>& m, std::vector<int> group, std::vector<int> order, int lwindow=0, int rwindow=0, std::string measure="cosine", double min_value=0, bool verbose=true) {
  int n = group.size();
  m = m.transpose();
  if (n != m.cols()) stop("Length of order and group has to be the same as the number of columns in m");

  MatrixColIter m1i = MatrixColIter(group, order, 0, 0);
  MatrixColIter m2i = MatrixColIter(group, order, lwindow, rwindow);

  if (measure == "cosine") matrix_norm_scores(m);

  std::vector<Eigen::Triplet<double>> tl;
  tl.reserve(ceil(n*10));    // start with lazy guess. update in manage_capacity()

  //Progress p(n, verbose); // disabled due to error after aborting
  while (!m1i.is_done()) {
    bool has_position = m2i.move_to_position(m1i.get_group(), m1i.get_order());
    if (has_position) {
      manage_capacity(tl, (m1i.pct_done() + m2i.pct_done()) / 2);
      Eigen::SparseMatrix<double> m1 = m1i.subset_matrix(m);
      Eigen::SparseMatrix<double> m2 = m2i.subset_matrix(m);
      calc_sim(tl, m1, m1i.get_start(), m2, m2i.get_start(), measure, min_value);
    }
    //if (Progress::check_abort())
    //  stop("Aborted");
    //p.increment(m1i.get_end() - m1i.get_start());
    m1i.next_position();
  }

  Eigen::SparseMatrix<double> out(m.cols(),m.cols());
  out.setFromTriplets(tl.begin(), tl.end());
  return(out);
}


// [[Rcpp::export]]
Eigen::SparseMatrix<double> compare_documents_xy_cpp(Eigen::SparseMatrix<double>& m_x, std::vector<int> group_x, std::vector<int> order_x, Eigen::SparseMatrix<double>& m_y, std::vector<int> group_y, std::vector<int> order_y, int lwindow=0, int rwindow=0, std::string measure="cosine", double min_value=0, bool verbose=true) {
  int n_x = group_x.size();
  int n_y = group_y.size();
  m_x = m_x.transpose();
  m_y= m_y.transpose();
  if (n_x != m_x.cols()) stop("Length of order_x and group_x has to be the same as the number of columns in m_x");
  if (n_y != m_y.cols()) stop("Length of order_y and group_y has to be the same as the number of columns in m_y");

  MatrixColIter m_xi = MatrixColIter(group_x, order_x, 0, 0);
  MatrixColIter m_yi = MatrixColIter(group_y, order_y, lwindow, rwindow);

  if (measure == "cosine") {
    matrix_norm_scores(m_x);
    matrix_norm_scores(m_y);
  }

  std::vector<Eigen::Triplet<double>> tl;
  tl.reserve(ceil((n_x+n_y)*10));    // start with lazy guess. update in manage_capacity()

  //Progress p(n_x, verbose);
  while (!m_xi.is_done()) {
    bool has_position = m_yi.move_to_position(m_xi.get_group(), m_yi.get_order());
    if (has_position) {
      manage_capacity(tl, (m_xi.pct_done() + m_yi.pct_done()) / 2);
      Eigen::SparseMatrix<double> m1 = m_xi.subset_matrix(m_x);
      Eigen::SparseMatrix<double> m2 = m_yi.subset_matrix(m_y);
      Rcout << m_yi.get_start() << std::endl;
      //Rcout << m1.cols() << " " << m2.cols() << std::endl;
      calc_sim(tl, m1, m_xi.get_start(), m2, m_yi.get_start(), measure, min_value);
    }
    //if (Progress::check_abort())
    //  stop("Aborted");
    //p.increment(m_xi.get_end() - m_xi.get_start());
    m_xi.next_position();
  }

  Eigen::SparseMatrix<double> out(m_x.cols(),m_y.cols());
  out.setFromTriplets(tl.begin(), tl.end());
  return(out);
}
