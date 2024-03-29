% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/untokenize.r
\name{untokenize}
\alias{untokenize}
\title{Reconstruct original texts}
\usage{
untokenize(tc)
}
\arguments{
\item{tc}{A tCorpus, created with \code{\link{create_tcorpus}}, with remember_spaces = TRUE}
}
\value{
A data.table with the text fields and meta fields as columns.
}
\description{
If the tCorpus was created with remember_spaces = T, you can rebuild the original texts.
}
\examples{
tc = create_tcorpus(sotu_texts, doc_column='id')
untokenize(tc)
}
