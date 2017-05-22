#' Plot a wordcloud with words ordered and coloured according to a dimension (x)
#'
#' Plot a wordcloud with words ordered and coloured according to a dimension (x)
#'
#' @param x The (approximate) x positions of the words
#' @param y The (approximate) y positions of the words
#' @param wordfreq The frequency of the words, defaulting to 1
#' @param words A character vector with the words to plot
#' @param xlab Label of the x axis
#' @param ylab Label of the y axis
#' @param yaxt see \code{par} documentation
#' @param scale Maximum size to scale the wordsize
#' @param random.y if TRUE, the y position of words is random, otherwise it represents the word frequency.
#' @param xlim Starting value of x axis
#' @param ylim Starting value of y axis
#' @param ... additional parameters passed to the plot function
#'
#' @return nothing
#' @export
plotWords <- function(x, y=NULL, words, wordfreq=rep(1, length(x)), xlab='', ylab='', yaxt='n', scale=2, random.y=T, xlim=NULL, ylim=NULL, ...){
  wordsize = rescale_var(log(wordfreq), 0.75, scale)
  if (is.null(y) & random.y) y = sample(seq(-1, 1, by = 0.001), length(x))
  if (is.null(y) & !random.y) y = wordsize
  xmargin = (max(x) - min(x)) * 0.2
  ymargin = (max(y) - min(y)) * 0.2
  if (is.null(xlim)) xlim = c(min(x) - xmargin, max(x) + xmargin)
  if (is.null(ylim)) ylim = c(min(y) - ymargin, max(y) + ymargin)

  graphics::plot(x, y, type = "n", xlim = xlim, ylim = ylim, frame.plot = F, yaxt = yaxt, ylab = ylab, xlab = xlab, ...)
  wl <- as.data.frame(wordcloud::wordlayout(x, y, words, cex = wordsize))

  graphics::text(wl$x + 0.5 * wl$width, wl$y + 0.5 * wl$ht, words, cex = wordsize, ...)
}

#' Plot a word cloud from a dtm
#'
#' Compute the term frequencies for the dtm and plot a word cloud with the top n topics
#' You can either supply a document-term matrix or provide terms and freqs directly
#' (in which case this is an alias for wordcloud::wordcloud with sensible defaults)
#'
#' @param dtm the document-term matrix
#' @param nterms the amount of words to plot (default 100)
#' @param freq.fun if given, will be applied to the frequenies (e.g. sqrt)
#' @param terms the terms to plot, ignored if dtm is given
#' @param freqs the frequencies to plot, ignored if dtm is given
#' @param scale the scale to plot (see wordcloud::wordcloud)
#' @param min.freq the minimum frquency to include (see wordcloud::wordcloud)
#' @param rot.per the percentage of vertical words (see wordcloud::wordcloud)
#' @param ... other arguments passed to wordcloud::wordcloud
#' @export
dtm.wordcloud <- function(dtm=NULL, nterms=100, freq.fun=NULL, terms=NULL, freqs=NULL, scale=c(6, .5), min.freq=1, rot.per=.15, ...) {
  if (!is.null(dtm)) {
    t = term_statistics(dtm)
    t = t[order(t$termfreq, decreasing=T), ]
    terms = t$term
    freqs = t$termfreq
  }
  if (length(terms) < nterms) nterms = length(terms)
  if (!is.null(freq.fun)) freqs = freq.fun(freqs)
  if (!is.null(nterms)) {
    select = order(-freqs)[1:nterms]
    terms = terms[select]
    freqs = freqs[select]
  }
  if (is.null(terms) | is.null(freqs)) stop("Please provide dtm or terms and freqs")
  wordcloud::wordcloud(terms, freqs,
                        scale=scale, min.freq=min.freq, max.words=Inf, random.order=FALSE,
                        rot.per=rot.per, ...)
}

#' @export
plot.vocabularyComparison <- function(x, n=25, mode=c('both', 'over_x','over_y'), ...){
  mode = match.arg(mode)
  if (!all(c('feature', 'over', 'chi2') %in% colnames(x)))
  if(mode == 'over_y') x = x[x$over < 1,]
  if(mode == 'over_x') x = x[x$over > 1,]
  x = x[order(-x$chi2),]
  x = head(x, n)
  relfreqmean = ((x$freq.x / sum(x$freq.x)) + (x$freq.y / sum(x$freq.y))) / 2
  if (mode %in% c('over_x','over_y')) dtm.wordcloud(terms=x$feature, freqs=log(x$over), ...)
  if (mode == 'both') plotWords(x = log(x$over), words=x$feature, wordfreq = relfreqmean, ...)
}

