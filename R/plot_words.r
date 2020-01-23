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
#' @param col  A vector of colors that is passed to colorRamp to interpolate colors over x axis
#' @param ... additional parameters passed to the plot function
#'
#' @return nothing
#' @examples
#' x = c(-10, -5, 3, 5)
#' y = c(0, 2, 5, 10)
#' words = c('words', 'where', 'you', 'like')
#'
#' \donttest{
#' plot_words(x,y,words, c(1,2,3,4))
#' }
#' @export
plot_words <- function(x, y=NULL, words, wordfreq=rep(1, length(x)), xlab='', ylab='', yaxt='n', scale=1, random.y=T, xlim=NULL, ylim=NULL, col=c('darkred','navyblue'), ...){
  wordsize = rescale_var(wordfreq, 0.25, scale) + 1
  if (is.null(y) && random.y) y = sample(seq(-1, 1, by = 0.001), length(x))
  if (is.null(y) && !random.y) y = wordsize
  xmargin = (max(x) - min(x)) * 0.2
  ymargin = (max(y) - min(y)) * 0.2
  if (is.null(xlim)) xlim = c(min(x) - xmargin, max(x) + xmargin)
  if (is.null(ylim)) ylim = c(min(y) - ymargin, max(y) + ymargin)
  graphics::plot(x, y, type = "n", xlim = xlim, ylim = ylim, frame.plot = F, yaxt = yaxt, ylab = ylab, xlab = xlab, ...)
  wl <- as.data.frame(wordcloud::wordlayout(x, y, words, cex = wordsize))

  cramp = grDevices::colorRamp(col)
  col = cramp(rescale_var(wl$x, 0, 1))
  col = grDevices::rgb(col[,1], col[,2], col[3,], maxColorValue=255, alpha=255)
  graphics::text(wl$x + 0.5 * wl$width, wl$y + 0.5 * wl$ht, words, cex = wordsize, col=col, ...)
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
#'
#' @examples
#' ## create DTM
#' tc = create_tcorpus(sotu_texts[1:100,], doc_column = 'id')
#' tc$preprocess('token', 'feature', remove_stopwords = TRUE)
#' dtm = get_dtm(tc, 'feature')
#'
#' \donttest{
#' dtm_wordcloud(dtm, nterms = 20)
#'
#' ## or without a DTM
#' dtm_wordcloud(terms = c('in','the','cloud'), freqs = c(2,5,10))
#' }
#' @export
dtm_wordcloud <- function(dtm=NULL, nterms=100, freq.fun=NULL, terms=NULL, freqs=NULL, scale=c(4, .5), min.freq=1, rot.per=.15, ...) {
  if (!is.null(dtm)) {
    t = dtm_term_statistics(dtm)
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

#' visualize vocabularyComparison
#'
#' @param x a vocabularyComparison object, created with the \link{compare_corpus} or \link{compare_subset} method
#' @param n the number of words in the plot
#' @param mode use "both" to plot both overrepresented and underrepresented words using the plot_words function.
#'             Whether a term is under- or overrepresented is indicated on the x-axis, which shows the log ratios (negative is underrepresented, positive is overrepresented).
#'             Use "ratio_x" or "ratio_y" to only plot overrepresented or underrepresented words using dtm_wordcloud
#' @param balance if TRUE, get an equal amount of terms on the left (underrepresented) and right (overrepresented) side. If FALSE, the top chi words are used, regardless of ratio.
#' @param size use "freq", "chi2" or "ratio" for determining the size of words
#' @param ... additional arguments passed to plot_words ("both" mode) or dtm_wordcloud (ratio modes)
#'
#' @examples
#' ## as example, compare SOTU paragraphs about taxes to rest
#' tc = create_tcorpus(sotu_texts[1:100,], doc_column = 'id')
#' comp = tc$compare_subset('token', query_x = 'tax*')
#'
#' \donttest{
#' plot(comp, balance=T)
#' plot(comp, mode = 'ratio_x')
#' plot(comp, mode = 'ratio_y')
#' }
#' @export
plot.vocabularyComparison <- function(x, n=25, mode=c('both', 'ratio_x','ratio_y'), balance=T, size = c('chi2','freq','ratio'), ...){
  #if (!methods::is(x, 'vocabularyComparison')) stop('x has to be a vocabularyComparison object')
  mode = match.arg(mode)
  size = match.arg(size)

  if(mode == 'ratio_y') x = x[x$ratio < 1,]
  if(mode == 'ratio_x') x = x[x$ratio > 1,]
  x = x[order(-x$chi2),]
  if (balance && mode == "both") {
    x = rbind(head(x[x$ratio < 1,], ceiling(n/2)),
              head(x[x$ratio >= 1,], floor(n/2)))
  } else {
    x = head(x, n)
  }

  if (size == 'freq') wsize = relfreqmean = ((x$freq.x / sum(x$freq.x)) + (x$freq.y / sum(x$freq.y))) / 2
  if (size == 'ratio') wsize = x$ratio
  if (size == 'chi2') wsize = x$chi2

  if (mode %in% c('ratio_x','ratio_y')) dtm_wordcloud(terms=x$feature, freqs=wsize, ...)
  if (mode == 'both') plot_words(x = log(x$ratio), words=x$feature, wordfreq = wsize, ...)
}

#' visualize feature associations
#'
#' @param x a featureAssociations object, created with the \link{feature_associations} function
#' @param n the number of words in the plot
#' @param size use "freq", "chi2" or "ratio" for determining the size of words
#' @param ... additional arguments passed to dtm_wordcloud
#'
#' @examples
#' ## as example, compare SOTU paragraphs about taxes to rest
#' tc = create_tcorpus(sotu_texts[1:100,], doc_column = 'id')
#' comp = tc$compare_subset('token', query_x = 'tax*')
#'
#' \donttest{
#' plot(comp, balance=T)
#' plot(comp, mode = 'ratio_x')
#' plot(comp, mode = 'ratio_y')
#' }
#' @export
plot.featureAssociations <- function(x, n=25, size = c('chi2','freq','ratio'), ...){
  size = match.arg(size)
  x = x[x$ratio > 1,]
  x = x[order(-x$chi2),]
  x = head(x, n)

  if (size == 'freq') wsize = relfreqmean = ((x$freq.x / sum(x$freq.x)) + (x$freq.y / sum(x$freq.y))) / 2
  if (size == 'ratio') wsize = x$ratio
  if (size == 'chi2') wsize = x$chi2

  dtm_wordcloud(terms=x$feature, freqs=wsize, ...)
}

