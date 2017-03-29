lda_features <- function(tc, m, feature, new_feature='LDA_topic', context_level=c('document','sentence')){
  context_level = match.arg(context_level)
  if (!is(m, 'LDA_Gibbs')) stop('An LDA model of class LDA_Gibbs (topicmodels::LDA with method = "Gibbs") is required')

  d = data.table(context = tc$context(context_level),
                 feature = fast_factor(tc$data(feature)),
                 i = 1:tc$n)
  wa = data.table(context=factor(m@documents[m@wordassignments$i]),
                  feature=factor(m@terms[m@wordassignments$j]),
                  v=m@wordassignments$v)

  setkeyv(d, c('context','feature'))
  setkeyv(wa, c('context','feature'))
  merge(d, wa, all.x = T)
}


#' Estimate a topic model using the lda package
#'
#' Estimate an LDA topic model using the \code{\link{LDA}} function
#' The parameters other than dtm are simply passed to the sampler but provide a workable default.
#' See the description of that function for more information
#'
#' @param dtm a document term matrix (e.g. the output of \code{\link{amcat.dtm.create}})
#' @param K the number of clusters
#' @param num.iterations the number of iterations
#' @param alpha the alpha parameter
#' @param eta the eta parameter
#' @return A fitted LDA model (see \code{\link{LDA}})
#' @export
lda_fit <- function(dtm, method='Gibbs', K=50, num.iterations=500, alpha=50/K, eta=.01, burnin=250) {
  if (!require(topicmodels)) stop('The topicmodels package is required for this function, but has not yet been installed. You can run install.packages("topicmodels")')
  if (is(dtm, 'DocumentTermMatrix')) dtm = tm_dtm_to_dgTMatrix(dtm)

  empty_rows = sum(Matrix::rowSums(dtm) == 0)
  empty_cols = sum(Matrix::rowSums(dtm) == 0)
  if (empty_rows) message(sprintf('%s rows in the dtm are empty. These have been deleted', empty_rows))
  if (empty_cols) message(sprintf('%s columns in the dtm are empty. These have been deleted', empty_cols))

  dtm = dtm[Matrix::rowSums(dtm) > 0, Matrix::colSums(dtm) > 0]
  m = LDA(dtm, k=K, method=method, control=list(iter=num.iterations, burnin=burnin, alpha=alpha, delta=eta))
  m
}



#' Get word assignments from LDA_GIBBS class (output of lda_fit). This is similar to the documentsums object that comes as the output of lda.collapsed.gibbs.sampler
#'
#' Get word assignments from LDA_GIBBS class (output of lda_fit). This is similar to the documentsums object that comes as the output of lda.collapsed.gibbs.sampler
#' LDA assigns a topic to each unique word in a document. If you also want to take into account how often this word occured, the document term matrix (as used in the input for lda_fit) must be included in the weight.by.dtm argument.
#'
#' @param m The output from one of the topicmodeling functions in the topicmodels package (e.g., LDA_GIBBS)
#' @param weight.by.dtm If you want to weight the topic assignment of a word to the number of times the word occured, give the document term matrix for this argument
#' @return A matrix where rows are topics and columns are documents. Values represent the number of times the topic is assigned to a word in this document (essentially this is the same as the documentsums object in the output of lda.collapsed.gibbs.samler)
#' @export
documentsums <- function(m, weight.by.dtm=NULL){
  assignments = data.frame(i=m@wordassignments$i, j=m@wordassignments$j, v=m@wordassignments$v)
  if(!is.null(weight.by.dtm)){
    dtm = weight.by.dtm[m@documents,m@terms]
    dtm = data.frame(i=dtm$i, j=dtm$j, count=dtm$v)
    assignments = merge(assignments, dtm, by=c('i','j'), all.x=T)
    docsums = acast(assignments, v ~ i, value.var='count', fun.aggregate=sum)
  } else docsums = acast(assignments, v ~ i, value.var='j', fun.aggregate=length)
  docsums
}

#' Get the topics per document
#'
#' Return a data frame containing article ids and topic occurence per document. Topic occurence can either be based on the posterior distrition (default) or based on wordassignments (as.wordassignments = T).
#'
#' @param m A LDA model from the topicmodels package
#' @param as.wordassignments If True, return the topic occurence per document as the number of words assigned to the topic (instead of the posterior distribution)
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
topics_per_document <- function(m, as.wordassignments=F) {
  ids = as.numeric(m@documents)
  if(as.wordassignments) {
    tpd = t(documentsums(m))
  } else{
    tpd = posterior(m)$topics
  }
  cbind(id=ids, data.frame(tpd))
}

#' Add document meta to LDA output
#'
#' Add a dataframe containing document meta to the output (a list) of \code{\link{lda.collapsed.gibbs.sampler}}.
#'
#' @param m The output of \code{\link{LDA}}
#' @param meta A data.frame with document meta. Has to contain a vector to match the document.ids
#' @param match.by The name of the vector in meta that matches the document.ids
#' @return The LDA output appended with document meta
#' @export
order_meta <- function(m, meta, match.by = 'id'){
  meta[match(m@documents, meta[,match.by]),]
}

### PLOT LDA TOPICS

#' Plot all topics
#'
#' Write plots for all topics with \code{\link{plot.topic}} in designated folder
#'
#' @param m The output of \code{\link{collapsed.gibbs.sampler}}
#' @param time_var A vector with time stamps (either numeric or Date class) of the same length and order of the documents (rows) in m$document_sums
#' @param category_var A vector with id values of the same length and order of the documents (rows) in m$document_sums
#' @param path The path for a folder where output will be saved
#' @param date_interval The interval for plotting the values over time. Can be: 'day', 'week', 'month' or 'year'
#' @param ... Additional options passed to lda_plot_topic (and on to lda_plot_wordcloud)
#' @return Nothing
#' @export
lda_plot_alltopics <- function(m, time_var, category_var, path, date_interval='day', value='relative', create_index=T, ...){
  if (!file.exists(path)) dir.create(path)
  for(topic_nr in 1:m@k){
    message('Plotting:', topic_nr)
    fn = file.path(path, paste(topic_nr, ".png", sep=""))
    if (!is.null(fn)) png(fn, width=1280,height=800)
    lda_plot_topic(m, topic_nr, time_var, category_var, date_interval, value=value, ...)
    if (!is.null(fn)) dev.off()
  }
  par(mfrow=c(1,1), mar=c(3,3,3,3))
  if (create_index) {
    index = create_index(m)
    fn = file.path(path, "index.html")
    write(index,  fn)
    message('Writing ', fn)
    if (interactive()) browseURL(fn)
  }
}

#' Plots topic wordcloud, and attention over time and per category
#'
#' Plots \code{\link{plot.wordcloud}}, \code{\link{plot.time}} and \code{\link{plot.category}}
#'
#' @param m The output of \code{\link{collapsed.gibbs.sampler}}
#' @param The index of the topic (1 to K)
#' @param time_var A vector with time stamps (either numeric or Date class) of the same length and order of the documents (rows) in m$document_sums
#' @param category_var A vector with id values of the same length and order of the documents (rows) in m$document_sums
#' @param date_interval The interval for plotting the values over time. Can be: 'day', 'week', 'month' or 'year'
#' @param pct Show topic values as percentages
#' @param value Show topic values as 'total', or as 'relative' to the attention for other topics
#' @param ... Additional options passed to lda_plot_wordcloud
#' @return Nothing, just plots
#' @export
lda_plot_topic <- function(m, topic_nr, time_var, category_var, date_interval='day', pct=F, value='relative', ...){
  par(mar=c(4.5,3,2,1), cex.axis=1.7)
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), widths=c(3,1), heights=c(1,3))
  lda_plot_time(m, topic_nr, time_var, date_interval, pct=pct, value=value)
  lda_plot_wordcloud(m, topic_nr, ...)
  lda_plot_category(m, topic_nr, category_var, pct=pct, value=value)
  par(mfrow=c(1,1), mar=c(3,3,3,3))
}

#' Change date object to date_interval
#'
#' Change date object to date_interval
#'
#' @param time_var A vector of Date values
#' @param date_interval The desired date_interval ('day','week','month', or 'year')
#' @return A vector of Date values
#' @export
prepare_time_var <- function(time_var, date_interval){
  if(class(time_var) == 'Date'){
    if(date_interval == 'day') time_var = as.Date(format(time_var, '%Y-%m-%d'))
    if(date_interval == 'month') time_var = as.Date(paste(format(time_var, '%Y-%m'),'-01',sep=''))
    if(date_interval == 'week') time_var = as.Date(paste(format(time_var, '%Y-%W'),1), '%Y-%W %u')
    if(date_interval == 'year') time_var = as.Date(paste(format(time_var, '%Y'),'-01-01',sep=''))
  }
  time_var
}

#' Add empty values for pretty plotting
#'
#' When plotting a timeline, gaps in date_intervals are ignored. For the attention for topics gaps should be considered as having value 0.
#'
#' @param d A data.frame with the columns 'time' (Date) and 'value' (numeric)
#' @param date_interval The date_interval is required to know what the gaps are
#' @return A data.frame with the columns 'time' (Date) and 'value' (numeric)
#' @export
fill_time_gaps <- function(d, date_interval){
  if(class(d$time) == 'numeric'){
    t = min(d$time):max(d$time)
    empty = t[!t %in% d$time]
    if (length(empty) > 0) d = rbind(d, data.frame(time=empty, value=0))
  }
  if(class(d$time) == 'Date'){
    date_sequence = seq.Date(from=min(d$time), to=max(d$time), by=date_interval)
    empty = date_sequence[!date_sequence %in% d$time]
    if (length(empty) > 0) d = rbind(d, data.frame(time=empty, value=0))
  }
  d[order(d$time),]
}

#' Prepares the topic values per document for plotting
#'
#' Prepares the topic values per document for plotting
#'
#' @param m The output of \code{\link{LDA}}
#' @param break_var A break vector to aggregate topic values per document
#' @param The index of the topic (1 to K)
#' @param pct Show topic values as percentages
#' @param value Show topic values as 'total', or as 'relative' to the attention for other topics
#' @return The aggregated/transformed topic values
#' @export
prepare_plot_values <- function(m, break_var, topic_nr, pct=F, value='relative', filter=NULL){
  docsums = documentsums(m)
  hits = docsums[topic_nr,]
  d = aggregate(hits, by=list(break_var=break_var), FUN='sum')
  if(value == 'relative'){
    total_hits = colSums(docsums)
    totals = aggregate(total_hits, by=list(break_var=break_var), FUN='sum')
    d$x = d$x / totals$x
  }
  if(pct == T) d$x = d$x / sum(d$x)
  d
}


#' Plots topic values over time
#'
#' Plots the attention for a topic over time
#'
#' @param m The output of \code{\link{LDA}}
#' @param The index of the topic (1 to K)
#' @param time_var A vector with time stamps (either numeric or Date class) of the same length and order of the documents (rows) in m$document_sums
#' @param date_interval The interval for plotting the values over time. Can be: 'day', 'week', 'month' or 'year'
#' @param pct Show topic values as percentages
#' @param value Show topic values as 'total', or as 'relative' to the attention for other topics
#' @param return.values Logical. If true, data that is plotted is returned as a data.frame
#' @return data.frame for plotted values
#' @export
lda_plot_time <- function(m, topic_nr, time_var, date_interval='day', pct=F, value='relative', return.values=F){
  par(mar=c(3,3,3,1))
  time_var = prepare_time_var(time_var, date_interval)
  d = prepare_plot_values(m, break_var=time_var, topic_nr=topic_nr, pct=pct, value=value)
  colnames(d) = c('time','value')
  d = fill_time_gaps(d, date_interval)
  plot(d$time, d$value, type='l', xlab='', main='', ylab='', xlim=c(min(d$time), max(d$time)), ylim=c(0, max(d$value)), bty='L', lwd=5, col='darkgrey')
  par(mar=c(3,3,3,3))
  if(return.values==T) d
}

#' Plots topic values per category
#'
#' Plots the attention for a topic per category
#'
#' @param m The output of \code{\link{LDA}}
#' @param The index of the topic (1 to K)
#' @param category_var A vector with id values of the same length and order of the documents (rows) in m$document_sums
#' @param pct Show topic values as percentages
#' @param value Show topic values as 'total', or as 'relative' to the attention for other topics
#' @param return.values Logical. If true, data that is plotted is returned as a data.frame
#' @return data.frame for plotted values
#' @export
lda_plot_category <- function(m, topic_nr, category_var, pct=F, value='total', return.values=F){
  par(mar=c(15,5,1,2))
  d = prepare_plot_values(m, break_var=category_var, topic_nr=topic_nr, pct=pct, value=value)
  colnames(d) = c('category','value')
  barplot(as.matrix(t(d[,c('value')])), main='', beside=TRUE,horiz=FALSE,
          density=NA,
          col='darkgrey',
          xlab='',
          ylab="",
          axes=T, names.arg=d$category, cex.names=1.2, cex.axis=1.2, adj=1, las=2)
  par(mar=c(3,3,3,3))
  if(return.values==T) d
}

#' Plot wordcloud for LDA topic
#'
#' Plots a wordcloud of the top words per topic
#'
#' @param m The output of \code{\link{LDA}}
#' @param topic_nr The index of the topic (1 to K)
#' @param ... Additional options passed to dtm.wordcloud, e.g. freq.fun and pal
#' @return Nothing, just plots
#' @export
lda_plot_wordcloud  <- function (m, topic_nr, ...)
{
  x = posterior(m)$terms[topic_nr, ]
  x = sort(x, decreasing = T)[1:100]
  x = x[!is.na(x)]
  names = sub("/.*", "", names(x))

  dtm.wordcloud(terms = names, freqs = x, ...)
}


#' Create an index file for plot.all.topics
create_index <- function(m) {
  html = c('<html>',
           '<head><link href="http://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.2.0/css/bootstrap.min.css" rel="stylesheet"></head>',
           '<body><div class="row">')
  for (i in 1:nrow(m@beta)) {
    chunk = paste('<div class="col-xs-6 col-md-2 col-lg-3"><a href="', i, '.png"',
                  ' class="thumbnail"><img src="',i,'.png"></a></div>', sep='')
    html = c(html, chunk)
  }
  html = c(html, "</div></body></html>")
  paste(html, collapse="\n")
}


#' Create JSON data for LDAvis
#'
#' @param m a fitted lda model
#' @param dtm the dtm used to cerate the lda model
#'
#' @return a json string suitable for use with LDAvis::servis
#' @export
ldavis_json <- function(m, dtm){

  dtm = dtm[row_sums(dtm) > 0, ]

  # Find required quantities
  phi <- posterior(m)$terms %>% as.matrix
  theta <- posterior(m)$topics %>% as.matrix
  vocab <- colnames(phi)

  doc.length = row_sums(dtm)
  term.freq = col_sums(dtm)[match(vocab, colnames(dtm))]

  createJSON(phi = phi, theta = theta,
             vocab = vocab,
             doc.length = doc.length,
             term.frequency = term.freq)

}
