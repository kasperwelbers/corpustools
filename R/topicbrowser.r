#' Create a topic browser
#'
#' This will create a set of linked html pages to browse the given topic model
#'
#' @param m: the fitted LDA model object from the topicmodels package
#' @param terms: a vector of terms, which should be sorted in their original order and match m@@terms
#' @param documents: a vector of the same length as terms, indicating the document of each term, matching m@@documents
#' @param meta: a data frame with meta data about the documents, should have columns aid, headline, medium, and date (todo: make more flexible)
#' @param topic_ids: optionally restrict output to a selection of topics
#' @param date_interval: specify the interval for plotting the meta$data
#' @param words: if given, use instead of terms for displaying document
#' @param output: an optional file name to write the browser html to
#' @return the html generated or (invisible) the filename
#' @export
createTopicBrowser <- function(x, terms=NULL, documents=NULL, meta=NULL, topic_ids=1:m@k,
                               date_interval='year', words=terms, output=NULL, browse=interactive()) {
  message("Preparing variables")
  info = if (class(x) == "list") x else clusterinfo(m, terms, documents, meta, topic_ids, words, date_interval)


  url = wrap_html(render_html(info),info = info, output=output)

  if (browse) browseURL(url)
  invisible(url)
}

#' Create a 'clusterinfo' object that can be used for the individual render methods
#'
#' #' @param m: the fitted LDA model object from the topicmodels package
#' @param terms: a vector of terms, which should be sorted in their original order and match m@@terms
#' @param documents: a vector of the same length as terms, indicating the document of each term, matching m@@documents
#' @param meta: a data frame with meta data about the documents, should have columns aid, headline, medium, and date (todo: make more flexible)
#' @param topic_ids: optionally restrict output to a selection of topics
#' @param date_interval: specify the interval for plotting the meta$data
#' @param words: if given, use instead of terms for displaying document
#' @return a list with tokens, wordassignments, and other items
#' @export
clusterinfo <- function(m, terms, documents, meta, topic_ids=1:m@k, words=terms, date_interval='year') {
  keep = documents %in% meta$id
  tokens = data.frame(id=1:sum(keep), term=terms[keep], aid=documents[keep], word=words[keep])

  # build aid / term / topic triplet frame and topics x {doc, term} matrices
  wordassignments = data.frame(aid = m@documents[m@wordassignments$i],
                               term = m@terms[m@wordassignments$j],
                               topic = m@wordassignments$v)
  wordassignments = wordassignments[wordassignments$aid %in% meta$id, ]

  topics_per_doc = acast(wordassignments, topic ~ aid, value.var='term', fun.aggregate=length)
  topics_per_term = acast(wordassignments, topic ~ term, value.var='aid', fun.aggregate=length)

  # order meta for plots
  meta = meta[match(colnames(topics_per_doc), meta$id),]
  list(tokens=tokens, wordassignments=wordassignments, topics_per_doc=topics_per_doc, topics_per_term=topics_per_term,
       topic_ids=topic_ids, meta=meta, date_interval=date_interval)
}

### HTML rendering

#' Wrap a call that 'cats' html fragments and save it as a html file with header and footer
#'
#' Since R evaluates arguments lazily, call with the actual wrapped call, i.e. wrap_html(render_html(...))
#'
#' @param wrapped the wrapped call
#' @param output an optional filename to output to. If NULL, a tempfile will be used
#' @return the output filename
#' @export
wrap_html <- function(wrapped, info, output=NULL) {
  if (is.null(output)) {
    output = tempfile("topicbrowser_", fileext = ".html")
    message("Writing html to ", output)
  }
  sink(output)
  tryCatch({
    cat(html_header(info$topic_ids))
    force(wrapped)
    cat(html_footer())
  }, finally=sink())
  output
}

html_header <- function(topic_ids) {
  TEMPLATE = system.file("template/template.html", package="topicbrowser", mustWork=T)
  html = readChar(TEMPLATE, file.info(TEMPLATE)$size)
  css = paste(get_css(topic_ids), collapse="\n")
  parts = unlist(strsplit(html, "$CONTENT$", fixed = T))
  footer = parts[1]
  gsub("$CSS$", css, footer, fixed = T)
}

html_footer <- function() {
  TEMPLATE = system.file("template/template.html", package="topicbrowser", mustWork=T)
  html = readChar(TEMPLATE, file.info(TEMPLATE)$size)
  parts = unlist(strsplit(html, "$CONTENT$", fixed = T))
  parts[2]
}

#' Render the tabbed html containing overview and individual tabs
#'
#' This function does not return the html, but rather cats it directly
#' So you might wish to use wrap_html or sink to capture the output
#'
#' @param info the cluster_info object (list)
#' @export
render_html <- function(info) {
  # tabs
  cat('<ul class="nav nav-tabs" role="tablist" id="topictab">\n')
  cat('<li class="active"><a href="#home" role="tab" data-toggle="tab">Overview</a></li>\n')
  for (topic_id in info$topic_ids)
    cat('<li><a href="#t', topic_id, '" role="tab" data-toggle="tab">Topic ', topic_id, '</a></li>\n', sep = "")
  cat('</ul>\n')

  # content
  cat('<div class="tab-content">\n')
  cat('<div class="tab-pane fade in active" id="home">\n')
  message("Rendering overview")
  render_overview(info)
  cat('</div>\n')

  for (topic_id in info$topic_ids) {
    message("Rendering topic ", topic_id)
    cat('<div class="tab-pane fade in" id="t',topic_id,'">\n', sep="")
    render_topic(topic_id, info)
    cat('</div>\n')
  }
  cat('</div>\n')
}

tab_content_html <- function(id, content) {
  paste('<div class="tab-pane fade" id="', id, '">', content, '</div>', sep="")
}

tab_html <- function(id, name) {
  paste('<li><a href="#', id, '" role="tab" data-toggle="tab">', name, '</a></li>', sep="")
}

#' Render the index page
#'
#' This function does not return the html, but rather cats it directly
#' So you might wish to use wrap_html or sink to capture the output
#'
#' @param info the cluster_info object (list)
#' @export
render_overview <- function(info) {
  for(topic_id in info$topic_ids){
    cat('<a href="#" onclick="showTab(', topic_id, ');">', sep='')
    cat_plot(plot.topicoverview(info$topics_per_term, info$topics_per_doc, info$meta$date, topic_id, info$date_interval), width=300)
    cat("</a>")
  }
}

#' Plot the plot in plotfun and cat as a base64 encoded <img>
cat_plot <- function(plotfun, ...)  {
  png  = plot_to_file(plotfun, ...)
  cat(img(png))
}

#' Plot the plot in plotfun to a tempfile and return the filename
plot_to_file <- function(plotfun, width=500, height=width) {
  fn = tempfile(fileext = ".png", pattern="plot_")
  png(filename = fn, width = width, height = height)
  tryCatch(force(plotfun), finally=dev.off())
  fn
}

#' Render a single topic
#'
#' This function does not return the html, but rather cats it directly
#' So you might wish to use wrap_html or sink to capture the output
#'
#'
#' @param topic_id: the topic id to render
#' @param info the cluster_info object (list)
#' @export
render_topic <- function(topic_id, info, nmaxdoc=10) {
  cat("<h1>Topic", topic_id, "</h1>")
  cat_plot(plot.wordcloud(info$topics_per_term, topic_nr = topic_id, wordsize_scale = .5), width=500)
  cat("<h2>Over time</h3>")
  cat_plot(plot.time(info$topics_per_doc, topic_nr = topic_id, time_var =info$meta$date, date_interval = info$date_interval, value = 'relative'),
           width=500, height=200)
  for (var in setdiff(colnames(info$meta), c("id", "date"))) {
    cat("<h2>Per",var,"</h2>")
    cat_plot(topics.plot.category(info$topics_per_doc, topic_id, info$meta[[var]]),
             width=500, height=200)
  }

  cat("<h2>Articles</h2>")
  topicass = info$topics_per_doc[topic_id,]
  docs = names(head(topicass[order(-topicass)], n=nmaxdoc))

  for (doc in docs) render_article(doc, info, maxwords=100)
}

#' Render a single article
#'
#' This function does not return the html, but rather cats it directly
#' So you might wish to use wrap_html or sink to capture the output
#'
#'
#' @param doc: the document id (dtm rowname) to render
#' @param info the cluster_info object (list)
#' @export
render_article <- function(doc, info, maxwords=NULL) {
  # header
  cat('<h3>', doc, '</h3>')

  # meta
  cat('<table>')
  for (name in colnames(info$meta))
    cat('<tr><th>', name, '</th><td>', as.character(info$meta[info$meta$id == doc, name]), '</td></tr>')
  cat('</table>')

  # render words
  cap <- function(x, n) if (is.null(n)) x else head(x,n)
  tok = info$tokens[info$tokens$aid == doc, ]
  tok = tok[cap(order(tok$id), maxwords), ]
  wa = info$wordassignments[info$wordassignments$aid == doc, c("term", "topic")]
  topics = wa$topic[match(tok$term, wa$term)]
  cat(tagTokens(tok$word, topics))
}

### HTML functions

#' Get the filename for a topic
topic_filename <- function(topic) paste("t", topic, ".html", sep="")

#' Add title, href, and class to tokens
#'
#' @param tokens: a vector of words
#' @param topics: a vector of topics, can include NA's for words without a topic
#' @return a vector of raw html per token
tagTokens <- function(tokens, topics){
  tokens = as.character(tokens)
  tokens = gsub("`", "'", tokens)

  # add hrefs
  tokens = ifelse(is.na(topics), tokens, paste("<a href='#'>", tokens, "</a>", sep=""))
  # add span class and title
  tokens = paste("<span",
                 ifelse(is.na(topics),
                        " class='notopic'",
                        paste(" onclick='showTab(",topics,")' class='t", topics, "' title='", topics, "'", sep="")),
                 ">", tokens, "</span>", sep="")
  tokens
}



get_css <- function(topic_ids) {
  CSS_TEMPLATE = system.file("template/style.css", package="topicbrowser", mustWork=T)
  css = readLines(CSS_TEMPLATE, warn=F)
  colo = substr(rainbow(length(topic_ids)), 1,7)
  colorcss = paste(".t", topic_ids, " {background-color: ",colo, "}", sep="")
  c(css, colorcss)
}


