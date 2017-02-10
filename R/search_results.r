## not yet updated!!


#' Title
#'
#' @param tokens
#' @param hits
#' @param tokenfreq
#' @param keywordIC
#' @param kwic_nwords
#' @param kwic_sample
#' @param random_sample
#' @param feature.col
#' @param hitcount
#'
#' @export
reportSummary <- function(tokens, hits=NULL, token_i=NULL, hitcount=T, tokenfreq=T, keywordIC=T, kwic_nwords=10, kwic_sample=10, random_sample=T, doc.col=getOption('doc.col','doc_id'), position.col=getOption('position.col','position'), feature.col=getOption('wor.col','word')){
  if(!is.null(token_i)) hits = droplevels(tokens[token_i,])
  out = list()

  ## report number of hits and articles
  if(hitcount){
    out[['hitcount']] = c(hits=nrow(hits), docs=length(unique(hits[,doc.col])))
  }

  ## report token frequency
  if(tokenfreq & nrow(hits) > 0) {
    out[['tokenfreq']] = reportTokenFreq(hits, doc.col=doc.col, feature.col=feature.col)
  }

  ## report keywords in confeature
  if(keywordIC & nrow(hits) > 0) {
    cat('\n')
    out[['keywordIC']] = reportKWIC(tokens, hits, nwords=kwic_nwords, nsample=kwic_sample, random_sample=random_sample, doc.col=doc.col, position.col=position.col, feature.col=feature.col)
  }

  out
}

#' Title
#'
#' @param tokens
#' @param hits
#'
#' @export
reportHitcount <- function(tokens, hits=NULL, token_i=NULL, doc.col=getOption('doc.col','doc.id')){
  if(!is.null(token_i)) hits = droplevels(tokens[token_i,])
  nhits = nrow(hits)
  narts = length(unique(hits[,doc.col]))
  sprintf('%s hit%s in %s article%s (N = %s)', nhits, ifelse(nhits==1, '', 's'),
          narts, ifelse(narts==1, '', 's'),
          length(unique(tokens[,doc.col])))
}



#' Title
#'
#' @param hits
#' @param feature.col
#'
#' @export
reportTokenFreq <- function(hits, token_i = NULL, doc.col=getOption('doc.col','doc.col'), feature.col=get.Option('word','word')){
  if(!is.null(token_i)) hits = droplevels(tokens[token_i,])
  termfreq = aggregate(list(hits=hits[,doc.col]), by=list(code=hits$code, word=as.character(hits[,feature.col])), FUN=function(x) cbind(length(x), length(unique(x))))
  termfreq = data.frame(code = termfreq$code, word = termfreq$word, freq = termfreq$hits[,1], doc_freq = termfreq$hits[,2])
  termfreq$doc_pct = round(termfreq$doc_freq / length(unique(hits[,doc.col])) * 100,1)
  termfreq = termfreq[order(-termfreq$doc_pct),]
  termfreq
}

#' Title
#'
#' @param tokens
#' @param hits
#' @param nwords
#' @param nsample
#' @param random_sample
#' @param feature.col
#'
#' @export
reportKWIC <- function(tc, hits=NULL, token_i=NULL, nwords=10, nsample=10, random_sample=T, doc.col=getOption('doc.col','doc_id'), position.col=getOption('position.col','position'), feature.col=getOption('wor.col','word')){
  if(!is.null(token_i)) hits = droplevels(tokens[token_i,])

  if(!is.null(nsample)) {
    if(random_sample) hits = hits[sample(1:nrow(hits), nrow(hits)),]
    hits = head(hits, nsample)
  }
  hits$kwic = getKwic(tokens, hits, token_i, nwords = nwords, nsample=nsample, doc.col=doc.col, position.col=position.col, feature.col=feature.col)

  #for(doc_id in unique(hits[,doc.col])){
  #  ahits = hits[hits[,doc.col] == doc_id,]
  #  metanames = c(doc.col, position.col)
  #  for(metaname in metanames){
  #    message(paste(metaname, paste(unique(ahits[,metaname]), collapse=' / '), sep=': '))
  #  }
  #  print(ahits$kwic)
  #}
  hits[,c('code', doc.col, position.col, 'kwic')]
}

#' Get keyword-in-confeature from a token list
#'
#' @param tokens a data frame of tokens containing columns for document id (doc_id), feature position (position) and feature string (column name can be specified in feature.col, defaults to 'word').
#' @param nwords the number of words in front and after the keyword
#' @param hits
#' @param prettypaste
#' @param feature.col a character string giving the name of the term string column
#'
#' @return A data.frame with the keyword in confeature
#' @export
getKwic <- function(tc, hits=NULL, token_i=NULL, nwords=10, nsample=NA, prettypaste=T){
  if(class(token_i) == 'logical') token_i = which(token_i)
  ## first filter tokens on document id (to speed up computation)

  if(!is.null(token_i)) {
    if(!is.na(nsample)) token_i = sample(token_i, nsample)
    tokens$token_i = F
    tokens$token_i[token_i] = T
    tokens = tokens[tokens[,doc.col] %in% unique(tokens[token_i, doc.col]),]
    token_i = which(tokens$token_i)
  } else {
    if(!is.na(nsample) & nsample < nrow(hits)) {
      hits = hits[sample(1:nrow(hits), nsample),]
    }
    tokens = tokens[tokens[,doc.col] %in% unique(hits[,doc.col]),]
    token_i = tokenLookup(tokens, hits[,doc.col], hits[,position.col], doc.col, position.col)
  }

  kwicldply <- function(i, doc_ids, words, nwords){
    doc_id = doc_ids[i]

    sent_i = (i-nwords):(i+nwords)
    keyword_i = if(min(sent_i) < 0) min(sent_i) + nwords else nwords + 1

    sent_i = sent_i[sent_i >= 0 & sent_i <= length(words)]
    sent = as.character(words[sent_i])

    sent = gsub('\\[|\\]', '', sent)
    sent[keyword_i] = sprintf('[%s]', sent[keyword_i])
    sent = sent[doc_ids[sent_i] == doc_id] # only show confeature words if they occur in the same article
    data.frame(doc_id=doc_id, kwic=paste(sent, collapse=' '))
  }
  o = ldply(token_i, kwicldply, doc_ids=tokens[,doc.col], words=tokens[,feature.col], nwords=nwords)

  if(prettypaste) o$kwic = prettyKWIC(o$kwic)
  o$kwic
}

prettyKWIC <- function(x){
  x = gsub('_', ' ', x)
  x = gsub('  ', ' ', x)
  x = gsub(" ([.,?!:;>)])", '\\1', x)
  x = gsub('([(<]) ', '\\1', x)
  x = sprintf('...%s...', x)
}

tokenLookup <- function(tokens, doc_id, position, doc.col=getOption('doc.col','doc_id'), position.col=getOption('position.col','position')){
  tokens$i = 1:nrow(tokens)
  tokens = tokens[tokens[,doc.col] %in% unique(doc_id), c('i', doc.col, position.col)]
  which.sub = match(paste(doc_id, position, sep='___'),
                    paste(tokens[,doc.col], tokens[,position.col], sep='___'))
  tokens$i[which.sub]
}

#' Title
#'
#' @param tokens
#' @param hits.x
#' @param hits.y
#'
#' @return
#' @export
#'
#' @examples
compareHits <- function(tokens, hits.x, hits.y, doc.col, position.col){
  id.x = paste(hits.x$doc_id, hits.x$position, sep='---')
  id.y = paste(hits.y$doc_id, hits.y$position, sep='---')
  x = length(id.x)
  y = length(id.y)
  x_not_y = length(setdiff(id.x, id.y))
  y_not_x = length(setdiff(id.y, id.x))
  x_and_y = length(intersect(id.x, id.y))
}

#' Title
#'
#' @param tokens
#' @param hits.x
#' @param hits.y
#' @param ...
#'
#' @export
XandY <- function(hits.x, hits.y, doc.col=getOption('doc.col','doc_id'), position.col=getOption('position.col','position')){
  id.x = paste(hits.x$doc_id, hits.x$position, sep='---')
  id.y = paste(hits.y$doc_id, hits.y$position, sep='---')
  hits.x[id.x %in% id.y,]
}

#' Title
#'
#' @param tokens
#' @param hits.x
#' @param hits.y
#' @param ...
#'
#' @export
XnotY <- function(hits.x, hits.y, doc.col=getOption('doc.col','doc_id'), position.col=getOption('position.col','position')){
  id.x = paste(hits.x$doc_id, hits.x$position, sep='---')
  id.y = paste(hits.y$doc_id, hits.y$position, sep='---')
  hits.x[!id.x %in% id.y,]
}

#' Title
#'
#' @param tokens
#' @param hits.x
#' @param hits.y
#' @param ...
#'
#' @export
YnotX <- function(hits.x, hits.y, doc.col=getOption('doc.col','doc_id'), position.col=getOption('position.col','position')){
  id.x = paste(hits.x$doc_id, hits.x$position, sep='---')
  id.y = paste(hits.y$doc_id, hits.y$position, sep='---')
  hits.y[!id.y %in% id.x,]
}


#' Title
#'
#' @param tokens
#' @param hits.x
#' @param hits.y
#' @param ...
#'
#' @export
reportXandY <- function(tokens, hits.x, hits.y, doc.col=getOption('doc.col','doc_id'), position.col=getOption('position.col','position'), feature.col=getOption('wor.col','word'), ...){
  reportSummary(tokens, XandY(hits.x, hits.y, doc.col, position.col), doc.col=doc.col, position.col=position.col, feature.col=feature.col, ...)
}

#' Title
#'
#' @param tokens
#' @param hits.x
#' @param hits.y
#' @param ...
#'
#' @export
reportXnotY <- function(tokens, hits.x, hits.y, doc.col=getOption('doc.col','doc_id'), position.col=getOption('position.col','position'), feature.col=getOption('wor.col','word'), ...){
  reportSummary(tokens, XnotY(hits.x, hits.y, doc.col, position.col), doc.col=doc.col, position.col=position.col, feature.col=feature.col, ...)
}

#' Title
#'
#' @param tokens
#' @param hits.x
#' @param hits.y
#' @param ...
#'
#' @export
reportYnotX <- function(tokens, hits.x, hits.y, doc.col=getOption('doc.col','doc_id'), position.col=getOption('position.col','position'), feature.col=getOption('wor.col','word'), ...){
  reportSummary(tokens, YandX(hits.x, hits.y, doc.col, position.col), doc.col=doc.col, position.col=position.col, feature.col=feature.col, ...)
}



