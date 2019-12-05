testthat::context('Search Features')


test_that("Query search works", {
  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte')
  tc = create_tcorpus(text, doc_id = c('a','b','c'), split_sentences = T)

  hits = search_features(tc, '"renewable fuel" AND better')
  expect_equal(as.character(hits$hits$feature), c('Renewable','fuel','better'))

  ## simple keyword only
  hits = search_features(tc, 'fuel')
  tc$code_features('fuel')
  expect_equal(as.character(hits$hits$feature), c('fuel','fuel'))

  ## aggregating results
  res = count_tcorpus(tc, hits=hits)
  expect_equal(colnames(res), c('group','N','V', 'query_1'))

  ## ensure aggregate counts correctly (ignoring duplicateh hit_ids within codes)
  hits = search_features(tc, c('fuel', 'mark'))  ## ensure hit_ids are counted well
  agg = count_tcorpus(tc, hits=hits)
  expect_true(agg$query_1 == 2)
  expect_true(agg$query_2 == 1)

  ## multitoken keywords
  hits = search_features(tc, '"a fueled debate"')
  expect_equal(as.character(hits$hits$feature), c('A','fueled', 'debate'))

  ## proximity keywords
  hits = search_features(tc, '"renewable fuels"~10')
  expect_equal(as.character(hits$hits$feature), c('Renewable','fuels'))
  hits = search_features(tc, '"renewable fuels"~2')
  expect_true(nrow(hits$hits) == 0)

  ## two keywords
  hits = search_features(tc, 'fuel fuels')
  expect_equal(as.character(hits$hits$feature), c('fuel','fuels','fuel'))

  ## keyword with wildcard
  hits = search_features(tc, 'fuel*')
  expect_equal(as.character(hits$hits$feature), c('fuel','fuels','fueled','fuel'))
  hits = search_features(tc, 'fue?s')
  expect_equal(as.character(hits$hits$feature), c('fuels'))

  ## case sensitive flag
  hits = search_features(tc, 'Rutte~s')
  expect_true(nrow(hits$hits) == 2)
  hits = search_features(tc, 'rutte~s')
  expect_true(nrow(hits$hits) == 0)

  ## keyword with AND
  hits = search_features(tc, 'fuel* AND (renewable green clean)')
  expect_equal(as.character(hits$hits$feature), c('Renewable','fuel')) ## second fuel not matched, because it looks for full unique query matches

  hits = search_features(tc, 'fuel* AND (renewable green clean)', mode = 'features')  ##  in feature mode, all features for which the query is satisfied are returned
  expect_equal(as.character(hits$hits$feature), c('Renewable','fuel','fuels')) ## second fuel not matched, because it looks for full unique query matches

  hits = search_features(tc, 'fuel* AND (renewable~g green~g clean~g)')  ## use ~i for ghost search. has to match, but will not be returned as feature
  expect_equal(as.character(hits$hits$feature), c('fuel', 'fuels'))

  hits = search_features(tc, 'fuel* AND (renewable green clean)~g')  ## use ~g or ~s flags on parentheses to use them on all nested terms
  expect_equal(as.character(hits$hits$feature), c('fuel','fuels'))

  ## multitoken and proximity conditions
  hits = search_features(tc, 'fuel AND ("renewable fuel" OR "a debate"~3)~g')
  expect_equal(as.character(hits$hits$feature), c('fuel','fuel'))

  ## Normally, only full and unique matches for queries are returned, which is best for accurate counting of hits
  ## For other purposes, such as coding tokens, it is better to return all features for which the query is true.
  ## e.g., query "a AND b" on text "a b b" will in the first case only match the first a and b, but the second be is ignored because it is not a complete match
  ## in the second case, the second b is also returned.

  hits = search_features(tc, '"mark rutte"~10') ## only matches first full occurence
  hits$hits
  expect_equal(as.character(hits$hits$feature), c('Mark','Rutte'))

  hits = search_features(tc, '"mark rutte"~10', mode = 'features') ## matches all features for which query is true
  expect_equal(as.character(hits$hits$feature), c('Mark','Rutte','Rutte'))

  ## using codes, either in query or via parameter
  hits = search_features(tc, 'Mark Rutte Label# "mark rutte"~10') ## only matches first full occurence
  expect_equal(as.character(hits$hits$code)[1], 'Mark Rutte Label')

  hits = search_features(tc, '"mark rutte"~10', code = 'Mark Rutte Label') ## only matches first full occurence
  expect_equal(as.character(hits$hits$code)[1], 'Mark Rutte Label')

  ## multiple queries
  queries = data.frame(code=c('renewable fuel', 'mark rutte', 'debate'),
                       query = c('fuel* AND (renewable green clean)', '"mark rutte"~2', 'debate'))
  hits = search_features(tc, queries$query, code=queries$code)
  expect_equal(nrow(hits$hits), 5)

  ## kwic
  hits = search_features(tc, 'better')
  kw = get_kwic(tc, hits=hits, ntokens=2)
  expect_true(kw$kwic == '...fuel is <better> than fossil...')
  kw = get_kwic(tc, query = 'better', ntokens=2)
  expect_true(kw$kwic == '...fuel is <better> than fossil...')

  ## kwic with multitoken queries
  kw = get_kwic(tc, query = '"renewable fuels"~10', nsample = NA) ## without gap
  expect_equal(kw$feature, 'Renewable -> fuels')
  kw = get_kwic(tc, query = c('"renewable fuels"~10'), ntokens = 2) ## with gap
  expect_true(grepl('[...]', kw$kwic))

  ## complex BOOLEAN unique hits
  tc = create_tcorpus("A B C")

  f = search_features(tc, 'A AND (B C)', mode = 'features')$hits$feature  ## feature mode matches everything
  expect_equal(as.character(f), c('A','B','C'))
  f = search_features(tc, '(B C) AND A')$hits$feature                     ## in unique_hits mode, only one hits is found, because there is only one A.
  expect_equal(as.character(f), c('A','B'))
  f = search_features(tc, '(B C) AND A~g')$hits$feature                   ## ghost terms can be reused
  expect_equal(as.character(f), c('B','C'))
  f = search_features(tc, 'A~g AND (B C)')$hits$feature                   ## check whether ghost term can be first term (requires repeating of loop)
  expect_equal(as.character(f), c('B','C'))

  ## match longest
  tc = create_tcorpus('mr. bob smith')

  hits = search_features(tc, 'Bob OR "bob smith"')  # match longest by default (bob smith)
  expect_true(length(hits$hits$feature) == 2)

  hits = search_features(tc, 'Bob OR "bob smith"', keep_longest = F) # otherwise, match first (bob)
  expect_true(length(hits$hits$feature) == 1)
})

