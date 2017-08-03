create_feature_index <- function(tc, feature, context_level=c('document','sentence'), max_window_size=100, as_ascii=F){
  context_level = match.arg(context_level)
  feature_index = data.table::data.table(feature = tc$get(feature))
  if (!methods::is(feature_index$feature, 'factor')) feature_index$feature = fast_factor(feature_index$feature)
  if (as_ascii) levels(feature_index$feature) = iconv(levels(feature_index$feature), to='ASCII//TRANSLIT')
  feature_index$i = 1:nrow(feature_index)
  feature_index$global_i = get_global_i(tc, context_level, max_window_size)
  levels(feature_index$feature) = gsub('_', ' ', levels(feature_index$feature), fixed=T)
  setkey(feature_index, 'feature')
  feature_index
}
