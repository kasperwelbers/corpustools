#devtools::install_github('kasperwelbers/tokenbrowser')
library(tokenbrowser)
data(sotu)

head(tokens)
head(meta)

## highlighting
highlight = nchar(as.character(tokens$token))
highlight[highlight > 15] = 15
highlight = highlight / max(highlight)

url = highlighted_reader(tokens, value = highlight, meta)
browseURL(url)

## scaling
scale = rescale_var(highlight, new_min = -1, new_max = 1)
scale[abs(scale) < 0.7] = NA

url = colorscaled_reader(tokens, value = scale, meta=meta)
browseURL(url)

## topics
topic = match(tokens$pos, c('N','M','V'))
url = topic_reader(tokens, topic=topic, meta=meta)
browseURL(url)
