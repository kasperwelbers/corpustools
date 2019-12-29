[![Travis-CI Build Status](https://travis-ci.org/kasperwelbers/corpustools.svg?branch=master)](https://travis-ci.org/kasperwelbers/corpustools)

Corpustools
============

The corpustools package offers various tools for anayzing text corpora. The backbone is the tCorpus R6 class, which offers features ranging from corpus management tools such as pre-processing, subsetting, Boolean (Lucene) queries and deduplication, to analysis techniques such as corpus comparison, document comparison, semantic network analysis and topic modeling. Furthermore, by using tokenized texts as the backbone, it is made easy to reconstruct texts for a qualitative analysis and/or validation of the results of computational text analysis methods (e.g., topic browsers, keyword-in-context lists, texts with highlighted segments for search results or document comparisons). 

One of the primary goals of corpustools is to make computational text analysis available and intuitive for users that are not experienced programmers. Notably, the authors are both active as researchers in the social sciences, and strive to promote the use of computational text analysis as a research method. This is also why we double down on the feature to reconstruct the original texts to enable a more qualitative investigation and validation of results. 


Getting started
============

You can install corpustools directly from CRAN

```{r}
install.packages('corpustools')
```

Or you can install the development version from Github

```{r}
install_github("kasperwelbers/corpustools")
library(corpustools)
```

A vignette is provided ([HTML version](http://htmlpreview.github.io/?https://github.com/kasperwelbers/corpustools/blob/master/vignettes/corpustools.html)) with instructions on how to use corpustools and an overview of usefull features.

```{r}
vignette('corpustools')
```
