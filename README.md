[![Travis-CI Build Status](https://travis-ci.org/kasperwelbers/corpustools.svg?branch=master)](https://travis-ci.org/kasperwelbers/corpustools)

About this package
============

It should be noted that a previous version of corpustools has been around for quite a while, and is still available on https://github.com/kasperwelbers/corpus-tools (the current page dropped the hyphen, which was silly anyway since R does not accept hyphens in package names). We decided to make a fresh start due to the introduction of the tCorpus R6 class that is now the backbone of corpustools, due to which a major overhaul of the package structure was needed. 

Also, where the first corpustools was mostly a collection of convenient functions, driven by the ad-hoc needs of collegues, students and ourselves, the new corpustools is a more concerted effort. The main reason for this endeavour is that amidst the existing text analysis packages in R we did not find a class for a text corpus that fit our specific needs. We often use advanced pre-processing techniques from external tools (CoreNLP, Alpino), and we wanted a way to bind the detailed token output of these tools with document meta information in a single object. The new tCorpus class serves this purpose, and offers methods to manage, explore, analyze and visualize this data. 

Corpustools
============

The corpustools package offers various tools for anayzing text corpora. The backbone is the tCorpus R6 class, which offers features ranging from corpus management tools such as pre-processing, subsetting, Boolean (Lucene) queries and deduplication, to analysis techniques such as corpus comparison, document comparison, semantic network analysis and topic modeling. Furthermore, by using tokenized texts as the backbone, it is made easy to reconstruct texts for a qualitative analysis and/or validation of the results of computational text analysis methods (e.g., topic browsers, keyword-in-context lists, texts with highlighted segments for search results or document comparisons). 

One of the primary goals of corpustools is to make computational text analysis available and intuitive for users that are not experienced programmers. Notably, the authors are both active as researchers in the social sciences, and strive to promote the use of computational text analysis as a research method. This is also why we double down on the feature to reconstruct the original texts to enable a more qualitative investigation and validation of results. 

In time, we will add tutorials for the different analysis techniques. With the introduction of the tCorpus class pretty much everything about the first corpustools version has changed, so we still have to add a lot of new documentation and rewrite the tutorials.


Getting started
============

You can install corpustools directly from github:

```{r}
install_github("kasperwelbers/corpustools")
library(corpustools)
```

tCorpus is built around the tCorpus R6 class. This means that the methods for working with the tCorpus object are accessed with the dollar symbol (e.g., tc$subset(...), tc$semnet(...)). An overview of the methods, including links with more detailed method documentation, is available through a central documentation hub:

```{r}
?tCorpus
```


