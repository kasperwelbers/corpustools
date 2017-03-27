About this package
============

This git contains the development version of corpustools. Note that a previous version of corpustools has been around for quite a while, and is still available on https://github.com/kasperwelbers/corpus-tools (the current page dropped the hyphen, which was silly anyway since R does not accept hyphens in package names). We decided to make a fresh start due to the introduction of the tCorpus R6 class that is now the backbone of corpustools, which required a major overhaul of the package structure. 

Also, where the first corpustools was mostly a collection of usefull functions, driven by the ad-hoc needs of collegues, students and ourselves, the new corpustools is a more concerted effort to create a text analysis package. The main reason for this endeavour is that amidst the existing text analysis packages in R we did not find a good class for a text corpus where the underlying data is a token list. We often use advanced pre-processing techniques (that are not yet available in R), and we wanted a way to bind all token and document meta information in a single object. The new tCorpus class serves this purpose.

Given the new tCorpus class, it made sense to pour a lot of the code that we developed (including the entire semnet package) for this type of data together. In that sense, there is some overlap between corpustools and other text analysis packages such as quanteda. Still, due to the dedicated focus on the token list as the core data, corpustools has ample differences, making it more or less suitable depending on the use case. One of the current development goals is to support better interaction with other packages to better enable users to switch to and from packages based on their needs.

Corpustools
============

The corpustools package offers various tools for anayzing text corpora. The backbone is the tCorpus R6 class, which offers a flexible environment to store tokenized text data together with document meta data. Since data in this environment can only be accessed through methods, we can make several key assumptions about how the data is organized, which allows us to provide an extensive suite of efficient methods. These methods range from corpus management tools such as pre-processing, subsetting, Boolean queries (lucene) and deduplication, to analysis techniques such as corpus comparison, document comparison, semantic network analysis and topic modeling. Furthermore, by using tokenized texts as the backbone, it is made easy to reconstruct texts for a qualitative analysis and/or validation of the results of computational text analysis methods (e.g., topic browsers, keyword-in-context lists, texts with highlighted segments for search results or document comparisons). 

One of the primary goals of corpustools is to make computational text analysis available and intuitive for users that are not experienced programmers. Notablly, the authors are both active as researchers in the social sciences, and strive to promote the use of computational text analysis as a research method. This is also why we double down on the feature to reconstruct the original texts to enable a more qualitative investigation and validation of results. 

In time, we will add tutorials for the different analysis techniques. Currently, however, you are looking at the development version. Since the introduction of the tCorpus class pretty much everything about the first corpustools version has changed, so we still have to add a lot of new documentation and rewrite the tutorials.


Getting started
============

You can install corpustools directly from github:

```{r}
install_github("kasperwelbers/corpustools")
library(tcorpus)
```

tCorpus is built around the tCorpus R6 class. This means that the methods for working with the tCorpus object are accessed with the dollar symbol (e.g., tc$subset(...), tc$semnet(...)). An overview of the methods, including links with more detailed method documentation, is available through a central documentation hub:

```{r}
?tCorpus
```


