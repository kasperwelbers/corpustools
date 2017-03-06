tCorpus
============

tCorpus is a new corpus class that we're working on, that will at some point (soon...) be integrated in corpustools. 

----

You can install tCorpus directly from github:

```{r}
library(tcorpus)
install_github("kasperwelbers/tcorpus")
```

tCorpus is build around the tCorpus R6 class. This means that the methods for working with the tCorpus object are accessed with the dollar symbol (e.g., tc$subset(...), tc$semnet(...)). An overview of the methods, including links with more detailed method documentation, is available from:

```{r}
?tCorpus
```


