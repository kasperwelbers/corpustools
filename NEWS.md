CRAN v0.4.2 (Release date: 2020-01-22)
================

This is a minor update that mainly made dictionary search much faster (enabling the use of huge dictionary resources, such as named entity lists). 

CRAN v0.4.1 (Release date: 2019-11-18)
================
  
This update makes some changes to the API. 

* Many of the functionalities that were before provided as R6 methods of the tCorpus are now replaced with regular functions. The R6 methods are now only used in cases where the tCorpus is modified by reference. 
* Working with the R6 methods is now optional for operations that modify the tCorpus, such as subsetting. Before, the default approach was to modify the tCorpus by reference, but with the option to make copies. Now, this behavior is split up into functions and R6 methods. For instance, instead of the R6 method 'subset' that changes the tCorpus by reference, the generic subset (S3) function returns the subset without modifying the input tCorpus. Using the R6 methods is still recommended, but unless large corpora are used it's not that important. 

Several new features have been added, and there is (finally) a vignette.

* A versatile browse_texts function has been added that makes it easy to create full-text browsers from a tCorpus, using token features or annotations (e.g., search results, topics from a topic model, sentiment scores) to highlight and navigate the texts.
* The udpipe wrapper in create_tcorpus now makes persistent caches for the last three calls, that store results for every batch (100 docs) that has been processed. This makes it easy to parse many documents across sessions (and continue if stuff freezes), and is overall more user friendly.
* A vignette has been added that demonstrates the main features, and discusses the reason for and use of the R6 methods.
