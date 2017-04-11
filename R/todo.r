
### all functions within methods that have the clone parameter MUST work by reference only!!
### check this thoroughly!!

## add the add_data and add_tokens method. This can simply be a combination of create_tcorpus/tokens_to_tcorpus and merge_tcorpua

## in time, replace the restrictions on data manipulation (e.g., in transform, within) with more elegant checks. Especially the within selection is pretty hacky
## update: perhaps remove transform and within altogether now that modifying by assignment is possible

## add information gain measure for semnet. add function to select only the top words that contain most information about a given term (as an ego network based filter)

## add a from_csv argument to create_tcorpus, that uses the excellent data.table::fread
## (also, in time add a from_csv argument for creating huge shattered tcorpora from csv)

## add boilerplate and remove_boilerplate functions. The idea is then to look for long identical sequences of words that occurr across many documents (and optionally, over time).
## Possible method: for long boilerplate, look for 5-grams, then given detected 5-grams look for 6-grams, 7-grams etc. till a given length, and mark as boilerplate if it occurs in more than a given pct of articles. (possibly use 90% overlap instead of 100%)
## also add option to give meta variables, that will be used to do this per group (since boilerplate is often medium specific)
## also take word possitions into account

## if multiple text columns are given to create_tcorpus, add a column that notes what part of the document it is (e.g., headline, body)
## Kohlschutter, C., Fankhauser, P., and Nejdl, W. (2010). Boilerplate detection using shallow text features. InProceedings of the 3rdACM International Conference on Web Search and Data Mining (WSDM ’10). New York, NY, USA: ACM, 441–450.
