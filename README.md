# Waterpoint Functionality Prediction in Tanzania
This project focuses on predicting the operational status of water pumps in Tanzania for the Pump it Up competition hosted by DrivenData.
We provide here an instruction of repeating the code:
0. This path '.libPaths("/dss/dsshome1/01/ra59qow2/R/x86_64-pc-linux-gnu-library/4.3")' in the scripts is an absolute cluster-path, please change it to your path if you want to repeat the code.

1.Save the four raw data sets (Excel CSV files) in the `data` folder and run `setup.R`

2.Open the 'data_preprocessing/' folder and find our scripts for data engineering:
  'data_combining.R' join the three tables(train values/labels, test values) into one.
  'data_cleaning.R' provides initial data cleaning and several simple data engineering work.
  'funder.R' specifically deals with the high cardinality of the text features.
  'spatial_preprocessing.R' handles with spatial enhancement of the features.
  'data_spatial_large.R' creates our enhanced dataset and 'cluster_eva.R' tests the silhouette score of the spatial cluster.
  
3.Open the 'baseline/' folder and find our scripts for baseline benchmarking-evaluation:
  'bs_non_imp.R' evaluates the performance of baseline models with non-imputed dataset.
  'bs_imp_.. .R' evaluates the performance of baseline models with non-imputed dataset, the result is saved for each model seperately.

4.Open the 'tuning/' folder and find our scripts for tuning Catboost, XGBoost and Ranger:
  All the '..._tuning.R' files tuned base models on the imputed-enhanced dataset and provides the tuning archive and top_10 params of the tuning process.

5.Open the 'baseline_submitted/' folder and find our scripts for submitting base models:
   All that end with '..._s.R' output the submission file for non-tuned models.
   All that end with '..._ts.R' output the submission file for tuned models.

6.Open the 'stack/' folder and find our scripts for submitting stack models with different base and super learners combinations:
  Specifically, 'stack_7.R' provides the best submission score.

7.Open the 'cascaded/' folder and find our design for reproduct the cascading modeling ideas.
