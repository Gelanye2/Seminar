# Waterpoint Functionality Prediction in Tanzania

This project predicts the operational status of water pumps in Tanzania for the **Pump it Up** competition (DrivenData).  
Below are the **instructions to reproduce** our results.

> **Note 0.** The path `.libPaths("/dss/dsshome1/01/ra59qow2/R/x86_64-pc-linux-gnu-library/4.3")` in some scripts is a cluster-specific absolute path. Please change it to your own path.

1. **Setup**
   - Save the four raw datasets (CSV files) in the `data/` folder.
   - Run `setup.R`.

2. **Data preprocessing** (see `data_preprocessing/`)
   - `data_combining.R` — joins the three tables (train values/labels, test values) into one.
   - `data_cleaning.R` — performs initial cleaning and simple feature engineering.
   - `funder.R` — handles the high cardinality of text features.
   - `spatial_preprocessing.R` — adds spatially enhanced features.
   - `data_spatial_large.R` — creates the enhanced dataset.
   - `cluster_eva.R` — evaluates the silhouette score of the spatial clusters.

3. **Baselines** (see `baseline/`)
   - `bs_non_imp.R` — evaluates baseline models on the non-imputed dataset.
   - `bs_imp_*.R` — evaluates baseline models on the imputed dataset; results are saved per model.

4. **Tuning** (see `tuning/`)
   - `*_tuning.R` — tunes **CatBoost**, **XGBoost**, and **Ranger** on the imputed-enhanced dataset and saves the tuning archive and top-10 parameter sets.

5. **Submissions for base models** (see `baseline_submitted/`)
   - Files ending with `_s.R` — output submission files for **non-tuned** models.
   - Files ending with `_ts.R` — output submission files for **tuned** models.

6. **Stacking** (see `stack/`)
   - Scripts generate submissions for different base & super-learner combinations.
   - `stack_7.R` yields the best submission score.

7. **Cascaded modeling** (see `cascaded/`)
   - Contains our design and code to **reproduce** the cascading modeling approach.
