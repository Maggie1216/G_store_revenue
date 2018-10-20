# G_store_revenue
Kaggle project 201809-201810

## Files and Timeline
### Updating
Coding.R

All codes, including data processing, eda, modeling and so on.
### Created 20180926
workspace_10000 samples in test, all train with y + datetime and NAs transfered to NAs.RData

-All train data with y, datatime dealt with and NAs transfered to NAs(those variables with all NAs are removed); 

-Test data, sampled 10000 from population
### Created 20180928
workspace_10000 samples in test, all train with y + datetime and NAs transfered to NAs 0.85 + adjustments.RData

-Threshold adjusted to 0.85, which means when that percentage of NA exceeds 15%, this feature will be dropped;

-Saved a few variables, with NA has real meaning, such as isTrueDirect (NA means FALSE) and firstvisit (NA means 0);

-Deleted a deplicated value, the Zulia case.
### Created 20181006
workspace_10000 samples in test, all train with y(unit $) + datetime and data class ad NA dealt with.RData

-EDA(not done yet);

-unit for y is now $;

-corrected typeof for all columns;

-Now almost ready for modeling, except for dummy variables tranformations.
### Created 20181019
workspace_10000 samples in test, all train with y(unit $) + datetime and data class ad NA dealt with+onehotecoding_2_versions.RData

-Deleted no-use column visits (all ones);

-Two versions of one-hot encoding (one with combined variables (onehottedtrain_com) , one without (onehottedtrain) );

-A first modeling try (random forest) on data, reached 15.9% R-square;

-I think now it's ready for more models!
### Created 20181020
[RF]workspace_10000 samples in test, all train with y(unit $) + datetime and data class ad NA dealt with+onehotecoding_2_versions+More Cleaning.RData

-Created ln(y) and decided to replace y;

-More eda with focus on correlation analysis;

-More data cleaning based on the eda, e.g. removed highly relevant variables;

-RF model, with rf_models and rf_best (now R-square is about 18%).
