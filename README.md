# who-item-reduction-final-2021

**Contents**

* The R code for preparing the data for the item response analysis, merging
the data from each source, and running the item response analysis on the merged
data set is found in `code/item_response.R`
  + the code uses the following R packages: stringr, dplyr, readxl, and vaItemResponse
  + the raw data are assumed to be in a folder with a relative path: `../data/raw data/ SOURCE`
  + the merged data file is saved in a file with a relative path: `../data/clean/combined-data.csv`
  + the item response results are saved in a file with a relative path: `../results/item response/item_response_results.csv`

* The R package (vaItemResponse) for running the item response analysis is found in 
the `R-pkg` folder

