# Between administration and research: Understanding data management practices in a mid-sized technical university


This repository holds code and data for the preprint [Between administration and research: Understanding data management practices in a mid-sized technical university](https://osf.io/preprints/socarxiv/75ac6/)
on SocArXiv.


## Reproducible code
The analytic pipeline for creating the figures is reproducible through the 
`drake` package. Running `r_make()` will rebuild the analysis.

The pipeline is specified in the file `R/02-plan.R`. You can visualise the 
dependencies with the following code:

```r
library(drake)
source("R/02-plan.R")

vis_drake_graph(plan)
```


All packages that are used during the analysis are specified in `R/packages.R`.
For the analysis files to render you will need to install the font "Hind" (for
example from [Google Fonts](https://fonts.google.com/)) and 
register it with
[`extrafont`](https://cran.r-project.org/web/packages/extrafont/README.html). 
See the file `load_fonts.R` for specific instructions.

The figures of the final paper can be found in `figs/final`. This directory also
includes further figures that were not used in the paper. 

Initial explorations can be found in `general_exploration.html`, as well as 
in the directory `figs/descriptive`. 

### Code files

**Core files for reproducing the analysis**

|Filename                        |Purpose                                                                    |
|:-------------------------------|:--------------------------------------------------------------------------|
|_drake.R                        |Core file for building the analysis. Run with `drake::r_make("_drake.R")`.| 
|R/00-packages.R                 |Lists and loads all packages relevant to the analysis.                 |
|R/01-functions.R                |Functions that wrap preprocessing tasks as outlined in `plan.R`.|
|R/02-plan.R                     |Describes all inputs and outputs along with the functions processing them.|


## Data description
The directory `data/raw` contains the raw files as they came from LimeSurvey. 
There is one file with all respondents, and one with those that completed the
survey. It is not advised to use the file with all respondents, since some of
the respondents did not finish the survey by clicking "Submit". This drop-out
might be due to genuine loss of interest, or simply because someone wanted to
look at the survey without giving honest answers.

There was no substantial cleaning done on the survey responses, only tweaks 
to the column headers. The result of this, and therefore the basis for the
figures, can be found in `data/processed`. This directory also includes an 
overview of the variables in the survey, mapping short codes to answers.

### Overview of data files

**Raw data files**

|Filename                        |Purpose                                                               |
|:-------------------------------|:---------------------------------------------------------------------|
|data/raw/results-survey896447-total.csv|Raw data from the survey with all respondents.|
|data/raw/results-survey896447-total.csv |Raw data from the survey with respondents who finished the survey.|
|data/raw/label_basis.csv |Full headers, which are used to produce the overview of variables.|


**Processed data files**

|Filename                        |Purpose                                                                    |
|:-------------------------------|:--------------------------------------------------------------------------|
|data/processed/survey_processed.csv|Cleaned data from the survey. This file is the basis for the analysis presented in our paper.|
|data/processed/labels.csv|Plain language descriptions of the variables in `data/processed/survey_processed.csv`.|


## Further resources
You can find more information on the `drake` package here: 
https://books.ropensci.org/drake/



## Package versions used
The analysis was last rendered with the following package versions:

```
- Session info -----------------------------------------------------------------
 setting  value                       
 version  R version 4.0.3 (2020-10-10)
 os       Windows 10 x64              
 system   x86_64, mingw32             
 ui       RStudio                     
 language (EN)                        
 collate  German_Austria.1252         
 ctype    German_Austria.1252         
 tz       Europe/Berlin               
 date     2021-01-21                  

- Packages ---------------------------------------------------------------------
 package      * version date       lib source                             
 ash            1.0-15  2015-09-01 [1] CRAN (R 4.0.0)                     
 assertthat     0.2.1   2019-03-21 [1] CRAN (R 4.0.0)                     
 backports      1.2.0   2020-11-02 [1] CRAN (R 4.0.3)                     
 base64url      1.4     2018-05-14 [1] CRAN (R 4.0.0)                     
 broom          0.7.2   2020-10-20 [1] CRAN (R 4.0.2)                     
 ca             0.71.1  2020-01-24 [1] CRAN (R 4.0.2)                     
 callr          3.5.1   2020-10-13 [1] CRAN (R 4.0.3)                     
 cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.0.0)                     
 cli            2.1.0   2020-10-12 [1] CRAN (R 4.0.3)                     
 clisymbols     1.2.0   2017-05-21 [1] CRAN (R 4.0.2)                     
 colorspace     1.4-1   2019-03-18 [1] CRAN (R 4.0.3)                     
 crayon         1.3.4   2017-09-16 [1] CRAN (R 4.0.0)                     
 DBI            1.1.0   2019-12-15 [1] CRAN (R 4.0.0)                     
 dbplyr         2.0.0   2020-11-03 [1] CRAN (R 4.0.3)                     
 desc           1.2.0   2018-05-01 [1] CRAN (R 4.0.0)                     
 devtools       2.3.2   2020-09-18 [1] CRAN (R 4.0.2)                     
 digest         0.6.27  2020-10-24 [1] CRAN (R 4.0.3)                     
 dplyr        * 1.0.2   2020-08-18 [1] CRAN (R 4.0.2)                     
 drake        * 7.12.7  2020-10-27 [1] CRAN (R 4.0.3)                     
 ellipsis       0.3.1   2020-05-15 [1] CRAN (R 4.0.0)                     
 evaluate       0.14    2019-05-28 [1] CRAN (R 4.0.0)                     
 extrafont      0.17    2014-12-08 [1] CRAN (R 4.0.0)                     
 extrafontdb    1.0     2012-06-11 [1] CRAN (R 4.0.0)                     
 fansi          0.4.1   2020-01-08 [1] CRAN (R 4.0.0)                     
 filelock       1.0.2   2018-10-05 [1] CRAN (R 4.0.0)                     
 forcats      * 0.5.0   2020-03-01 [1] CRAN (R 4.0.0)                     
 fs             1.5.0   2020-07-31 [1] CRAN (R 4.0.2)                     
 gdtools        0.2.2   2020-04-03 [1] CRAN (R 4.0.0)                     
 generics       0.1.0   2020-10-31 [1] CRAN (R 4.0.3)                     
 ggalt        * 0.4.0   2017-02-15 [1] CRAN (R 4.0.2)                     
 ggchicklet   * 0.5.0   2020-06-25 [1] local                              
 ggplot2      * 3.3.2   2020-06-19 [1] CRAN (R 4.0.2)                     
 ggrepel      * 0.8.2   2020-03-08 [1] CRAN (R 4.0.2)                     
 glue           1.4.2   2020-08-27 [1] CRAN (R 4.0.2)                     
 gtable         0.3.0   2019-03-25 [1] CRAN (R 4.0.0)                     
 haven          2.3.1   2020-06-01 [1] CRAN (R 4.0.0)                     
 hms            0.5.3   2020-01-08 [1] CRAN (R 4.0.0)                     
 hrbrthemes   * 0.8.0   2020-03-06 [1] CRAN (R 4.0.0)                     
 htmltools      0.5.0   2020-06-16 [1] CRAN (R 4.0.0)                     
 httr           1.4.2   2020-07-20 [1] CRAN (R 4.0.2)                     
 igraph         1.2.6   2020-10-06 [1] CRAN (R 4.0.3)                     
 janeaustenr    0.1.5   2017-06-10 [1] CRAN (R 4.0.2)                     
 jsonlite       1.7.1   2020-09-07 [1] CRAN (R 4.0.2)                     
 KernSmooth     2.23-17 2020-04-26 [2] CRAN (R 4.0.3)                     
 knitr          1.30    2020-09-22 [1] CRAN (R 4.0.2)                     
 lattice        0.20-41 2020-04-02 [2] CRAN (R 4.0.3)                     
 lifecycle      0.2.0   2020-03-06 [1] CRAN (R 4.0.0)                     
 lubridate      1.7.9   2020-06-08 [1] CRAN (R 4.0.0)                     
 magrittr       1.5     2014-11-22 [1] CRAN (R 4.0.0)                     
 maps           3.3.0   2018-04-03 [1] CRAN (R 4.0.2)                     
 MASS           7.3-53  2020-09-09 [2] CRAN (R 4.0.3)                     
 Matrix         1.2-18  2019-11-27 [2] CRAN (R 4.0.3)                     
 memoise        1.1.0   2017-04-21 [1] CRAN (R 4.0.0)                     
 modelr         0.1.8   2020-05-19 [1] CRAN (R 4.0.0)                     
 munsell        0.5.0   2018-06-12 [1] CRAN (R 4.0.0)                     
 patchwork    * 1.1.0   2020-11-09 [1] CRAN (R 4.0.3)                     
 pillar         1.4.6   2020-07-10 [1] CRAN (R 4.0.2)                     
 pkgbuild       1.1.0   2020-07-13 [1] CRAN (R 4.0.2)                     
 pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.0.0)                     
 pkgload        1.1.0   2020-05-29 [1] CRAN (R 4.0.0)                     
 prettyunits    1.1.1   2020-01-24 [1] CRAN (R 4.0.0)                     
 processx       3.4.4   2020-09-03 [1] CRAN (R 4.0.2)                     
 progress       1.2.2   2019-05-16 [1] CRAN (R 4.0.0)                     
 proj4          1.0-10  2020-03-02 [1] CRAN (R 4.0.0)                     
 prompt         1.0.0   2020-07-01 [1] Github (gaborcsardi/prompt@b332c42)
 ps             1.4.0   2020-10-07 [1] CRAN (R 4.0.2)                     
 purrr        * 0.3.4   2020-04-17 [1] CRAN (R 4.0.0)                     
 R6             2.5.0   2020-10-28 [1] CRAN (R 4.0.3)                     
 RColorBrewer   1.1-2   2014-12-07 [1] CRAN (R 4.0.0)                     
 Rcpp           1.0.5   2020-07-06 [1] CRAN (R 4.0.2)                     
 readr        * 1.4.0   2020-10-05 [1] CRAN (R 4.0.2)                     
 readxl         1.3.1   2019-03-13 [1] CRAN (R 4.0.0)                     
 remotes        2.2.0   2020-07-21 [1] CRAN (R 4.0.2)                     
 reprex         0.3.0   2019-05-16 [1] CRAN (R 4.0.0)                     
 rlang          0.4.8   2020-10-08 [1] CRAN (R 4.0.3)                     
 rmarkdown      2.5     2020-10-21 [1] CRAN (R 4.0.3)                     
 rprojroot      1.3-2   2018-01-03 [1] CRAN (R 4.0.0)                     
 rstudioapi     0.12    2020-11-10 [1] CRAN (R 4.0.3)                     
 Rttf2pt1       1.3.8   2020-01-10 [1] CRAN (R 4.0.0)                     
 rvest          0.3.6   2020-07-25 [1] CRAN (R 4.0.2)                     
 scales       * 1.1.1   2020-05-11 [1] CRAN (R 4.0.0)                     
 sessioninfo    1.1.1   2018-11-05 [1] CRAN (R 4.0.0)                     
 SnowballC      0.7.0   2020-04-01 [1] CRAN (R 4.0.0)                     
 storr          1.2.4   2020-10-12 [1] CRAN (R 4.0.3)                     
 stringi        1.5.3   2020-09-09 [1] CRAN (R 4.0.2)                     
 stringr      * 1.4.0   2019-02-10 [1] CRAN (R 4.0.0)                     
 systemfonts    0.3.2   2020-09-29 [1] CRAN (R 4.0.2)                     
 testthat       3.0.0   2020-10-31 [1] CRAN (R 4.0.3)                     
 tibble       * 3.0.4   2020-10-12 [1] CRAN (R 4.0.3)                     
 tidyr        * 1.1.2   2020-08-27 [1] CRAN (R 4.0.2)                     
 tidyselect     1.1.0   2020-05-11 [1] CRAN (R 4.0.0)                     
 tidytext       0.2.6   2020-09-20 [1] CRAN (R 4.0.2)                     
 tidyverse    * 1.3.0   2019-11-21 [1] CRAN (R 4.0.0)                     
 tokenizers     0.2.1   2018-03-29 [1] CRAN (R 4.0.2)                     
 txtq           0.2.3   2020-06-23 [1] CRAN (R 4.0.2)                     
 usethis        1.6.3   2020-09-17 [1] CRAN (R 4.0.2)                     
 vctrs          0.3.4   2020-08-29 [1] CRAN (R 4.0.2)                     
 withr          2.3.0   2020-09-22 [1] CRAN (R 4.0.2)                     
 xfun           0.19    2020-10-30 [1] CRAN (R 4.0.3)                     
 xml2           1.3.2   2020-04-23 [1] CRAN (R 4.0.0)                         
```
