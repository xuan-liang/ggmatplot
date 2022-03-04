---
title: '`ggmatplot`: An R package for data visualization on wide-format data'
tags:
- R
- grammar of graphics
- ggplot2
- data visualisation
authors:
  - name: Xuan Liang
    orcid: 0000-0002-0792-3751
    affiliation: 1
  - name: Francis K. C. Hui
    orcid: 0000-0003-0765-3533
    affiliation: 1
  - name: Dilinie Seimon
    orcid: 0000-0003-4431-6077
    affiliation: 2
  - name: Emi Tanaka
    orcid: 0000-0002-1455-259X
    affiliation: 2
affiliations:
  - name: Research School of Finance, Actuarial Studies and Statistics, The Australian National University
    index: 1
  - name: Department of Econometrics and Business Statistics, Monash University
    index: 2
citation_author: Liang et. al.
date: "2022-03-04"
year: 2022
bibliography: paper.bib
output: 
  rticles::joss_article:
    keep_tex: true
csl: apa.csl
journal: JOSS
---






# Summary

The layered grammar of graphics [@Wickham2010-kt], implemented as the `ggplot2` package [@Wickham2016] in the statistical language R [@rstats], is a powerful and popular tool to create versatile statistical graphics. This graphical system, however, requires input data to be organised in a manner that a data column is mapped to an aesthetic element (e.g. x-coordinate, y-coordinate, color, size), which create friction in constructing plots with an aesthetic element that span multiple columns in the original data by requiring users to re-organise the data.  

The `ggmatplot`, built upon `ggplot2`, is an R-package that allows quick plotting across the columns of matrices or data with the result returned as a `ggplot` object. The package is inspired by the function `matplot()` in the core R `graphics` system, thus `ggmatplot` can be considered as a `ggplot` version of `matplot` with the benefits of customising the plots as any other `ggplot` objects via `ggplot2` functions.


# Statement of need

Input data to construct plots with `ggplot2` require data to be organised in a manner that maps data columns to aesthetic elements. This generally works well where data is tidied in a rectangular form, referred to as "tidy data" [@Wickham2014-gy], where each row represents an observational unit, each column represents a variable, and each cell represents a value. In some cases, what constitutes a variable (or observational unit), hence a column (or row), in a tidy data can be dependent upon interpretation or downstream interest (e.g. Tables \ref{tab:tab1} and \ref{tab:tab2} can be both considered as tidy data), but a clear violation of tidy data principles is when the column names contain data values, e.g. Table \ref{tab:tab3} contain the name of the species across a number of column names.

\begin{table}

\caption{\label{tab:tab1}Restaurant rating data in "tidy" form. The first column shows the restaurant ID, and the next four columns show the average ratings (out of 5) for food, service, ambience and overall, respectively.}
\centering
\begin{tabular}[t]{lrrrr}
\toprule
\multicolumn{1}{c}{ } & \multicolumn{4}{c}{Rating} \\
\cmidrule(l{3pt}r{3pt}){2-5}
Restaurant & Food & Service & Ambience & Overall\\
\midrule
R1 & 4 & 3 & 4 & 4\\
R2 & 4 & 5 & 4 & 4\\
R3 & 3 & 4 & 5 & 3\\
R4 & 2 & 4 & 4 & 3\\
R5 & 3 & 4 & 4 & 3\\
\bottomrule
\end{tabular}
\end{table}



\begin{table}

\caption{\label{tab:tab2}Another form for the restaurant rating data in Table \ref{tab:tab1}. In @Wickham2014-gy, this format is called the "molten" data.}
\centering
\begin{tabular}[t]{llr}
\toprule
Restauant & Rating type & Rating\\
\midrule
R1 & food & 4\\
R1 & service & 3\\
R1 & ambience & 4\\
R1 & overall & 4\\
R2 & food & 4\\
R2 & service & 5\\
R2 & ambience & 4\\
R2 & overall & 4\\
R3 & food & 3\\
R3 & service & 4\\
R3 & ambience & 5\\
R3 & overall & 3\\
R4 & food & 2\\
R4 & service & 4\\
R4 & ambience & 4\\
R4 & overall & 3\\
R5 & food & 3\\
R5 & service & 4\\
R5 & ambience & 4\\
R5 & overall & 3\\
\bottomrule
\end{tabular}
\end{table}


\begin{table}

\caption{\label{tab:tab3}Spider abundance data with environmental covariates. The rows correspond to the site, the first two columns are environmental covariates that measure the soil dry mass and cover moss, and the following five columns shows the abundance of the species.}
\centering
\begin{tabular}[t]{rrrrrrrr}
\toprule
\multicolumn{1}{c}{ } & \multicolumn{2}{c}{Environment covariates} & \multicolumn{5}{c}{Species abundance} \\
\cmidrule(l{3pt}r{3pt}){2-3} \cmidrule(l{3pt}r{3pt}){4-8}
Site & Soil dry mass & Moss & Alopcune & Arctlute & Pardpull & Trocterr & Zoraspin\\
\midrule
1 & 2.3321 & 3.0445 & 10 & 0 & 45 & 57 & 4\\
2 & 3.0493 & 1.0986 & 2 & 0 & 37 & 65 & 9\\
3 & 2.5572 & 2.3979 & 20 & 0 & 45 & 66 & 1\\
\bottomrule
\end{tabular}
\end{table}

The organisation of the data is largely dependent on the downstream analysis and there is no one correct way to do this. Some forms of multivariate data, e.g. Table \ref{tab:tab3}, are prevalent in the field because it aligns as an input data for a modelling software and/or the format is more convenient for input or view of the data in spreadsheet format. However, this format is not consistent with the required format for `ggplot2`, and consequently, plotting with `ggplot2` interrupts the workflow of a user that is trying to quickly visualise these types of data. The `ggmatplot` R-package provides a solution to this common friction in producing plots with `ggplot2`. 



# Examples


In this section we demonstrate the use of the `ggmatplot` package and contrast the specification with `ggplot2` with data wrangling using `dplyr` and `tidyr` [@Wickham2019] using the example data in Tables \ref{tab:tab1} and \ref{tab:tab3}, which are stored in the objects `wide_df` and `abun_df`, respectively.

## Example 1


```r
library(ggmatplot)
ggmatplot(x = wide_df[, -1], plot_type = "both",
          xlab = "Restaurant",  ylab = "Rating", legend_title = "Type")
```

![](paper_files/figure-latex/matplot1-1.pdf)<!-- --> 


```r
library(ggplot2)
library(tidyr) # or library(tidyverse)
wide_df %>% 
  pivot_longer(contains("rating"), 
               names_to = "rating_type",
               values_to = "rating") %>% 
  ggplot(aes(restaurant, rating, color = rating_type)) + 
  geom_point() +
  geom_line(aes(group = rating_type,
                linetype = rating_type))
```

## Example 2


```r
ggmatplot(x = wide_df[, 2:4], y = wide_df[, 5], plot_type = "both")
```

![](paper_files/figure-latex/matplot2-1.pdf)<!-- --> 

# Discussion

The `ggmatplot` R-package provides a solution to a common friction to quickly plotting multivariate data where the primary interest is mapping the column names as an aesthetic element. The solution provided however is a recipe-driven approach where the user can only produce plot types as many there are included in the `plot_type` option. Future development of the package could benefit from using a grammar approach, like in @Wilkinson2005-oz and @Wickham2010-kt, where plot types can be extensible. 

# Acknowledgements 


FKCH was supported by ARC DECRA XXX. 

# References
