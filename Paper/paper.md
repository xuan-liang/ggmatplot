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
  - name: The Australian National University
    index: 1
  - name: Monash University
    index: 2
citation_author: Liang et. al.
date: "2022-03-03"
year: 2022
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---






# Summary

The layered grammar of graphics [@Wickham2010-kt], implemented as the `ggplot2` package [@Wickham2016] in the statistical language R [@rstats], is a powerful and popular tool to create versatile statistical graphics. This graphical system, however, requires input data to be organised in a manner that a data column is mapped to an aesthetic element (e.g. x-coordinate, y-coordinate, color, size), which create friction in constructing plots with an aesthetic element that span multiple columns in the original data by requiring users to re-organise the data.  

The `ggmatplot`, built upon `ggplot2`, is an R-package that allows quick plotting across the columns of matrices or data with the result returned as a `ggplot` object. The package is inspired by the function `matplot()` in the core R `graphics` system, thus `ggmatplot` can be considered as a `ggplot` version of `matplot` with the benefits of customising the plots as any other `ggplot` objects via `ggplot2` functions.


# Statement of need

Input data to construct plots with `ggplot2` require data to be organised in a manner that maps data columns to aesthetic elements. This required form is, however, not consistent with some prevelant data formats, specifically multivariate data where the rows correspond to observational units and continguous columns correspond to the responses of the same measurement units. Examples of common utilization of this form include gene expression data in bioinformatics, where rows correspond to samples, column names correspond to genes, and cell values correspond to the expression level; and multi-abundance data in ecology, where rows correspond to site, column names correspond to species name, and cell values correspond to count. Consequently, plotting with `ggplot2` interrupts the workflow of a user that is trying to quickly explore these types of data. The `ggmatplot` R-package provides a solution to this common friction in producing plots with `ggplot2`. 


# Examples


Data can be tidied in a rectangular form where each row represents an observational unit, each column represents a variable, and each cell represents a value [@Wickham2014-gy]. In some cases, what constitutes a variable (or observational unit), hence a column (or row), in a tidy data can be dependent upon interpretation or downstream interest (e.g. Tables \ref{tab:tab1} and \ref{tab:tab2} can be both considered as tidy data), but a clear violation of tidy data principles is when the column names contain data values, e.g. Table \ref{tab:tab3} contain the name of the species across a number of column names.


\begin{table}

\caption{\label{tab:tab1}Restaurant rating data in "tidy" form}
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

\caption{\label{tab:tab2}Restaurant rating data in "molten" form}
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

\caption{\label{tab:tab3}Spider abundance data with environmental covariates.}
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


```r
library(ggmatplot)
ggmatplot(x = select(wide_df, contains("rating")),
          plot_type = "both",
          xlab = "Restaurant")
```

![](paper_files/figure-latex/plot1-1.pdf)<!-- --> 


# Acknowledgements



# References
