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
date: "2022-05-17"
year: 2022
bibliography: paper.bib
output: 
  rticles::joss_article:
    keep_tex: true
csl: apa.csl
journal: JOSS
nocite: '@mosaicData'
---






# Summary

The layered grammar of graphics [@Wickham2010-kt], implemented as the `ggplot2` package [@Wickham2016] in the statistical language R [@rstats], is a powerful and popular tool to create versatile statistical graphics. However, this graphical system requires input data to be organised in a manner that a data column is mapped to an aesthetic element (e.g., x-coordinate, y-coordinate, color, size), which creates friction in constructing plots with an aesthetic element that spans multiple columns in the original data, as it requires the user to reorganise the data.  
<!-- FKCH: Should we use the words long and wide format data frames here, or is that wrong? I only ask because I (maybe others?) think about ggplot requiring long format data, but we want to wide formatting? 

XL: Regarding Francis' question, I don't know if the long format or wide format data are well defined in the literature. If it is, we definitely can mention it. If not, do we need to define it? --commented by XL 

FKCH: @Emi you know the literature best. Can you make a decision on this? Thanks!

ET: I don't think we should mention long/wide -- there's more nuance than that. 
-->

The `ggmatplot`, built upon `ggplot2`, is an R-package that allows quick plotting across the columns of matrices or data with the result returned as a `ggplot` object. The package is inspired by the function `matplot()` in the core R `graphics` system -- as such, `ggmatplot` may be considered as a `ggplot` version of `matplot`, with the benefits of customising the plots as any other `ggplot` objects via `ggplot2` functions, as well as offering several other plotting types that are not immediately available from `matplot` directly e.g., comparative violin plots. The `ggmatplot` package is available on the Comprehensive R Archive Network (CRAN) with the latest developmental source code available at \url{https://github.com/xuan-liang/ggmatplot}. 


# Statement of need

Input data to construct plots with `ggplot2` require data to be organised in a manner that maps data columns to aesthetic elements. This generally works well where data is tidied in a long rectangular form, often referred to as "tidy data" [@Wickham2014-gy], where each row represents an observational unit, each column represents a variable, and each cell represents a value. However, in some cases what constitutes a variable (or observational unit), and hence a column (or row), in tidy data can be dependent upon interpretation or downstream interest (e.g. Tables \ref{tab:tab1} and \ref{tab:tab2} can both be considered as tidy data), but a clear violation of tidy data principles is when the column names contain data values, e.g. Table \ref{tab:tab3} contains months of the year across a number of column names.

\begin{table}

\caption{\label{tab:tab1}Restaurant rating data in "tidy" form. The first column shows the restaurant ID, and the next four columns show the average ratings (out of 5) for food, service, ambience and overall, respectively.}
\centering
\begin{tabular}[t]{lrrrr}
\toprule
\multicolumn{1}{c}{ } & \multicolumn{4}{c}{Average rating} \\
\cmidrule(l{3pt}r{3pt}){2-5}
Restaurant & Food & Service & Ambience & Overall\\
\midrule
R1 & 4.3 & 3.4 & 4.3 & 4.9\\
R2 & 4.3 & 5.0 & 4.5 & 4.4\\
R3 & 3.2 & 4.4 & 5.0 & 3.0\\
R4 & 2.3 & 4.6 & 4.4 & 3.8\\
R5 & 3.9 & 4.8 & 4.2 & 3.3\\
\bottomrule
\end{tabular}
\end{table}


\begin{table}

\caption{\label{tab:tab2}Another form for the restaurant rating data in Table \ref{tab:tab1}. In Wickham (2014), this format is called the "molten" data.}
\centering
\begin{tabular}[t]{llr}
\toprule
Restauant & Rating type & Average rating\\
\midrule
R1 & food & 4.3\\
R1 & service & 3.4\\
R1 & ambience & 4.3\\
R1 & overall & 4.9\\
R2 & food & 4.3\\
R2 & service & 5.0\\
R2 & ambience & 4.5\\
R2 & overall & 4.4\\
R3 & food & 3.2\\
R3 & service & 4.4\\
R3 & ambience & 5.0\\
R3 & overall & 3.0\\
R4 & food & 2.3\\
R4 & service & 4.6\\
R4 & ambience & 4.4\\
R4 & overall & 3.8\\
R5 & food & 3.9\\
R5 & service & 4.8\\
R5 & ambience & 4.2\\
R5 & overall & 3.3\\
\bottomrule
\end{tabular}
\end{table}

\begin{table}

\caption{\label{tab:tab3}The first 6 rows and 11 columns of the snowfall data for Grand Rapids, Michigan in the R pacakge  \texttt{mosaicData} (Prium, Kaplan \& Horton, 2021).}
\centering
\begin{tabular}[t]{rrrrrrrrrrr}
\toprule
SeasonStart & SeasonEnd & Jul & Aug & Sep & Oct & Nov & Dec & Jan & Feb & Mar\\
\midrule
1893 & 1894 & 0 & 0 & 0 & 0.0 & 8.0 & 24.9 & 12.5 & 6.8 & 4.8\\
1894 & 1895 & 0 & 0 & 0 & 0.0 & 7.5 & 5.3 & 21.5 & 8.0 & 22.5\\
1895 & 1896 & 0 & 0 & 0 & 0.4 & 23.2 & 15.0 & . & 8.5 & 2.0\\
1896 & 1897 & 0 & 0 & 0 & 0.2 & 8.0 & 8.0 & 4.9 & 11.2 & 12.0\\
1897 & 1898 & 0 & 0 & 0 & 0.0 & 1.4 & 8.0 & 15.5 & 29.5 & 0.0\\
1898 & 1899 & 0 & 0 & 0 & 0.0 & 18.5 & 18.0 & 20 & 3.4 & 16.0\\
\bottomrule
\end{tabular}
\end{table}

The organisation of the data is largely dependent on the subsequent analysis, and there is no one correct way to do this. Some forms of multivariate data, e.g. Table \ref{tab:tab3}, are prevalent in many scientific fields because it aligns with the input data for a particular modelling software, and/or the format is more convenient for input or view of the data in spreadsheet format (say). Unfortunately, this format is not consistent with the required format for `ggplot2`, and consequently plotting with `ggplot2` interrupts the workflow of a user that is trying to quickly visualise these types of data (as part of their exploratory data analysis, for example). The `ggmatplot` R-package seeks to provide a solution to this common friction. 
<!-- in producing plots with `ggplot2`.  -->



# Examples


In this section, we demonstrate the use of the `ggmatplot` package and contrast the specification with `ggplot2` after data wrangling using `dplyr` and `tidyr` [@Wickham2019]. We will use the example data in Tables \ref{tab:tab1} and \ref{tab:tab3}, which are stored in the objects `wide_df` and `SnowGR`, respectively.


## Example 1

The code below constructs a line plot (superimposed with a point) of the various types (food, service and ambience) of ratings, contained in columns 2 to 4 of `wide_df`, against the overall rating in column 5 of `wide_df` as shown in Figure 1.

<!-- Something is wrong in ggmatplot CRAN version with order of legend!!! The legend is displaying things in order of appearance, but the lines are plotting in alphabetical order. See ggplot below for correct order 

ET: fixed in the latest github version.

-->


```r
library(ggmatplot)
ggmatplot(x = wide_df[, 5], y = wide_df[, 2:4], plot_type = "both",
          xlab = "Overall rating",  ylab = "Rating", legend_title = "Type") 
```

![Line plot of the food, service, and ambience ratings versus overall rating, for five restaurants.](paper_files/figure-latex/matplot2-1.pdf) 

In contrast to the above, using `ggplot2` alone to obtain the same result as Figure 1 requires the data be wrangled to a long form first before plotting. This is exemplified in the code below, from which we identify a small, but noticeable, friction to the workflow for the practitioner that is looking to promptly explore their data. 


```r
library(ggplot2)
library(tidyr) # or library(tidyverse)
wide_df %>% 
  select(contains("rating")) %>% 
  pivot_longer(-overall_rating, 
               names_to = "rating_type",
               values_to = "rating") %>% 
  mutate(rating_type = fct_inorder(rating_type)) %>%
  ggplot(aes(x = overall_rating, y = rating, color = rating_type)) + 
  geom_point(aes(shape = rating_type)) +
  geom_line(aes(group = rating_type, linetype = rating_type)) +
  labs(x = "Overall rating", y = "Rating", 
       color = "Type", linetype = "Type", shape = "Type")
```


## Example 2

The example code draws the boxplot of each column of amount of snowfall across months in the `SnowGR` data, as presented in Figure 2. As the resulting object is a `ggplot` object, the user can leverage the `ggplot` functions to modify the output (e.g., addition of a title). 


```r
library(ggmatplot)
ggmatplot(x = SnowGR[, 3:14], plot_type = "boxplot",
          xlab = "Month",  ylab = "Snowfall") + 
          ggtitle("Grand Rapids, Michigan, 1893-2011")
```

![The distribution of the amount of snowfall at Grand Rapids, Michigan, across months from 1893-2011.](paper_files/figure-latex/matplot3-1.pdf) 

The equivalent code to produce Figure 2 without using `ggmatplot` is given below. Again, we observe a slight but non-negligible friction in putting the data in the right format prior to plotting. The original wide data format as seen in Table \ref{tab:tab3} is common in the environmental sciences, among other disciplines, and thus an analyst who has to repeat these tasks can benefit from a quick approach as `ggmatplot` offers.


```r
library(ggplot2)
library(tidyr) 
library(forcats) # or library(tidyverse)
SnowGR %>% 
  pivot_longer(Jul:Jun, 
               names_to = "Month",
               values_to = "Snowfall") %>% 
  mutate(Month = fct_inorder(Month)) %>% 
  ggplot(aes(Month, Snowfall)) + 
  geom_boxplot(alpha = 0.5) + 
  ggtitle("Grand Rapids, Michigan, 1893-2011")
```


<!-- FKCH: WOULD IT BENEFIT FROM ALSO SHOWING A PLOT THAT BASE MATPLOT CAN NOT? THE ABOVE CAN BE DONE BY APPLY `boxplot`, AND HENCE WHY I WROTE VIOLIN PLOT AT THE BEGINNING AS IT IS NOT AVAILABLE AT LEAST IN CORE R? 

XL: The other option is the density plot which can not be done by matplot--commented by XL. But since there are too many months, it might not be a good idea. -

ET: violin plot doesn't look good here so I'd rather stick with boxplot.  I don't think we need to emphasise how it can provide more plots than matplot because it already offers an advantage of being a ggplot object, and thus leverage existing ggplot customisation
-->

# Discussion

The `ggmatplot` R-package provides a solution to a common friction encountered when wanting to quickly plot multivariate data, where the primary interest is mapping the column names as an aesthetic element. While an important start, we also acknowledge that solution provided is a recipe-driven approach, where the user can only produce plot types as many there are included in the `plot_type` option. Future developments of the package could benefit from using a grammar approach, like in @Wilkinson2005-oz and @Wickham2010-kt, where plot types can be extensible. The latest developmental source code can be found at \url{https://github.com/xuan-liang/ggmatplot}. Further examples can be found at \url{https://xuan-liang.github.io/ggmatplot/}. 

# Acknowledgements 


FKCH was supported by an Australian Research Council Discovery Fellowship DE200100435.

# References
