---
title: 'ggmatplot: An R package for data visualization on wide-format data'
tags:
- R
- data visualisation
date: "22 November 2021"
output: pdf_document
authors:
- name: Xuan Liang
  orcid: 0000-0002-0792-3751
  affiliation: 1
- name: Francis Hui
  orcid: 0000-0003-0765-3533
  affiliation: 1
- name: Emi Tanaka
  orcid: 0000-0002-1455-259X
  affiliation: 2
- name: Dilinie Seimon
  orcid: 0000-0003-4431-6077
  affiliation: 2
bibliography: paper.bib
affiliations:
- name: Australian National University
  index: 1
- name: Monash University
  index: 2
---

# Summary

ggmatplot is a quick and easy way of plotting the columns of two matrices or data frames against each other using ggplot2. ggmatplot is built upon ggplot2, and its functionality is inspired by matplot. Therefore, ggmatplot can be considered as a ggplot version of matplot.

# Statement of need

The increased use of a tidy approach in structuring datasets results in wrangling datasets into wide format: with variables as columns and observations as rows(https://vita.had.co.nz/papers/tidy-data.html). While this introduces a more meaningful mapping of the data into its structure, the grammar of graphics requires wide format data to be wrangled back into long format for visualizing. This can be quite cumbersome when creating simple plots, and converting a tidy data frame back to a long format with multiple variables stacked upon each other can also be considered counter-intuitive.

Therefore, the motivation for ggmatplot is to provide a solution that allows ggplot2 to handle wide format data. Although ggmatplot doesn't provide the same flexibility as ggplot2, it can be used as a workaround for having to wrangle wide format data into long format and creating simple plots using ggplot2.

# Examples



# Acknowledgements



# References
