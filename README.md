
# ggmatplot

`ggmatplot` is a quick and easy way of plotting the columns of two
matrices or data frames against each other using
[`ggplot2`](https://ggplot2.tidyverse.org/).

## Overview

[`ggplot2`](https://ggplot2.tidyverse.org/) requires wide format data to
be wrangled into long format for plotting, which can be quite cumbersome
when creating simple plots. Therefore, the motivation for `ggmatplot`
was to provide a solution that allows
[`ggplot2`](https://ggplot2.tidyverse.org/) to handle wide format data.
Although `ggmatplot` doesn’t provide the same flexibility as
[`ggplot2`](https://ggplot2.tidyverse.org/), it can be used as a
workaround for having to wrangle wide format data into long format and
creating simple plots using [`ggplot2`](https://ggplot2.tidyverse.org/).

`ggmatplot` is built upon [`ggplot2`](https://ggplot2.tidyverse.org/),
and its functionality is inspired by
[`matplot`](https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/matplot).
Therefore, `ggmatplot` can be considered as a `ggplot` version of
[`matplot`](https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/matplot).

Similar to
[`matplot`](https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/matplot),
`ggmatplot` plots a vector against the columns of a matrix, or the
columns of two matrices against each other, or a vector/matrix on its
own. However, unlike
[`matplot`](https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/matplot),
`ggmatplot` returns a `ggplot` object. Therefore, [ggplot add
ons](https://ggplot2.tidyverse.org/reference/index.html) such as scales,
faceting specifications, coordinate systems and themes can also be added
on to the resultant `ggplot` object.

## Installation

The latest version can be installed from
[GitHub](https://github.com/xuan-liang/ggmatplot):

``` r
# install.packages("remotes")
remotes::install_github("xuan-liang/ggmatplot")
```

## Examples

``` r
library(ggmatplot)
```

This example plots a vector x against each column of matrix z using the
default `plot_type` of `ggmatplot()` - ‘point’

``` r
# vector x
x <- c(rnorm(100, sd = 2))

# matrix z
y <- x * 0.5 + rnorm(100, sd = 1)
fit.y <- fitted(lm(y ~ x))
z <- cbind(actual = y,
           fitted = fit.y)

ggmatplot(x, z)
```

<img src="man/figures/README-point-1.png" width="80%" />

If two matrices with equal number of columns are used, the corresponding
columns of the matrices will be plotted against each other. i.e. column
1 of matrix x will be plotted against column 1 of matrix y, column 2 of
matrix x will be plotted against column 2 of matrix y, etc.

This example uses the iris dataset, with matrices x and y as shown
below. The Sepal.Length is plotted against Sepal.Width and the
Petal.Length is plotted against Petal.Width. Therefore the groups
‘Column 1’ and ‘Column 2’ can be interpreted as ‘Sepal’ and ‘Petal’
respectively.

``` r
x <- (iris[, c(1,3)])
head(x,5)
#>   Sepal.Length Petal.Length
#> 1          5.1          1.4
#> 2          4.9          1.4
#> 3          4.7          1.3
#> 4          4.6          1.5
#> 5          5.0          1.4

y <- (iris[, c(2,4)])
head(y,5)
#>   Sepal.Width Petal.Width
#> 1         3.5         0.2
#> 2         3.0         0.2
#> 3         3.2         0.2
#> 4         3.1         0.2
#> 5         3.6         0.2

ggmatplot(x,y)
```

<img src="man/figures/README-point-columns-1.png" width="80%" />

This example creates a line plot of vector x against the columns of
matrix y. The plot is therefore grouped by the columns of vector y.
Custom colors are assigned to each group using the `color` parameter,
and the limits of the y axis are updated using the `ylim` parameter.
Further, a ggplot theme is added on to the resultant ggplot object.

``` r
# matrix x
x <- 1:10

# matrix y
y <- cbind(
  square = x^2,
  cube = x^3
)

ggmatplot(x, y,
  plot_type = "line",
  color = c("blue", "purple"),
  ylim = c(0, 750)
) +
  theme_minimal()
```

<img src="man/figures/README-line-1.png" width="80%" />

This is a plot of the US personal expenditure over 5 categories and 5
years, and is a simple example of how wide format data can used with
`ggmatplot()`. Note how the expenditure categories to be used on the x
axis is used as vector x, and the expenditure values is used in wide
format as matrix y - with its columns corresponding to the grouping
structure.

The plot uses the `plot_type` ‘both’, which is a combination of ‘point’
and ‘line’ plots. Its customized using `ggmatplot()` parameters and a
`ggplot` theme as well.

``` r
USPersonalExpenditure
#>                       1940   1945  1950 1955  1960
#> Food and Tobacco    22.200 44.500 59.60 73.2 86.80
#> Household Operation 10.500 15.500 29.00 36.5 46.20
#> Medical and Health   3.530  5.760  9.71 14.0 21.10
#> Personal Care        1.040  1.980  2.45  3.4  5.40
#> Private Education    0.341  0.974  1.80  2.6  3.64

# vector x
x <- rownames(USPersonalExpenditure)

ggmatplot(x, USPersonalExpenditure,
  plot_type = "both",
  xlab = "Category",
  ylab = "Expenditure (in Billions of Dollars)",
  legend_title = "Year",
  legend_label = c(1940, 1945, 1950, 1955, 1960)
) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<img src="man/figures/README-both-1.png" width="80%" />

Density plots only accept a single matrix or data frame and will group
the plot based on its columns. The following density plot uses a two
column matrix, and groups the plot by the two columns. While the default
density estimate is represented in the measurement units of the data, an
aesthetic mapping is added on to the ggplot object to scale the density
estimate to a maximum of 1.

``` r
# matrix X
x <- (iris[, 1:2])

ggmatplot(x, plot_type = "density") +
  aes(y = ..scaled..) +
  theme_bw()
```

<img src="man/figures/README-density-1.png" width="80%" />

Boxplots too accept only a single matrix or data frame, and uses its
columns as individual groups. While `ggmatplot` plots are filled by
default, the fill color can be made transparent by using `alpha = 0`.

It is also worth noticing that `alpha` isn’t a parameter defined in
`ggmatplot()`, but can be used. This is because `ggmatplot` is built
upon `ggplot2`, and each `plot_type` corresponds to a
[`geom`](https://ggplot2.tidyverse.org/reference/index.html#section-geoms).
Therefore, all valid parameters with the underlying
[`ggplot2 geom`](https://ggplot2.tidyverse.org/reference/index.html#section-geoms)
can be used with `ggmatplot()`

``` r
# matrix X
x <- (iris[, 1:4])

ggmatplot(x,
  plot_type = "boxplot",
  alpha = 0, # removing fill values
  xlab = "", ylab = ""
)
```

<img src="man/figures/README-boxplot-1.png" width="80%" />

Violin plots too accepts a single matrix or data frame input, and
behaves similar to density plots and boxplots.

This plot updates the colors of the two groups using the `color`
parameter, and it can be seen that the fill of the violin plots has been
updated too. This is because updating either the `color` or `fill`
parameter will automatically update the other, unless they are both
defined simultaneously.

``` r
# matrix X
x <- (iris[, 1:2])

ggmatplot(x,
  plot_type = "violin",
  color = c("#00AFBB", "#E7B800"),
  xlab = "", ylab = ""
)
```

<img src="man/figures/README-violin-1.png" width="80%" />

Similar to density, violin and box plots, histograms too accept a single
matrix or data frame input and groups the plot using its columns. The
histogram in the following example uses a matrix of 4 columns, and
therefore groups the plots based on these 4 columns. The plot is also
faceted by group.

The `color` and `fill` parameters have been defined simultaneously on
this plot. However, only a single `color` value is defined whereas the
number of `fill` colors correspond to the number of groups. If a single
value is defined it will be used over all groups, like the black line
color is used across all groups in this example.

``` r
# matrix X
x <- (iris[, 1:4])

ggmatplot(x,
  plot_type = "histogram",
  xlab = "Group",
  color = "black",
  fill = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
) +
  facet_wrap(~Group, scales = "free")
```

<img src="man/figures/README-histogram-1.png" width="80%" />
