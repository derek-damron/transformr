# transform
`transform` is an R package that contains a variety of (hopefully) short and intuitive functions that handle common data transformations.

So why should you consider using `transform` for your data analysis work?  The functions within `transform` are designed to address some of the issues that I have experienced with writing adhoc code for projects:

+ **Enhance the readability of your code** (both for yourself and for others!) by using intuitive function calls and minimizing multi-line data preparation steps.
+ **Boost your confidence that data transformations are occuring as expected** with reviewable unit testing.

## Table of contents
+ [Installing transform](#installing-transform)
+ [Using transform](#using-transform)
+ [Contributing to transform](#contributing-to-transform)

## Installing transform

You can install the most recent version of `transform` using the R code below.  Simply copy and paste the code into your R console!

```r
if (require(devtools)) {
    install.packages('devtools')
}
install_github('derek-damron/transform')
```

## Using transform

Currently, `transform` contains the following functions for transforming numeric and categorical variables:

+ Numeric variables
    + **`trim`**- trim numeric values at specified values or percentiles
    + **`rescale`**- rescale numeric values using common methods (e.g. to normality)
+ Categorical variables
    + **`corral`**- corral infrequent or uninteresting values and "level" the resulting variable

## Contributing to transform

`transform` is a work in constant progress, so if you have any ideas for expanding it's capabilities then please reach out!  

The best way to discuss new functionality for `transform` is to <a href="https://github.com/derek-damron/transform/issues/new" target="_blank">open up an issue here on Github</a>.  No potential improvement is too small to discuss, whether the idea is simply adding a new option to an existing function or creating a new function entirely.

If seeing an example would be helpful, <a href="https://github.com/derek-damron/transform/issues/1" target="_blank">take a look at this issue</a>.
