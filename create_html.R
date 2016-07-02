library(rmarkdown)
rmarkdown::render( "../transform/vignettes/introduction.Rmd"
                 , output_file = "index.html"
                 , output_dir = "."
                 )
