library(rmarkdown)
rmarkdown::render( "../transformr/vignettes/introduction.Rmd"
                 , output_file = "index.html"
                 , output_dir = "."
                 )
