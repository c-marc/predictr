#library(usethis)

library(tidyverse)
library(fs)


#usethis::create_package("my.analysis.package")

dir_info(all = TRUE, regexp = "^[.]git$") %>% 
  select(path, type)

usethis::use_build_ignore("dev_history.R")

## Create Common Files ----
## See ?usethis for more information
#usethis::use_mit_license( name = "Marc C" )  # You can set another license here
#usethis::use_proprietary_license()
#or edit LICENCE & DESCRIPTION
usethis::use_readme_rmd( open = FALSE )
devtools::build_readme()
#usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
#usethis::use_news_md( open = FALSE )

## Use git ----
usethis::use_git()

## (You'll need GitHub there)
usethis::use_github()


# Document functions and dependencies
#attachment::att_to_description()

# Check the package
devtools::check()

usethis::use_vignette("draft")

#devtools::install()

usethis::use_build_ignore("app.R")

usethis::use_r("estimate")
usethis::use_r("roc")
usethis::use_r("pred")
usethis::use_r("full_App")




# go to
rstudioapi::navigateToFile( "R/estimate.R" )



## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "ggplot2" )
usethis::use_package( "dplyr" )
usethis::use_package( "tidyr" )
usethis::use_pipe()

#for deployement
usethis::use_package("shiny")
usethis::use_package("pkgload")


## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 


## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )
