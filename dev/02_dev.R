# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "header") # Name of the module
golem::add_module(name = "main") # Name of the module
golem::add_module(name = "top") # Name of the module

golem::add_module(name = "mid") # Name of the module
golem::add_module(name = "mid_lexicons") # Name of the module
golem::add_module(name = "mid_stop_words") # Name of the module
golem::add_module(name = "mid_negation_words") # Name of the module

golem::add_module(name = "bot") # Name of the module
golem::add_module(name = "creds_modal") # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("twitter")
golem::add_fct("analysis")
golem::add_fct("R6")
golem::add_utils("trigger")
golem::add_utils("coords")
golem::add_utils("tidytext")
golem::add_utils("tryCatch_alert")

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("fct_twitter")
usethis::use_test("lookup_coords_nominatim")
usethis::use_test("mod_mid")
usethis::use_test("fct_tidytext_lexicons")
usethis::use_test("fct_tidytext_analysis")
usethis::use_test("R6")

# Documentation

## Vignette ----
usethis::use_vignette("design-spec", "Design Specification")
usethis::use_vignette("auth", "Authentication with senTWEETment")
usethis::use_vignette("search-tweets", "How to search Tweets")
usethis::use_vignette("analysis", "Conducting sentiment analysis")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
