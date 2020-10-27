################################################################################
# Prepare the website
################################################################################

# Create a new post
distill::create_post("2020-10-20-001-data-reshape")


rmarkdown::render('data-reshape.Rmd', 'distill::distill_article') 
