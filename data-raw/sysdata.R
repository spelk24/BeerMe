## code to prepare `beer_data` dataset goes here
beer_full_df <- read.csv("data-raw/beer_umap_full.csv")
beer_nn <- read.csv("data-raw/beer_nn.csv")

usethis::use_data(beer_full_df, beer_nn, internal = TRUE, overwrite = TRUE)
rm(beer_full_df, beer_nn)
