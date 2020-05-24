## code to prepare `beer_data` dataset goes here
usethis::use_data(beer_data, overwrite = TRUE)
beer_full_df <- read.csv("/Users/spelkofer/Desktop/Desktop/GitHub/MCBeerRecommender/data-raw/beer_umap_full.csv")
beer_nn <- read.csv("/Users/spelkofer/Desktop/Desktop/GitHub/MCBeerRecommender/data-raw/beer_nn.csv")

usethis::use_data(beer_full_df)
usethis::use_data(beer_nn)