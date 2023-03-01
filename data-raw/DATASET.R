## code to prepare `DATASET` dataset goes here
iw <- silicate::inlandwaters
er <- sf::write_sf(iw, tf <- tempfile(fileext = ".fgb"))
iw <- sf::read_sf(tf)
usethis::use_data(iw, overwrite = TRUE)
