local_image_path <- system.file("r_logo.png", package = "ggpath")

img <- gdal_get(local_image_path)


vp <- grid::viewport(
        x = grid::unit(0.5, "npc"),
        y = grid::unit(0.5,"npc"),
        width = grid::unit(0.2, "npc"),
        height = grid::unit(0.3, "npc"),
        angle = 0
      )
grid.draw(rasterGrob(img, vp = vp))




# compute path of an R logo file shipped with ggpath
local_image_path <- system.file("r_logo.png", package = "ggpath")

# create dataframe with x-y-coordinates and the above local path
plot_data <- data.frame(x = 0, y = c(-1, 1), gdal = local_image_path, px = 26, py = 18, resample = c("near", "cubic"))

# plot images directly from local path
ggplot(plot_data, aes(x = x, y = y, px = px, py = py, resample = resample)) +
  geom_from_gdal(aes(gdal = gdal), width = 0.3, height = .3) +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-2, 2)) +
  theme_minimal()
