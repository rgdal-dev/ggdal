
#' @export
#' @importFrom vapour vapour_raster_info vapour_warp_raster vapour_vrt
#' @importFrom palr d_pal
#' @importFrom grDevices dev.size hcl.colors
gdal_get <- function(x, dm = NULL, ..., extent = NULL, projection = NULL) {
  info <- vapour::vapour_raster_info(x)
  if (diff(info$extent)[3] < 0) {
    x <- vapour::vapour_vrt(x, extent = info$extent[c(1, 2, 4, 3)])
  }
  info <- vapour::vapour_raster_info(x)
  if (is.null(dm)) dm <- dev.size("px")
  data <- vapour::vapour_warp_raster(x, extent = info$extent, dimension = dm, bands = 1, ...)[[1L]]
  data <- palr::image_pal(as.numeric(data), col = hcl.colors(56))
  structure(list(data = matrix(data, dm[2L], byrow = TRUE),
       info = info), class = c("gdal-data", "gdal", "list"))
}
#' @export
print.gdal <- function(x, ...) {
  print(x$info$dimension)
  print(x$info$extent)
  print(sprintf("hasdata: %s", as.character(!is.null(x$data))))
}

#' @export
#' @importFrom grDevices as.raster
as.raster.gdal <- function(x, ...) {
  out <- x$data
  if (!is.character(out)) {
    out <- array(palr::image_pal(x$data, ...), dim(x$data)[1L])
  }
  #ximage(out)
  as.raster(out)
}


# INTERNAL HELPER THAT BUILDS THE GROBS FOR GeomFromGDAL - ripped off from ggpath
#' @importFrom grid rasterGrob viewport unit
#' @importFrom rlang caller_env
build_gdal_grobs <- function(i, alpha, colour, gdal, px, py, resample, data,
                        is_theme_element = FALSE,
                        call = rlang::caller_env()) {

  img <- gdal_get(gdal[i], dm = c(px[i], py[i]), resample = resample[i])

  # theme elements require justification outside the viewport
  # so we have to do this twice here
  if(isFALSE(is_theme_element)){
    grid::rasterGrob(
      img,
      vp = grid::viewport(
        x = grid::unit(data$x[i], "native"),
        y = grid::unit(data$y[i], "native"),
        width = grid::unit(data$width[i], "npc"),
        height = grid::unit(data$height[i], "npc"),
        just = c(data$hjust[i], data$vjust[i]),

        angle = data$angle[i]
      ),
      interpolate = FALSE,
      name = paste0("ggdal.grob.", i)
    )
  } else if (isTRUE(is_theme_element)){
    grid::rasterGrob(
      img,
      vp = grid::viewport(
        x = grid::unit(data$x[i], "npc"),
        y = grid::unit(data$y[i], "npc"),
        width = grid::unit(data$width[i], "npc"),
        height = grid::unit(data$height[i], "npc"),
        angle = data$angle[i]
      ),
      just = c(data$hjust[i], data$vjust[i]),
      interpolate = FALSE,
      name = paste0("ggdal.grob.", i)
    )
  }
}


#' @rdname ggdal-package
#' @export
geom_from_gdal <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = FALSE,
                           inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFromGDAL,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggdal-package
#' @export
GeomFromGDAL <- ggplot2::ggproto(
  "GeomFromGDAL", ggplot2::Geom,
  required_aes = c("x", "y", "gdal"),
  # non_missing_aes = c(""),
  default_aes = ggplot2::aes(
    alpha = NULL, colour = NULL, angle = 0, hjust = 0.5,
    vjust = 0.5, width = 1.0, height = 1.0, px = 64, py = 64, resample = "bilinear"
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data <- coord$transform(data, panel_params)

    grobs <- lapply(
      seq_along(data$gdal),
      build_gdal_grobs,
      alpha = data$alpha,
      colour = data$colour,
      gdal = data$gdal,
      px = data$px,
      py  = data$py,
      resample = data$resample,
      data = data
      )

    class(grobs) <- "gList"

    grid::gTree(children = grobs)
  },
  draw_key = function(...) grid::nullGrob()
)
