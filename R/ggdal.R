

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



#' ggplot2 Layer for Visualizing Images from GDAL sources (file path, URLs, database connection strings, VRT text in memory, etc. )
#'
#' @description This geom is used to plot raster data from a GDAL source
#'   of points in a ggplot. Currently requeires x,y aesthetics as well as a gdal DSN, but generally these can
#' obtained from the source, and a graphics device can dictate that the GDAL warper provides the data in a given
#' extent ....
#'
#' @inheritParams ggplot2::geom_point
#' @section Aesthetics:
#' `geom_from_gdal()` understands the following aesthetics (required aesthetics are in bold):
#'  FIXME: px, py, resample, etc - do we want to have this graphics placement or totally override with xmin, xmax, ymin, ymax, etc
#' \itemize{
#'   \item{**x**}{ - The x-coordinate.}
#'   \item{**y**}{ - The y-coordinate.}
#'   \item{**path**}{ - a file path, url, raster object or bitmap array. See [`magick::image_read()`] for further information.}
#'   \item{`alpha = NULL`}{ - The alpha channel, i.e. transparency level, as a numerical value between 0 and 1.}
#'   \item{`colour = NULL`}{ - The image will be colorized with this colour. Use the special character `"b/w"` to set it to black and white. For more information on valid colour names in ggplot2 see <https://ggplot2.tidyverse.org/articles/ggplot2-specs.html?q=colour#colour-and-fill>}
#'   \item{`angle = 0`}{ - The angle of the image as a numerical value between 0° and 360°.}
#'   \item{`hjust = 0.5`}{ - The horizontal adjustment relative to the given x coordinate. Must be a numerical value between 0 and 1.}
#'   \item{`vjust = 0.5`}{ - The vertical adjustment relative to the given y coordinate. Must be a numerical value between 0 and 1.}
#'   \item{`width = 1.0`}{ - The desired width of the image in `npc` (Normalised Parent Coordinates).
#'                           The default value is set to 1.0 which is *big* but it is necessary
#'                           because all used values are computed relative to the default.
#'                           A typical size is `width = 0.1` (see below examples).}
#'   \item{`height = 1.0`}{ - The desired height of the image in `npc` (Normalised Parent Coordinates).
#'                            The default value is set to 1.0 which is *big* but it is necessary
#'                            because all used values are computed relative to the default.
#'                            A typical size is `height = 0.1` (see below examples).}
#' }
#' @param ... Other arguments passed on to [ggplot2::layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value. See the below
#'   section "Aesthetics" for a full list of possible arguments.
#' @return A ggplot2 layer ([ggplot2::layer()]) that can be added to a plot
#'   created with [ggplot2::ggplot()].
#' @export
#' @examples
#' library(ggplot2)
#' library(ggdal)
#' ## R logo file shipped with ggpath
#' dsn <- system.file("r_logo.png", package = "ggpath")
#' #create dataframe with x-y-coordinates and the above gdal source
#' plot_data <- data.frame(x = 0, y = c(-1, 1), gdal = dsn, px = 18, py = 18,
#'   resample = c("near", "cubic"))
#' # plot images directly from local path, with vectorized resampling method invoked by GDAL itself
#' ggplot(plot_data, aes(x = x, y = y, px = px, py = py, resample = resample)) +
#'   geom_from_gdal(aes(gdal = gdal), width = 0.3, height = .3) +
#'   coord_cartesian(xlim = c(-1, 1), ylim = c(-2, 2)) +
#'   theme_minimal()
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

#' @name ggdal-package
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
