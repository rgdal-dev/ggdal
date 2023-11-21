.ideal_dim <- function(dimension = dev.size("px"), extent = c(0, 1, 0, 1)) {
  rat <- diff(extent[1:2])/diff(extent[3:4])
  #print(paste("rat:", rat))
  if (rat < 1) {
    width <- dimension[2L] * rat
    height <- dimension[2L]
  }
  if (rat >= 1) {
    width <- dimension[1L]
    height <- dimension[1L]/rat
  }
  #dm <- as.integer(c(width, height))
  #print(dm)
  #print(diff(par("plt"))[c(1, 3)] * dm0)

  ## using plt is removing too much
  #as.integer(round(diff(par("plt"))[c(1, 3)] * c(width, height)))
  as.integer(c(width, height) * .95)
  #c(dimension[1L], 0)
}



#' Add background imagery
#'
#' Uses OpenStreetMap or VirtualEarth to add background imagery, or a custom source via 'dsn'.
#'  If you are publishing
#' a map using these tiles, make sure to use the proper attribution
#' (e.g., "Copyright OpenStreetMap contributors" when using an
#' OpenStreetMap-based tile set).  Ditto for VirtualEarth or any dsn you use.
#'
#' @param dsn The map source (currently 'osm' or 'virtualearth' are built-in -streetmap, or imagery, otherwise use a GDAL DSN - see package {sds} for some helpers)
#' @param interpolate Passed to [grid::rasterGrob()]
#' @param alpha Use to make this layer semi-transparent
#' @param resample resample algorithm for the GDAL warper
#' @param data,mapping Specify data and mapping to use this geom with facets
#'
#' @return A ggplot2 layer
#' @export
#' @importFrom vapour gdal_raster_image
#' @importFrom scales alpha
#' @importFrom grDevices as.raster rgb col2rgb
#' @importFrom grid rasterGrob
#' @examples
#' \donttest{
#' library(ggplot2)
#' data(iw)
#'
#' ggplot() +
#'   annotation_gdal() +
#'   geom_sf(data = sf::st_transform(iw, "EPSG:3857"), fill = NA, col = "grey50")
#'
#'  ggplot() +
#'   annotation_gdal("virtualearth") +
#'   geom_sf(data = iw, fill = NA, col = "grey50")
#'
#'
#' pts <- do.call(cbind, maps::map(plot = FALSE)[1:2])
#' pts <- pts[!is.na(pts[,1]), ]
#' pts <- pts[seq(1, nrow(pts), length.out = 8000), ]
#' sf <- sf::st_sf(geom = sf::st_sfc(sf::st_multipoint(pts), crs = "OGC:CRS84"))
#' ggplot() +
#'   annotation_gdal(dsn = "virtualearth") +
#'   geom_sf(data = sf::st_transform(sf, "+proj=laea +lon_0=147 +lat_0=-42"),
#'   fill = NA, col = "yellow", pch = ".")
#'
#' pts2 <- pts[pts[,1] > 50 & pts[,1] < 120 & pts[,2] < -20 & pts[,2] > -45, ]
#' sf <- sf::st_sf(geom = sf::st_sfc(sf::st_multipoint(pts2), crs = "OGC:CRS84"))
#' ggplot() +
#'   annotation_gdal(dsn = "osm") +
#'   geom_sf(data = sf::st_transform(sf, "EPSG:3577"), fill = NA, col = "yellow", pch = ".")
#' pts3 <- pts[ pts[,2] < -20, ]
#' sf <- sf::st_sf(geom = sf::st_sfc(sf::st_multipoint(pts3), crs = "OGC:CRS84"))
#' wms_arcgis_mapserver_tms <- arcgis_mapserver_imgery()
#' ggplot() +
#'   annotation_gdal(dsn = wms_arcgis_mapserver_tms, resample ="lanczos") +
#'   geom_sf(data = sf::st_transform(sf, "EPSG:3031"), fill = NA, col = "hotpink", pch = 19, cex = 0.2)
#'
#' }
#'
annotation_gdal <- function(dsn = c("osm", "virtualearth"), resample = "bilinear",
                            interpolate = FALSE, data = NULL, mapping = NULL, alpha = 1) {

  if(is.null(data)) {
    data <- data.frame(dsn = dsn, resample = resample, stringsAsFactors = FALSE)
    mapping <- ggplot2::aes(dsn = dsn, resample = resample)
  }

  c(
    ggplot2::layer(
      data = data,
      mapping = mapping,
      geom = GeomGdal,
      stat = "identity",
      position = "identity",
      params = list(
        interpolate = interpolate,
        alpha = alpha
      ),
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  )
}

#' @export
#' @rdname annotation_gdal
GeomGdal <- ggplot2::ggproto(
  "GeomGdal",
  ggplot2::Geom,

  extra_params = "",

  handle_na = function(data, params) {
    data
  },

  default_aes = ggplot2::aes(dsn = "osm", resample = "bilinear" ),

  draw_panel = function(
    data, panel_params, coordinates,  interpolate = TRUE, alpha = 1
  ) {


    coord_crs <- sf::st_crs(panel_params$crs)
    ex <- c(panel_params$x_range, panel_params$y_range)
    prj <- coord_crs$wkt


    ## here we need the dims to be smart, just pivot on the aspect ratio to a constant size for now
    dm <- .ideal_dim(extent = ex)
    src <- NULL

    dsn <- as.character(data$dsn[[1L]])
    resample <- as.character(data$resample[[1L]])
    if (dsn == "osm") {
      src <- sds::wms_openstreetmap_tms()
    }
    if (dsn == "virtualearth") {
      src <- sds::wms_virtualearth()
    }

    ## the user has passed in a DSN

    if (is.null(src))  src <- dsn
    ## dm might have a zero in it, so we use the record attribute "dimension"
    #print(paste("nr/nc:", dm[1]/dm[2]))
    #print(paste("dm", dm))

    #print(paste("px:", dev.size("px")))
    dpix <- vapour::gdal_raster_image(src, target_ext  = ex, target_crs = prj, target_dim = dm, resample = resample)
    img <- as.raster(matrix(dpix[[1L]], attr(dpix, "dimension")[2L], byrow = TRUE))
    if (alpha < 1) {
      img <- as.raster(matrix(scales::alpha(img, alpha = alpha), dm[2], byrow = TRUE))
    }
    ## again, override the extent (so we can allow no extent to be passed in )
    ex <- attr(dpix, "extent")
    corners <- data.frame(x = ex[1:2], y = ex[3:4])

    # transform corners to viewport cs
    corners_trans <- coordinates$transform(corners, panel_params)
    x_rng <- range(corners_trans$x, na.rm = TRUE)
    y_rng <- range(corners_trans$y, na.rm = TRUE)

    # return raster grob of the img
    grid::rasterGrob(
      img,
      x_rng[1], y_rng[1],
      diff(x_rng), diff(y_rng), default.units = "native",
      just = c("left","bottom"), interpolate = interpolate
    )
  }
)

