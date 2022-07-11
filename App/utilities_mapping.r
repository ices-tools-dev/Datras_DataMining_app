europe_shape <- rnaturalearth::ne_countries(
                                scale = 10, type = "countries",
                                continent = "europe",
                                returnclass = "sf"
                        )
europe_shape <- europe_shape[, c("iso_a3", "iso_n3", "admin", "geometry")]


map1 <- function(df, europe_shape, xlims, ylims) {
    ggplotly(ggplot2::ggplot() +
        ggplot2::theme_bw(base_size = 10) +
        ggplot2::geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
        ggplot2::coord_sf(xlim = xlims, ylim = ylims) +
        ggplot2::geom_point(ggplot2::aes(x = df$ShootLong, y = df$ShootLat, colour = factor(df$Country))) +
        ggplot2::theme(
            plot.caption = ggplot2::element_text(size = 9),
            plot.subtitle = ggplot2::element_text(size = 9),
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            legend.title = ggplot2::element_blank()
        ))
}

crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

get_map <- function(URL) {
        tmp_file <- tempfile(fileext = ".zip")
        download.file(
                url = URL,
                destfile = tmp_file,
                mode = "wb", quiet = TRUE
        )
        unzip(tmp_file, exdir = tmp_path)
}
tmp_path <- tempdir()
get_map("http://gis.ices.dk/shapefiles/ICES_rectangles.zip")
stat_rec <- sf::st_read(dsn = tmp_path, quiet = FALSE)
stat_rec <- sf::st_transform(stat_rec, crs = crs)


map2 <- function(df, europe_shape, xlims, ylims, stat_rec) {
    ggplotly(ggplot2::ggplot() +
                        ggplot2::theme_bw(base_size = 10) +
                        ggplot2::geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
                        ggplot2::geom_sf(data = stat_rec, aes(fill = df$n)) +
                        ggplot2::scale_fill_viridis_c(alpha = 0.2) +
                        ggplot2::coord_sf(xlim = xlims, ylim = ylims) +
                        ggplot2::theme(
                                plot.caption = ggplot2::element_text(size = 6),
                                plot.subtitle = ggplot2::element_text(size = 7),
                                axis.title.x = ggplot2::element_blank(),
                                axis.title.y = ggplot2::element_blank()
                        ))

}