utils::globalVariables(c(".data"))

# Support function
points_on_line <- function(.x, x1, y1, x2, y2) {
  .slope <- (y2 - y1) / (x2 - x1)
  .intercept <- y2 - .slope * x2
  .intercept + .x * .slope
}

#' @title make_smooth_density
#'
#' Takes points and turns them into a density line. The density curve is the multiplied with the group range in x times group mean in y.
#'
#' @param .df a data frame that must contain x and y
#' @param bw bandwidth of kernal density
#' @param n_grid number of x points that should be calculated. The higher the more smooth plot.
#' @param min_x minimum x value of all groups
#' @param max_x maximum x value of all groups
#'
#' @return a data frame
#'
#' @export
make_smooth_density <- function(.df, bw = bw, n_grid = n_grid, min_x, max_x){
  .group <- dplyr::first(.df$group)
  .df <- .df %>% tidyr::drop_na()
  range_dist <- max_x - min_x
  bwidth = bw
  
  w <- .df$y / sum(.df$y)
  m <- stats::density(.df$x, weights = w, from = min_x - range_dist, to = max_x + range_dist, n = n_grid, bw = bwidth)
  df <- dplyr::tibble(x = m$x,
                      y = m$y) %>%
    dplyr::filter(dplyr::case_when(x <= max_x & x >= min_x ~ T,
                                   .data$y > 1/10000 * max(.data$y) ~ T,
                                   T ~ F))
  
  # Unnormalize density so that height matches true data relative size
  group_min_x <- min(.df$x, na.rm = T)
  group_max_x <- max(.df$x, na.rm = T)
  group_average_y <- mean(.df$y)
  mulitplier <- abs(group_max_x - group_min_x) * group_average_y
  df$y <- df$y * mulitplier
  
  dplyr::tibble(
    x = df$x,
    y = df$y,
    group = .group
  )
}

#' @title make_smooth_loess
#'
#' Takes points and turns them into a LOESS-estimated line.
#'
#' @param .df a data frame that must contain x and y
#' @param bw bandwidth of kernal density
#' @param n_grid number of x points that should be calculated. The higher the more smooth plot.
#' @param min_x minimum x value of all groups
#' @param max_x maximum x value of all groups
#'
#' @return a data frame
#'
#' @export
make_smooth_loess <- function(.df, bw = bw, n_grid = n_grid, min_x, max_x){
  .group <- dplyr::first(.df$group)
  .df <- .df %>% tidyr::drop_na()
  bwidth <- bw
  
  m <- stats::loess(y~x, .df, span = bw)
  x_range <- seq(min_x, max_x, length.out = n_grid)
  df <- dplyr::tibble(x = x_range,
                      y = stats::predict(m, newdata = dplyr::tibble(x = x_range))) %>%
    dplyr::mutate(y = dplyr::if_else(.data$y < 0, 0, .data$y)) %>%
    tidyr::drop_na()
  
  dplyr::tibble(
    x = df$x,
    y = df$y,
    group = .group
  )
}


#' @title make_connect_dots
#'
#' Returns n number of points from data that perfectly fits data.
#'
#' @param .df a data frame that must contain x and y
#' @param n_grid number of x points that should be calculated. The higher the more smooth plot.
#' @param min_x minimum x value of all groups
#' @param max_x maximum x value of all groups
#'
#' @return a data frame
#'
#' @export
make_connect_dots <- function(.df, n_grid = n_grid, min_x, max_x){
  .df <- .df %>%
    dplyr::group_by(.data$x, .data$group) %>%
    dplyr::summarise(y = mean(.data$y)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$x)
  .group <- dplyr::first(.df$group)
  .df <- .df %>% tidyr::drop_na()
  range_x <- seq(min_x, max_x, length.out = n_grid)
  steps <- .df$x[-1]
  outside_local_range <- range_x[range_x < min(.df$x) | range_x > max(.df$x)]
  inside_local_range <- range_x[range_x >= min(.df$x) & range_x <= max(.df$x)]
  
  .df <- .df %>%
    dplyr::transmute(xlag = dplyr::lag(.data$x),
                     ylag = dplyr::lag(.data$y),
                     .data$x,
                     .data$y) %>%
    tidyr::drop_na()
  
  .inrange_out <- purrr::pmap_dfr(.df, ~{
    .xlr <- inside_local_range[inside_local_range >= ..1 & inside_local_range <= ..3]
    dplyr::tibble(x = .xlr,
                  y = points_on_line(.xlr, ..1, ..2, ..3, ..4))
  })
  
  out <- dplyr::bind_rows(
    .inrange_out,
    dplyr::tibble(x = outside_local_range,
                  y = rep(0, length(outside_local_range)))
  ) %>%
    dplyr::arrange(.data$x) %>%
    dplyr::mutate(group = .group)
  out
}


#' @title stack_densities
#'
#' Takes density lines of equal x range and stack them on top of each other symmetrically aournd zero.
#'
#' @param data a data frame
#' @param bw bandwidth of kernal density
#' @param n_grid number of x points that should be calculated. The higher the more smooth plot.
#' @param center_fun a function that returns the y center for each possible x in range of x.
#' @param method which estimation should be applied. Default is LOESS.
#'
#' @return a data frame
#'
#'
#' @export
stack_densities <- function(data, bw = bw, n_grid = n_grid, center_fun = center_fun, method = method) {
  if(is.null(center_fun)) {center_fun <- function(x) {return(0)}
  }
  fun <- if(method == "density") {
    ~make_smooth_density(.,
                         bw = bw,
                         n_grid = n_grid,
                         min_x = range(data$x, na.rm = T)[1],
                         max_x = range(data$x, na.rm = T)[2])
  } else if(method == "loess") {
    ~make_smooth_loess(.,
                       bw = bw,
                       n_grid = n_grid,
                       min_x = range(data$x, na.rm = T)[1],
                       max_x = range(data$x, na.rm = T)[2])
  }  else if(method == "raw") {
    ~make_connect_dots(.,
                       n_grid = n_grid,
                       min_x = range(data$x, na.rm = T)[1],
                       max_x = range(data$x, na.rm = T)[2])
  }
  data <- purrr::map_dfr(data %>% split(data$group), fun)
  
  data <- data %>%
    dplyr::mutate(group_tmp = factor(.data$group) %>% as.numeric()) %>%
    dplyr::arrange(.data$x, .data$group_tmp) %>%
    dplyr::group_by(.data$x) %>%
    dplyr::mutate(ymin = purrr::accumulate(.data$y, ~.x + .y, .init = -(sum(.data$y) / 2), .dir = "backward")[-1],
                  ymax = .data$ymin + .data$y) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at(dplyr::vars("ymin", "ymax"), ~{. + center_fun(.data$x)})
  
  data <- purrr::map_dfr(data %>% split(data$group),
                         ~{
                           .x <- .x %>% dplyr::arrange(x)
                           dplyr::tibble(
                             x = c(.x$x, rev(.x$x)),
                             y = c(.x$ymin, rev(.x$ymax)),
                             group = dplyr::first(.x$group))
                         }
  )
  data
}


StatStreamDensity <- ggplot2::ggproto("StatStreamDensity", ggplot2::Stat,
                                      required_aes = c("x", "y"),
                                      extra_params = c("bw", "n_grid", "na.rm", "center_fun", "method"),
                                      setup_data = function(data, params) {
                                        .panels <- unique(data$PANEL)
                                        .per_panel <- purrr::map_dfr(.panels, ~{
                                          data %>%
                                            dplyr::filter(PANEL == .x) %>%
                                            stack_densities(
                                              params$bw,
                                              params$n_grid,
                                              params$center_fun,
                                              params$method
                                            ) %>%
                                            dplyr::mutate(PANEL = .x)
                                        }) %>%
                                          dplyr::mutate(PANEL = factor(PANEL))
                                        
                                        suppressWarnings(data %>%
                                                           dplyr::select(-"x", -"y") %>%
                                                           dplyr::distinct() %>%
                                                           dplyr::left_join(.per_panel, by = c("group", "PANEL"))
                                        )
                                      },
                                      
                                      compute_group = function(data, scales) {
                                        data
                                      }
)

#' @title geom_stream
#'
#' stat to compute `geom_stream`
#'
#' @param mapping provide you own mapping. both x and y need to be numeric.
#' @param data provide you own data
#' @param geom change geom
#' @param position change position
#' @param na.rm remove missing values
#' @param show.legend show legend in plot
#' @param bw bandwidth of kernal density estimation
#' @param n_grid number of x points that should be calculated. The higher the more smooth plot.
#' @param center_fun a function that returns the y center for each possible x in range of x.
#' @param method Which method of estimation should be used. Default is LOESS, similar to `geom_smooth` but sets negative values to zero.
#' @param inherit.aes should the geom inherits aestethics
#' @param ... other arguments to be passed to the geom
#'
#' @return a data frame
#'
#' @examples
#'
#' library(ggplot2)
#' set.seed(123)
#'  df <- data.frame(x = rep(1:10, 3),
#'                   y = rpois(30, 2),
#'                   group = sort(rep(c("A", "B", "C"), 10)))
#' ggplot(df, aes(x, y, fill = group)) +
#'   geom_stream(bw = .6)
#'
#' ggplot(df, aes(x, y, fill = group)) +
#'   geom_stream(method = "raw", bw = .6)
#'
#' ggplot(df, aes(x, y, fill = group)) +
#'   geom_stream(method = "density")
#'
#' @export
geom_stream <- function(mapping = NULL, data = NULL, geom = "polygon",
                        position = "identity", show.legend = NA,
                        inherit.aes = TRUE, na.rm = T, bw = 0.75, n_grid = 3000, method = c("loess", "density", "raw"), center_fun = NULL, ...) {
  method <- match.arg(method)
  ggplot2::layer(
    stat = StatStreamDensity, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, bw = bw, center_fun = center_fun, n_grid = n_grid, method = method, ...)
  )
}








