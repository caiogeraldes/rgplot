require("tibble")
require("stringr")
require("rethinking")
require("ggplot2")

plot_dens_boundaries <- function(
    #' Plot Density curve with colored area inside given boundaries
    #'
    #' @param samples vector containing samples drawn from the posterior
    #' @param boundaries lower or upper boundaries for coloring the area below
    #' the density
    #' @param posterior (optional) vector containing the posterior
    #' @param samples vector containing samples drawn from the posterior
    #' @param xlabel,ylabel,title strings for formating the plot
    #'
    #' @return plot
    #' @export
    samples, boundaries, posterior = NULL,
    xlabel = "posterior", ylabel = "density", title = "") {
  if (length(boundaries) == 1) {
    boundaries <- c(0, boundaries[1])
  }
  samples_dens <- with(
    density(
      samples,
      adjust = 0.5,
      from = 0,
      to = 1
    ), tibble(x, y)
  )
  if (!is.null(posterior)) {
    x <- seq(from = 0, to = 1, length.out = length(posterior))
    y <- posterior

    df <- tibble(
      x = x,
      y = y
    )
  } else {
    df <- samples_dens
  }
  plt <- df %>% ggplot(aes(x, y)) + # nolint
    geom_line() +
    geom_area(
      data = subset(
        df,
        x > boundaries[1] & x < boundaries[2]
      ),
      fill = "purple",
      alpha = 0.5
    ) +
    xlab(xlabel) +
    ylab("density") +
    labs(title = title)
  return(plt)
}

plot_grid_hdpi <- function(
    posterior, prob, sample_size = 1e4,
    xlabel = "probability") {
  #' Plot Density curve with colored area inside the HPDI for the given
  #' probability, having sampled from the posterior.
  #'
  #' @param posterior vector containing the posterior
  #' @param prob probability density
  #' @param sample_size number of samples to be drawn from the posterior
  #' @param xlabel string for formating the plot
  #'
  #' @return plot
  #' @export
  p_grid <- seq(from = 0, to = 1, length.out = length(posterior))
  samples <- sample(p_grid,
    prob = posterior,
    size = sample_size, replace = TRUE
  )
  boundaries <- HPDI(samples, prob)
  title <- stringr::str_c("HPDI (prob=", prob, ")")
  plt <- plot_dens_boundaries(
    samples, boundaries, posterior, xlabel,
    title = title
  )
  return(plt)
}

plot_grid_pi <- function(posterior, prob, sample_size = 1e4, xlabel = "probability") {
  #' Plot Density curve with colored area inside the PI for the given
  #' probability, having sampled from the posterior.
  #'
  #' @param posterior vector containing the posterior
  #' @param prob probability density
  #' @param sample_size number of samples to be drawn from the posterior
  #' @param xlabel string for formating the plot
  #'
  #' @return plot
  #' @export
  p_grid <- seq(from = 0, to = 1, length.out = length(posterior))
  samples <- sample(p_grid,
    prob = posterior,
    size = sample_size, replace = TRUE
  )
  boundaries <- PI(samples, prob)
  title <- stringr::str_c("PI (prob=", prob, ")")
  plt <- plot_dens_boundaries(
    samples, boundaries, posterior, xlabel,
    title = title
  )
  return(plt)
}
