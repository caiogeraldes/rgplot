require("tibble")
require("ggplot2")

plot_dens_boundaries <- function(samples,
                                 boundaries,
                                 posterior = NULL,
                                 xlabel = "posterior",
                                 ylabel = "density", title = "") {
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
