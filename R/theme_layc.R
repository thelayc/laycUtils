##' Theme for LAYC plots
##'
##' Inspired by theme_few() from ggthemes package
##'
##' @references ggthemes
##' \url{https://github.com/jrnold/ggthemes}.
##'
##' @inheritParams ggplot2::theme_bw
##' @family themes few
##' @export
##' @examples
##' plot <- ggplot(diamonds, aes(x = , y = )) + geom_point()
##' layc_plot <- plot + theme_layc

theme_layc <- function (base_size = 12, base_family = "Calibri") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(line = element_line(colour = "grey"), 
          rect = element_rect(fill = "white", colour = NA),
          text = element_text(colour = "black"), 
          axis.ticks = element_line(colour = "grey"),
          legend.key = element_rect(colour = NA), 
          panel.border = element_rect(colour = "grey"),
          panel.grid = element_blank(), 
          strip.background = element_rect(fill = "white", colour = NA))
}