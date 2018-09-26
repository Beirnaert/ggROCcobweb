#' Theme for cobweb visualization
#'
#' To correctly plot the geom_cobweb this theme is required. 
#' \code{theme_net} provides access to the regular ggplot2 theme, but removes any
#' background, axes, and ensures an aspect ratio of 1 for better
#' viewing of networks and graphs.
#'
#' @param base_size base font size
#' @param base_family base font family
#'
#' @examples
#' library(ggplot2)
#' library(MetaboMeeseeks)
#' ggplot(AUC.df, aes(AUCs = AUCs, group = as.factor(type), colour =as.numeric(type))) + 
#' geom_cobweb() + 
#' theme_cobweb()
#'
#' @name theme_cobweb
#' 
#' @export
#' @import ggplot2
theme_cobweb <- function (base_size = 11, base_family = "")
{
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
        theme(
            plot.background = element_rect(fill = "white", colour = "white"),
            plot.margin = grid::unit(c(-0.2,0.4,-0.2,-0.6), unit="cm"),
            panel.background = element_rect(fill = "white", colour = "white"),
            legend.box.spacing = grid::unit(-0.6, unit="cm"),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            
            legend.key = element_rect(fill = NA, colour = "white"),
            
            panel.border = element_blank(),
            panel.grid = element_blank(),
            
            aspect.ratio = 1
        )
}
