
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(ggmap)

convert_to_cobweb_bg <- function(data, grid_ticks, grid_labels){
    print(data)
    if(is.null(grid_ticks)){
        ticks = pretty(c(0,max(data$AUCs)), n = 4)
        labels = as.character(ticks)
    } else if(!is.null(grid_ticks) & is.null(grid_labels)){
        ticks = as.numeric(grid_ticks)
        labels = as.character(grid_ticks)
    } else if(!is.null(grid_ticks) & !is.null(grid_labels)){
        ticks = as.numeric(grid_ticks)
        if(length(grid_labels) != length(grid_ticks)){
            labels = as.character(grid_ticks)
        } else{
            labels = as.character(grid_labels)
        }
    }
    
    if(0 %in% ticks){
        labels = labels[ticks !=0]
        ticks = ticks[ticks !=0]
    }
    
    labels = as.factor(labels)
    
    bg = data.frame(ticks, labels)
    return(bg)
}

Polar2xy <- function(AUCs){
    thetas <- seq(pi/2, 5*pi/2, length.out = length(AUCs)+1 )
    radii <- c(AUCs, AUCs[1])
    
    AUCcoordinates <- data.frame(x = 0.5+(radii*cos(thetas)/2), y = 0.5+(radii*sin(thetas)/2))
    AUCcoordinates[nrow(AUCcoordinates),] <- AUCcoordinates[1,]
    return(AUCcoordinates)
}

cobweb_stat <- ggproto("cobweb_stat", Stat,
                       required_aes = c("ticks", "labels"),
                       compute_group = function(data) {
                           # background = data.frame(levels = c(rep(0.5,6),
                           #                                    rep(0.25,6), 
                           #                                    rep(0.75,6), 
                           #                                    rep(1,6)), 
                           #                         level_factors = factor(c(rep("backg1",6),
                           #                                                  rep("backg2",6),
                           #                                                  rep("backg3",6),
                           #                                                  rep("backg4",6)) )
                           # )
                           
                           background = data.frame(rep(data$ticks, 6), level_factors = rep(data$labels, 6))
                           
                           data.frame(AUCs = background$levels,  group = background$level_factors)
                       }
)


geom_cobweb_proto <- ggplot2::ggproto("geom_cobweb_proto", Geom,
                                      required_aes = c("AUCs"),
                                      default_aes = aes(colour = "black", alpha = 1, linetype = "solid", size = 1),
                                      draw_key = draw_key_abline,
                                      draw_group = function(data, panel_params, coord) {
                                          
                                          data.df = data.frame(x = Polar2xy(AUCs = data$AUCs)$x, y = Polar2xy(AUCs = data$AUCs)$y)
                                          #print("data.df")
                                          #print(data.df)
                                          
                                          ## Transform the data first
                                          
                                          coords <- coord$transform(data.df, panel_params)
                                          #print("coords")
                                          #print(coords)
                                          
                                          df_points = base::data.frame(colour = data$colour,
                                                                       x = coords$x[1: (length(coords$x)-1)],
                                                                       y = coords$y[1: (length(coords$y)-1)],
                                                                       group = data$group,
                                                                       alpha = data$alpha,
                                                                       linetype = data$linetype, 
                                                                       size = data$size
                                          )
                                          
                                          df_points = rbind(df_points, df_points[1,])
                                          
                                          # Convert to character
                                          df_points$colour <- base::as.character(df_points$colour)
                                          df_points$linetype <- base::as.character(df_points$linetype)
                                          
                                          #print("df_points")
                                          #print(df_points)
                                          
                                          ## transform data points
                                          coords_df <- coord$transform(df_points, panel_params)
                                          #print("coords_df")
                                          #print(coords_df)
                                          
                                          #cobwebcrap = list(coords_df = coords_df)
                                          #list2env(cobwebcrap, envir = .GlobalEnv)
                                          
                                          ## Construct grid polygon
                                          grid::linesGrob(
                                              x= coords_df$x,
                                              y = coords_df$y,
                                              gp = grid::gpar(col = coords_df$colour,  
                                                              alpha = coords_df$alpha, 
                                                              lty = coords_df$linetype,
                                                              lwd = coords_df$size)
                                          )
                                          
                                          
                                          
                                      }
                                      
)


geom_cobweb_bg_proto <- ggplot2::ggproto("geom_cobweb_bg_proto", Geom,
                                         required_aes = c("AUCs"),
                                         default_aes = aes(colour = "black", alpha = 0.75, linetype = "solid", size = 1, fontsize = 12),
                                         draw_key = draw_key_abline,
                                         draw_group = function(data, panel_params, coord) {
                                             #print(data)
                                             
                                             
                                             data.df = data.frame(x = Polar2xy(AUCs = data$AUCs)$x, y = Polar2xy(AUCs = data$AUCs)$y)
                                             
                                             ## Transform the data first
                                             #coords <- coord$transform(data, panel_params)
                                             coords <- coord$transform(data.df, panel_params)
                                             
                                             df_points = base::data.frame(colour = data$colour,
                                                                          x = coords$x[1: (length(coords$x)-1)],
                                                                          y = coords$y[1: (length(coords$y)-1)],
                                                                          group = data$group,
                                                                          alpha = data$alpha,
                                                                          linetype = data$linetype, 
                                                                          size = data$size
                                             )
                                             
                                             df_points = rbind(df_points, df_points[1,])
                                             
                                             # Convert to character
                                             df_points$colour <- base::as.character(df_points$colour)
                                             df_points$linetype <- base::as.character(df_points$linetype)
                                             
                                             ## transform data points
                                             coords_df <- coord$transform(df_points, panel_params)
                                             
                                             ## Construct grid polygon
                                             bg_grob <- grid::linesGrob(
                                                 x = coords_df$x,
                                                 y = coords_df$y,
                                                 gp = grid::gpar(col = coords_df$colour,  
                                                                 alpha = coords_df$alpha, 
                                                                 lty = coords_df$linetype,
                                                                 lwd = coords_df$size)
                                             )
                                             
                                             bg_labels <- grid::textGrob(label = as.character(data$AUCs[1]),
                                                                         x = coords_df$x[1],
                                                                         y = coords_df$y[1],
                                                                         hjust = 0.5,
                                                                         vjust = -0.5,
                                                                         default.units = "npc",
                                                                         gp=grid::gpar(alpha = data$alpha[1], fontsize = data$fontsize))
                                             
                                             grid::gList(bg_grob,bg_labels)
                                             
                                         }
                                         
)



geom_cobweb <- function(mapping = NULL, data = NULL, stat = 'identity',
                        position = 'identity', na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE,  
                        theme = list(panel.background = element_rect(fill = "white", colour = "white")), 
                        grid_lty = "dashed", 
                        grid_lwd = 1, 
                        lwd = 2, 
                        grid_alpha = 0.75, 
                        grid_fontsize = 12, 
                        grid_ticks = NULL, 
                        grid_labels = NULL, ...) {
    
    #background = data.frame(levels = c(rep(0.5,6),rep(1,6)), level_factors = factor(c(rep("backg1",6),rep("backg2",6)) ) )
    # background = data.frame(levels = c(rep(0.5,6),
    #                                    rep(0.25,6), 
    #                                    rep(0.75,6), 
    #                                    rep(1,6)), 
    #                         level_factors = factor(c(rep("backg1",6),
    #                                                  rep("backg2",6),
    #                                                  rep("backg3",6),
    #                                                  rep("backg4",6)) )
    # )
    
    #bg = data.frame(ticks = c(0.25, 0.5, 0.75, 1)) 
    bg = convert_to_cobweb_bg(data = layer_data(), grid_ticks, grid_labels)
    print(bg)
    list(
        ggplot2::layer(
            geom = geom_cobweb_bg_proto,
            mapping = mapping,
            data = bg,
            stat = cobweb_stat,
            position = position,
            show.legend = FALSE,
            inherit.aes = FALSE,
            params = list(na.rm = na.rm, 
                          linetype = grid_lty, 
                          size = grid_lwd, 
                          alpha = grid_alpha, 
                          fontsize = grid_fontsize, 
                          ticks = ticks, 
                          labels = labels, ...)
        ),
        ggplot2::layer(
            geom = geom_cobweb_proto, 
            mapping = mapping,
            data = data, 
            stat = stat, 
            position = position, 
            show.legend = show.legend, 
            inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, size = lwd ,...)
        ) #,
        # ggplot2::layer(
        #     geom = "label", 
        #     mapping = aes(x = x,  y = y, label = y),
        #     data = background_labels, 
        #     stat = stat, 
        #     position = position, 
        #     show.legend = FALSE, 
        #     inherit.aes = FALSE,
        #     params = list(na.rm = na.rm,...)
        # )
    )
}



ggplot(AUC.df, aes(AUCs = AUCs,  colour = type)) + 
    geom_cobweb() + 
    theme_cobweb() 



