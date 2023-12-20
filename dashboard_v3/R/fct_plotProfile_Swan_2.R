#' plotProfile_Swan_2
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
#' 
library (dplyr)
library (sp)
#library (rivRmon)
library (ggplot2)
library (metR)


# Creates correct colour palette for measured metric
surfer_cols <- function(metric){
  if(metric == "sal"){
    sal_brk <- as.character(seq(2, 42, 2))
    sal_cols <- c("#996633", "#916E3D", "#897648", "#817E53", "#79865D", "#718E68",
                  "#699673", "#619E7E", "#59A688", "#51AE93", "#49B69E", "#41BEA9",
                  "#39C6B3", "#31CEBE", "#29D6C9", "#21DED4", "#19E6DE", "#11EEE9",
                  "#09F6F4", "#00FFFF", "#00FFFF")
    names(sal_cols) <- sal_brk
    return(sal_cols)
  } else if(metric == "do"){
    do_mg_l_brk <- as.character(seq(1, 17, 1))
    do_mg_l_cols <- c("#FF0000", "#FF6600", "#FFCC00", "#DAC35D", "#B5BABA",
                      "#A4E3E3", "#52F1F1", "#29DFF8", "#00CCFF", "#61EDC7",
                      "#99FF99", "#99FF4D", "#99FF00", "#73D90C", "#4DB319",
                      "#278D26", "#006633")
    names(do_mg_l_cols) <- do_mg_l_brk
    return(do_mg_l_cols)
  } else if(metric == "chl"){
    chl_brk <- c(as.character(seq(20, 80, 20)), "120", "160", "200", "300", "400", "1000")
    # chlr_cols <- c("white","#d6f5d6","#99e699", "#47d147", "#248f24", "#145214",
    #                "#990000", "#ff0000")
    chlr_cols <- c("#e6ffff","#ebf9eb","#ccf2cc", "#a3e8a3", "#7ade7a", "#52d452",
                   "#2eb82e", "#990000", "#cc0000", "#ff0000")
    names(chlr_cols) <- chl_brk
    return(chlr_cols)
  } else {
    temp_brk <- as.character(seq(11, 33, 1))
    temp_cols <- c("#00FFFF", "#0CECFF", "#19D9FF", "#26C6FF", "#33B3FF",
                   "#3FA0FF", "#4C8DFF", "#5284FF", "#597AFF", "#6373F0",
                   "#6D6BE0", "#7764D0", "#825CC0", "#8C55B0", "#974DA0",
                   "#A14590", "#AC3D80", "#B63670", "#C02E60", "#CA2750",
                   "#D51F40", "#DF1830", "#EA1020")
    names(temp_cols) <- temp_brk
    return(temp_cols)
  }
}


plotProfile_Swan_2 <- function(var){
  if(var == "sal"){
    plot <- ggplot()+
      geom_raster(data = tps_list_all[[1]],
                  aes(x=x, y=y, fill = factor(Salinity))) +
      scale_x_continuous(limits = c(-1, 51.6),
                         expand = c(0, 0),
                         breaks = seq(0, 50, by = 5)) +
      scale_y_continuous(expand = expansion(mult = c(0, .05))) +
      stat_contour2(data = tps_list_all[[1]], aes(x=x, y=y, z = Salinity),
                    colour = "grey5",
                    breaks = MakeBreaks(binwidth = 2),
                    size = 0.1) +
      scale_fill_manual(values = surfer_cols("sal"),
                        guide = guide_legend(reverse=T),
                        limits = as.character(seq(2, 42, 2))) +
      geom_rect(data = rectdf_all, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                fill = "black") +
      geom_polygon(data = S_bottom1,
                   aes(x=x, y=y), fill = "grey90", colour = "grey20") +
      geom_point(data = samp_locs,
                 aes(x = x, y = - y),
                 colour = "black",
                 size = 0.5) +
      geom_text(data = site_labs,
                aes(x = dist_mouth/1000, y = 0.9, label = site, fontface=2),
                size = 4.5,
                colour = "black",
                alpha = 1,
                check_overlap = TRUE) +
      annotate("text",
               label = "Salinity (ppt)",
               x = 19,
               y = -16.5,
               size = 9,
               fontface =2,
               colour = "black") +
      labs(y = "") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.length.y.left = (unit(2, "mm")),
            axis.ticks.length.y.right = (unit(2, "mm")),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 18),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            plot.title = element_text(hjust=0.5, vjust=0.5, face='bold', size = 28),
            plot.subtitle = element_text(hjust=0.5, vjust=0.5, size = 22),
            legend.background = element_rect(fill = "transparent"),
            legend.direction = "horizontal",
            legend.position = c(0.65, 0.22),
            legend.key.size =  unit(8, "mm"),
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            plot.margin=grid::unit(c(0,0,0,0), "mm")) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                                 label.position = "bottom"))
    return(plot)
    
  } else if(var == "do"){
    plot <- ggplot()+
      geom_raster(data = tps_list_all[[2]],
                  aes(x=x, y=y, fill = factor(Dissolved_Oxygen))) +
      scale_x_continuous(limits = c(-1, 51.6),
                         expand = c(0, 0),
                         breaks = seq(0, 50, by = 5)) +
      scale_y_continuous(expand = expansion(mult = c(0, .05))) +
      stat_contour2(data = tps_list_all[[2]],
                    aes(x=x, y=y, z = Dissolved_Oxygen),
                    colour = "grey10", 
                    breaks = MakeBreaks(binwidth = 1),
                    size = 0.1) +
      scale_fill_manual(values = surfer_cols("do"),
                        guide = guide_legend(reverse=T),
                        limits = as.character(seq(1, 17, 1))) +
      geom_rect(data = rectdf_all, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                fill = "black") +
      geom_polygon(data = S_bottom1,
                   aes(x=x, y=y), fill = "grey90", colour = "grey20") +
      geom_point(data = samp_locs,
                 aes(x = x, y = - y),
                 colour = "black",
                 size = 0.5) +
      geom_point(data = S_oxy_locs,
                 aes(x = x, y = y),
                 size = 6,
                 colour = "black",
                 bg = S_oxy_locs$c,
                 shape = 24) +
      annotate("text",
               label = "Dissolved Oxygen (mg/L)",
               x = 21,
               y = -16.7,
               size = 9,
               fontface = 2,
               colour = "black") +
      labs(y = "Depth (m)") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.length.y.left = (unit(2, "mm")),
            axis.ticks.length.y.right = (unit(2, "mm")),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 18),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            plot.title = element_text(hjust=0.5, vjust=0.5, face='bold'),
            plot.subtitle = element_text(hjust=0.5, vjust=0.5),
            legend.background = element_rect(fill = "transparent"),
            legend.direction = "horizontal",
            legend.position = c(0.65, 0.22),
            legend.key.size =  unit(8, "mm"),
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            plot.margin=grid::unit(c(0,0,0,0), "mm")) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                                 label.position = "bottom"))
    
    return(plot)
    
  } else if (var == "chl"){
plot <- ggplot()+
  geom_raster(data = tps_list_all[[4]],
              aes(x=x, y=y, fill = factor(Chlorophyll)),
              alpha = 0.5) +
  scale_x_continuous(limits = c(-1, 51.6),
                     expand = c(0, 0),
                     breaks = seq(0, 50, by = 5)) +
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  stat_contour2(data = tps_list_all[[4]],
                aes(x=x, y=y, z = Chlorophyll),
                colour = "grey10",
                breaks = as.numeric(chl_brk),
                size = 0.1) +
  scale_fill_manual(values = surfer_cols("chl"),
                    guide = guide_legend(reverse=T),
                    labels = c("20", "40", "60", "80", "120", "160",
                               "200", "300", "400", "> 400"),
                    limits = c(as.character(seq(20, 80, 20)),
                               "120", "160", "200", "300", "400", "1000")) +
  geom_rect(data = rectdf_all, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
            fill = "black") +
  geom_polygon(data = S_bottom1,
               aes(x=x, y=y), fill = "grey90", colour = "grey20") +
  geom_point(data = samp_locs,
             aes(x = x, y = - y),
             colour = "black",
             size = 0.5) +
  annotate("text",
           label = expression('bold(paste("F-Chlorophyll (", mu,"g/L)"))'),
           x = 20.4,
           y = -16.8,
           size = 9,
           fontface =2,
           colour = "black", parse = TRUE) +
  labs(x = "Distance From Entrance (km)",
       y = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.length.y.left = (unit(2, "mm")),
        axis.ticks.length.y.right = (unit(2, "mm")),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust=0.5, vjust=0.5, face='bold', size = 24),
        plot.subtitle = element_text(hjust=0.5, vjust=0.5, size = 22),
        legend.background = element_rect(fill = "transparent"),
        legend.direction = "horizontal",
        legend.position = c(0.65, 0.22),
        legend.key.size =  unit(8, "mm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                             label.position = "bottom"))
return(plot)

  }  else if(var == "temp"){
plot <- ggplot()+
  geom_raster(data = tps_list_all[[3]],
              aes(x=x, y=y, fill = factor(Temperature))) +
  scale_x_continuous(limits = c(-1, 51.6),
                     expand = c(0, 0),
                     breaks = seq(0, 50, by = 5)) +
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  stat_contour2(data = tps_list_all[[3]],
                aes(x=x, y=y, z = Temperature),
                colour = "grey10",
                breaks = MakeBreaks(binwidth = 1),
                size = 0.1) +
  scale_fill_manual(values = surfer_cols("temp"),
                    guide = guide_legend(reverse=T),
                    limits = as.character(seq(11, 33, 1))) +
  geom_rect(data = rectdf_all, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
            fill = "black") +
  geom_polygon(data = S_bottom1,
               aes(x=x, y=y), fill = "grey90", colour = "grey20") +
  geom_point(data = samp_locs,
             aes(x = x, y = - y),
             colour = "black",
             size = 0.5) +
  annotate("text",
           label = expression('bold(paste("Temperature (", degree,"C)"))'),
           x = 19,
           y = -16.8,
           size = 9,
           fontface =2,
           colour = "black", parse = TRUE) +
  labs(x = "Distance From Entrance (km)",
       y = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        axis.ticks.length.y.left = (unit(2, "mm")),
        axis.ticks.length.y.right = (unit(2, "mm")),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        axis.text.x = element_text(vjust = 0.5),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face = 'bold', size = 24),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 22),
        legend.background = element_rect(fill = "transparent"),
        legend.direction = "horizontal",
        legend.position = c(0.65, 0.22),
        legend.key.size =  unit(8, "mm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                             label.position = "bottom"))
return(plot)

  }
}

