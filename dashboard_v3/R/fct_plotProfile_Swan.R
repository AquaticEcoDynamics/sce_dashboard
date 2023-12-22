#' plotProfile_Swan
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
#library (sf)
library (rivRmon)
library (ggplot2)
library (metR)
library (grid)

load("data/rivRmon_sysdata.rda")

# select_week <- as.Date('2023-07-03')
# ovit <- 'green'
# ocav <- 'red'

#rawwiski <- awss3Connect(filename = 'arms/wiski.csv')
wiskiEnable <- as.logical(get_golem_config("enable", config = "wiski_connection"))

if(isTRUE(wiskiEnable))
{
  rawwiski <- awss3Connect(filename = 'arms/wiski.csv')
}else{
  rawwiski <- read.csv(file = 'www/wiski.csv', check.names = FALSE)
}

# rawwiski <- read.csv(file = 'C:/Users/00101765/AED Dropbox/Sherry Zhai/SCEVO/data/WQ_Jo/wiski.csv', check.names = FALSE)
# plotDataWeek <- as.Date('2023-01-03')
# ovit <- 'green'
# ocav <- 'red'

rawwiski$`Collect Date` <- as.Date(rawwiski$`Collect Date`,format="%d/%m/%Y")
swansites <- c('ARM','BLA','FP1','FP7','FP22','CAV','HEA','NAR','NIL',
               'STJ','MAY','RON','KIN','SUC','MEAD','MULB','WMP',
               'REG','MSB','POL','SANDBR','KMO','JBC','VIT')

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


plotProfile_Swan <- function(plotDataWeek, StatusOvit, StatusOcav){
  
  library (dplyr)
  library (sp)
  library (rivRmon)
  library (ggplot2)
  library (metR)
  library (grid)
  
 result <- tryCatch({  #this is to print error (if any) and skip instead of crashing the app
  
samp_data <- rawwiski %>%  
  dplyr::filter(`Collect Date` %in% as.Date(plotDataWeek,format="%Y-%m-%d") &
                  `Program Site Ref` %in% swansites & 
                  `Collection Method` %in% 'Insitu' &
                  `Data Category` %in% 'Instrument log')

samp_data <- samp_data %>% 
  rename("site" = "Program Site Ref",
         'c' = 'Temperature (deg C)',
         'sal_ppt' =  'Salinity (ppt)',
         'do_mg_l' = 'O2-{DO conc} (mg/L)',
         'chl_ug_l' = 'Chlorophyll a (in situ) (ug/L)',
         'dep_m' = 'Sample Depth (m)')

samp_data$site <- replace(samp_data$site,samp_data$site == "SANDBR","SAND")

comb_data <- dplyr::left_join(samp_data, S_sitesdf, by = "site")

d_reduced <- comb_data %>%
  dplyr::select(site, sal_ppt, do_mg_l, c, chl_ug_l, dep_m,
                dist_mouth, max_depth)

#### bottom adjust
daily_depth <- d_reduced %>%
  dplyr::group_by(dist_mouth) %>%
  dplyr::summarise(d_depth = -1*(max(dep_m) + 0.2)) %>%
  dplyr::mutate(dist_mouth = dist_mouth/1000)

S_bottom1 <- S_bottom %>%
  dplyr::left_join(daily_depth, by = c("x" = "dist_mouth")) %>%
  dplyr::mutate(y = case_when(
    !is.na(d_depth) ~ d_depth,
    TRUE ~ y
  )) %>%
  dplyr::select(-d_depth)

S_bottom_nar1 <- S_bottom_nar %>%
  dplyr::left_join(daily_depth, by = c("x" = "dist_mouth")) %>%
  dplyr::mutate(y = case_when(
    !is.na(d_depth) ~ d_depth,
    TRUE ~ y
  )) %>%
  dplyr::select(-d_depth)

# set up labels and params to plot
sparams <- c("Salinity", "Dissolved_Oxygen", "Temperature", "Chlorophyll")

# create interpolations and store in  separate lists
d_all <- d_reduced[stats::complete.cases(d_reduced),] %>%
  dplyr::mutate(y = -1 * dep_m, x = dist_mouth/1000)

d_nar <- d_reduced[stats::complete.cases(d_reduced),] %>%
  dplyr::mutate(y = -1 * dep_m, x = dist_mouth/1000) %>%
  dplyr::filter(x >= 21)

vals <- c("sal_ppt", "do_mg_l", "c", "chl_ug_l")



## based on TPS interps from here
tps_list_all <- vector("list", length(vals))
names(tps_list_all) <- vals

tps_list_n_s <- vector("list", length(vals))
names(tps_list_n_s) <- vals

library (sp)

# for all spline TPS
for(i in seq_along(vals)){
  
  #i=1
  
  
  val <- vals[i]
  d1all <- d_all[,c("x", "y", val)]
  names(d1all)[3] <- "value"
  sp::coordinates(d1all) <- ~x + y
  # d1all_sp <- sp::coordinates(d1all) <- ~x + y
  # d1all <- sf::st_as_sf(d1all, coords = c("x", "y"))
  # terra::vect(d1all)
  # d1all_terra <- terra::crds(d1all,"points",atts=NULL, crs="")
  # 
  # tpsmodall <- fields::Tps(d1all, d1all$value)
  
  
  tpsmodall <- fields::Tps(sp::coordinates(d1all), d1all$value)
  S_grd_all_R <- raster::raster(S_grd_all)
  tps_surfaceall <- raster::interpolate(S_grd_all_R, tpsmodall)
  tps_surfaceall[tps_surfaceall < 0] <- 0.1 # spline unfortunately interps to neg vals
  
  tps_r_classall <- raster::reclassify(tps_surfaceall, reclass_matrices[[i]])
  tps_spall <- raster::rasterToPoints(tps_r_classall, spatial = TRUE)
  tps_dfall <- data.frame(tps_spall)[-4]# ditch option
  names(tps_dfall)[1] <- sparams[i]
  tps_list_all[[i]] <- tps_dfall
}

# for narrows up TPS ## changed
# make a mask to crop the whole river to above Narrows
msk <- raster::raster(M_grd_nar)
for(i in seq_along(vals)){
  val <- vals[i]
  d1all <- d_all[,c("x", "y", val)]
  names(d1all)[3] <- "value"
  sp::coordinates(d1all) <- ~x + y
  
  tpsmodnar <- fields::Tps(coordinates(d1all), d1all$value)
  S_grd_nar_R <- raster::raster(S_grd_nar) 
  tps_surfacenar <- raster::interpolate(S_grd_nar_R, tpsmodnar)
  tps_surfacenar_m <- raster::crop(tps_surfacenar, msk) # crop all of river
  tps_surfacenar_m[tps_surfacenar_m < 0] <- 0.1 # spline unfortunately interps to neg vals
  
  tps_r_classnar <- raster::reclassify(tps_surfacenar_m, reclass_matrices[[i]])
  tps_spnar <- raster::rasterToPoints(tps_r_classnar, spatial = TRUE)
  tps_dfnar <- data.frame(tps_spnar)[-4]# ditch option
  names(tps_dfnar)[1] <- sparams[i]
  tps_list_n_s[[i]] <- tps_dfnar
}

# make sample collection points
samp_locs <- comb_data %>%
  dplyr::mutate(dist_mouth = dist_mouth/1000) %>%
  dplyr::rename(x = dist_mouth, y = dep_m) %>%
  dplyr::select(site, x, y)

site_labs <- S_sitesdf %>%
  dplyr::filter(site != "SRP_RSSA") %>%
  dplyr::filter(site != "SAND" & site != "KMO" & site != "VIT")

site_labs_upper <- S_sitesdf %>%
  dplyr::filter(site != "SRP_RSSA") %>%
  dplyr::filter(site != "VIT")

# # construct pretty date
# sday <- just_nums(as.numeric(substr(samp_date, 7, 8)))
# sdate <- paste(sday, format(ymd(samp_date), "%b %Y"), sep = " ")

# oxygenation plant status colour
oxy_col <- c(StatusOvit, StatusOcav)
S_oxy_locs$c <- oxy_col

# data frame of sites in this run
sites_this_week <- tibble(site = unique(d_all$site))


#### All river ####
## All River
# black out areas for plotting
rectdf_all <- S_blockdf_all %>%
  anti_join(sites_this_week, by = "site")


# returndat <- list(
# tps_list_all = tps_list_all,
# rectdf_all = rectdf_all,
# S_bottom1 = S_bottom1,
# samp_locs = samp_locs,
# site_labs = site_labs)
# 
# return(returndat)

# plots
salPlot_s <- ggplot()+
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
           # x = 19,
           # y = -16.5,
           x = 34,
           y = -15,
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
        #legend.direction = "horizontal",
        #legend.position = c(0.65, 0.22),
        legend.position = c(0.65, 0.15),
        legend.key.size =  unit(8, "mm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                             label.position = "bottom"))

#print(salPlot_s)


doPlot_s <- ggplot()+
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
           # x = 21,
           # y = -16.7,
           x = 34,
           y = -15,
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
        #legend.position = c(0.65, 0.22),
        legend.position = c(0.65, 0.15),
        legend.key.size =  unit(8, "mm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                             label.position = "bottom"))

#print(doPlot_s)

chlorPlot_s <- ggplot()+
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
           # x = 20.4,
           # y = -16.8,
           x = 34,
           y = -15,
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
        #legend.position = c(0.65, 0.22),
        legend.position = c(0.65, 0.15),
        legend.key.size =  unit(8, "mm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                             label.position = "bottom"))

#print(chlorPlot_s)

tempPlot_s <- ggplot()+
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
           # x = 19,
           # y = -16.8,
           x = 34,
           y = -15,
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
        #legend.position = c(0.65, 0.22),
        legend.position = c(0.65, 0.15),
        legend.key.size =  unit(8, "mm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                             label.position = "bottom"))

#print(tempPlot_s)
#plts <- lapply(list(salPlot_s, doPlot_s, chlorPlot_s, tempPlot_s), ggplotly)

# plts <- list(salPlot_s, doPlot_s, chlorPlot_s, tempPlot_s)
# 
# plts


# # create list of plotGrobs
# plts <- lapply(list(salPlot_s, doPlot_s, chlorPlot_s, tempPlot_s), ggplotGrob)
# # rbind (i.e. 1 column) size arg matters!
# surfers_s <- rbind(plts[[1]], plts[[2]], plts[[3]], plts[[4]], size = "first")
# # pdf_name <- paste0(path, "/plots/", "swan_", ymd(samp_date), "_surfer.pdf")
# #png_name <- paste0(path, "/plots/", "swan_", ymd(samp_date), "_surfer.png")
#
# # add margin padding coarse but effective
# # surfers_pads <- gtable::gtable_add_padding(surfers_s, padding = unit(c(1,4,3,4), "cm"))
#
# # ggsave(plot = grid.draw(surfers_s), filename = pdf_name, width=28, height=18)
# # cat(paste0(pdf_name,"\n"))
# #png(file = png_name, width = 1500, height = 960, res = 53, bg = "transparent")
# combinedplot <- grid.draw(surfers_s)
# #dev.off()
# #cat(paste0(png_name,"\n"))
#
# combinedplot



#### Upstream zoom-in ####
## Narrows Up
# black out areas for plotting _ recalc as deep FP sites screw up narrows plots
# if present
rectdf_nar <- S_blockdf_nar %>%
  anti_join(sites_this_week, by = "site")

# plots
salPlotZ_s <- ggplot()+
  geom_raster(data = tps_list_n_s[[1]],
              aes(x=x, y=y, fill = factor(Salinity))) +
  scale_x_continuous(limits = c(20.95,51.6),
                     expand = c(0, 0),
                     breaks = c(25, 30, 35, 40, 45, 50)) +
  scale_y_continuous(breaks = c(-8, -6, -4, -2, 0),
                     expand = expansion(mult = c(0, .05)))+
  stat_contour2(data = tps_list_n_s[[1]],
                aes(x=x, y=y, z = Salinity),
                colour = "grey10",
                breaks = MakeBreaks(binwidth = 2),
                size = 0.1) +
  scale_fill_manual(values = surfer_cols("sal"),
                    guide = guide_legend(reverse=T),
                    limits = as.character(seq(2, 42, 2))) +
  geom_rect(data = rectdf_nar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
            fill = "black") +
  geom_polygon(data = S_bottom_nar1,
               aes(x=x, y=y), fill = "grey90", colour = "grey20") +
  geom_point(data = filter(samp_locs, x >= 21),
             aes(x = x, y = - y),
             colour = "black",
             size = 0.5) +
  geom_text(data = filter(site_labs_upper, dist_mouth/1000 >= 21),
            aes(x = dist_mouth/1000, y = 0.7, label = site, fontface=2),
            size = 4.5,
            colour = "black",
            alpha = 1,
            check_overlap = TRUE) +
  annotate("text",
           label = "Salinity (ppt)",
           # x = 32.8,
           # y = -7.4,
           x = 41,
           y = -6.7,
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
        legend.title = element_blank(),
        #legend.position = c(0.65, 0.22),
        legend.position = c(0.65, 0.15),
        legend.key.size =  unit(8, "mm"),
        legend.text = element_text(size = 12),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                             label.position = "bottom"))


doPlotZ_s <- ggplot()+
  geom_raster(data = tps_list_n_s[[2]],
              aes(x=x, y=y, fill = factor(Dissolved_Oxygen))) +
  scale_x_continuous(limits = c(20.95,51.6),
                     expand = c(0, 0),
                     breaks = c(25, 30, 35, 40, 45, 50)) +
  scale_y_continuous(breaks = c(-8, -6, -4, -2, 0),
                     expand = expansion(mult = c(0, .05))) +
  stat_contour2(data = tps_list_n_s[[2]],
                aes(x=x, y=y, z = Dissolved_Oxygen),
                colour = "grey10",
                breaks = MakeBreaks(binwidth = 1),
                size = 0.1) +
  scale_fill_manual(values = surfer_cols("do"),
                    guide = guide_legend(reverse=T),
                    limits = as.character(seq(1, 17, 1))) +
  geom_rect(data = rectdf_nar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
            fill = "black") +
  geom_polygon(data = S_bottom_nar1,
               aes(x=x, y=y), fill = "grey90", colour = "grey20") +
  geom_point(data = filter(samp_locs, x >= 21),
             aes(x = x, y = - y),
             colour = "black",
             size = 0.5) +
  geom_point(data = S_oxy_locs,
             aes(x = x, y = y + 1),#nudge up
             size = 6,
             colour = "black",
             bg = S_oxy_locs$c,
             shape = 24) +
  annotate("text",
           label = "Dissolved Oxygen (mg/L)",
           # x = 34,
           # y = -7.57,
           x = 41,
           y = -7,
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
        #legend.position = c(0.65, 0.22),
        legend.position = c(0.65, 0.15),
        legend.key.size =  unit(8, "mm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                             label.position = "bottom"))

chlorPlotZ_s <- ggplot()+
  geom_raster(data = tps_list_n_s[[4]],
              aes(x=x, y=y, fill = factor(Chlorophyll)),
              alpha = 0.5) +
  scale_x_continuous(limits = c(20.95,51.6),
                     expand = c(0, 0),
                     breaks = c(25, 30, 35, 40, 45, 50)) +
  scale_y_continuous(breaks = c(-8, -6, -4, -2, 0),
                     expand = expansion(mult = c(0, .05))) +
  stat_contour2(data = tps_list_n_s[[4]],
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
  geom_rect(data = rectdf_nar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
            fill = "black") +
  geom_polygon(data = S_bottom_nar1,
               aes(x=x, y=y), fill = "grey90", colour = "grey20") +
  geom_point(data = filter(samp_locs, x >= 21),
             aes(x = x, y = - y),
             colour = "black",
             size = 0.5) +
  annotate("text",
           label = expression('bold(paste("F-Chlorophyll (", mu,"g/L)"))'),
           # x = 33.6,
           # y = -7.6,
           x = 41,
           y = -7,
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
        #legend.position = c(0.65, 0.22),
        legend.position = c(0.65, 0.15),
        legend.key.size =  unit(8, "mm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                             label.position = "bottom"))

tempPlotZ_s <- ggplot()+
  geom_raster(data = tps_list_n_s[[3]],
              aes(x=x, y=y, fill = factor(Temperature))) +
  scale_x_continuous(limits = c(20.95,51.6),
                     expand = c(0, 0),
                     breaks = c(25, 30, 35, 40, 45, 50)) +
  scale_y_continuous(breaks = c(-8, -6, -4, -2, 0),
                     expand = (mult = c(0, .05)))+
  stat_contour2(data = tps_list_n_s[[3]],
                aes(x=x, y=y, z = Temperature),
                colour = "grey10",
                breaks = MakeBreaks(binwidth = 1),
                size = 0.1) +
  scale_fill_manual(values = surfer_cols("temp"),
                    guide = guide_legend(reverse=T),
                    limits = as.character(seq(11, 33, 1))) +
  geom_rect(data = rectdf_nar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
            fill = "black") +
  geom_polygon(data = S_bottom_nar1,
               aes(x=x, y=y), fill = "grey90", colour = "grey20") +
  geom_point(data = filter(samp_locs, x >= 21),
             aes(x = x, y = - y),
             colour = "black",
             size = 0.5) +
  annotate("text",
           label = expression('bold(paste("Temperature (", degree,"C)"))'),
           # x = 33,
           # y = -7.6,
           x = 41,
           y = -7,
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
        #legend.position = c(0.65, 0.22),
        legend.position = c(0.65, 0.15),
        legend.key.size =  unit(8, "mm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                             label.position = "bottom"))


# # for zoomed at narrows
# # create list of plotGrobs
# pltzs <- lapply(list(salPlotZ_s, doPlotZ_s, chlorPlotZ_s, tempPlotZ_s), ggplotGrob)
# # rbind (i.e. 1 column) size arg matters!
# surfersZs <- rbind(pltzs[[1]], pltzs[[2]], pltzs[[3]], pltzs[[4]], size = "first")
# # pdf_nameZs <- paste0(path, "/plots/", "swan_middle_upper_", lubridate::ymd(samp_date), "_surfer.pdf")
# png_nameZs <- paste0(path, "/plots/", "swan_middle_upper_", lubridate::ymd(samp_date), "_surfer.png")
# 
# # add margin padding coarse but effective
# # surfersZ_pad <- gtable::gtable_add_padding(surfersZs, padding = unit(c(1,4,3,4), "cm"))
# 
# # ggsave(plot = grid.draw(surfersZs), filename = pdf_nameZs, width=28, height=18)
# # cat(paste0(pdf_nameZs,"\n"))
# png(file = png_nameZs, width = 1500, height = 960, res = 53, bg = "transparent") #, 
# grid.draw(surfersZs)
# dev.off()
# cat(paste0(png_nameZs,"\n"))

plts_all <- list(salPlot_s, doPlot_s, chlorPlot_s, tempPlot_s, salPlotZ_s, doPlotZ_s, chlorPlotZ_s, tempPlotZ_s)

plts_all

  }, error = function(e) {
    # if error occurs print an error message
    no_data_df <- data.frame(label = "data incomplete")
    errorplot <- ggplot2::ggplot(no_data_df, aes(x = 1, y = 1, label = label)) +
      geom_text(size = 10) +
      theme_void()

    plts_error <- list(errorplot,errorplot,errorplot,errorplot,errorplot,errorplot,errorplot,errorplot)
    plts_error

    # Return a value or NULL if needed

    # Continue with the next iteration
    #NULL
  })
  
}
