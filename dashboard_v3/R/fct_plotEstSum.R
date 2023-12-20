#' plotEstSum
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
#' 
library(rio)
library(dplyr)
library(ggplot2)
library(tidyr)
library(svglite)
library(tidyverse)
library(ggprism)
library(ggh4x)
library(gtable)
library(patchwork)
library('aws.s3')

# wd <- "C:/Users/00101765/AED Dropbox/Sherry Zhai/SCEVO/data/Nutrients plotting/Estuary code"
# setwd(wd)

# Import data downloaded via DBCA template (contains both catchment and estuary data)
#dfimport <- read.csv(file = 'www/DBCA_data_export/DBCA_data_export_2023-07-19_1615.csv', check.names = FALSE)
#dfimport <- awss3Connect(filename = 'arms/wiski.csv')

#dfimport <- read.csv(file = 'C:/Users/00101765/AED Dropbox/Sherry Zhai/SCEVO/data/WQ_Jo/wiski.csv', check.names = FALSE)

wiskiEnable <- as.logical(get_golem_config("enable", config = "wiski_connection"))

if(isTRUE(wiskiEnable))
{
  dfimport <- awss3Connect(filename = 'arms/wiski.csv')
}else{
  dfimport <- read.csv(file = 'www/wiski.csv', check.names = FALSE)
}

df <- dfimport


# Import ANZECC guidelines 2000
ANZECC <- import(file = 'www/Estuary_plotting/ANZECC guidelines 2000_long.xlsx')

# Import EMZ data
EMZ <- import(file = 'www/Estuary_plotting/EMZs.xlsx')

plotEstSum <- function(plotDataRegion, plotDataYear, plotDataVar){

  # plotDataYear <- 2022
  # plotDataVar <- "TN"
  # plotDataRegion <- "Swan"
  
  # Filtering variables
  Reporting.Period.Start <- as.numeric(plotDataYear)
  Reporting.Waterbody <- 'Estuary'
  
  # plotdir <- paste0(wd,"/Plot output/",Reporting.Period.Start)
  # if (!file.exists(plotdir)){
  #   dir.create(file.path(plotdir))
  # }
  
  # Calculate data range for plotting based on start of reporting year variable
  Relevant.Years <- (Reporting.Period.Start-5):(Reporting.Period.Start)
  
  # Create filtering variables and then re-order df to make them visible next to other categorical data
  
  # Financial year
  df$`Financial Year` <- ifelse(df$`Collect Month` <= 6, paste0(df$`Collect Year`-1, '-', df$`Collect Year`), 
                                paste0(df$`Collect Year`, '-', df$`Collect Year`+1))
  # Financial year start
  df$`Financial Year Start` <- ifelse(df$`Collect Month` <= 6, df$`Collect Year`-1, df$`Collect Year`)
  
  # Estuary or catchment
  df$`Waterbody` <- ifelse(substr(df$`Program Code`, 4, 4) == 'E', 'Estuary',
                           ifelse(substr(df$`Program Code`, 4, 4) == 'C', 'Catchment', 'Marine'))
  
  # Create named month column and apply levels to them for use in plotting
  df$`Month Name` <- factor(month.abb[(df$`Collect Month`)], levels = month.abb[c(7:12, 1:6)])
  
  # Trim for relevant data based on filtering variables
  df <- df %>% filter(`Financial Year Start` %in% Relevant.Years & Waterbody %in% Reporting.Waterbody)
  
  # Apply column to differentiate between reporting year and historical data year
  df$`Data Relevance` <- if_else(df$`Financial Year Start` == Reporting.Period.Start, 'Reporting year', 'Historical 5 year data')
  
  # Apply EMZs to relevant sites
  df <- merge(df, EMZ, by = 'Program Site Ref', all.x = T)
  
  # Reorder so filtering variables are visible in first page of columns
  df <- df[, c(1:6, 131:138, 7:130)]
  
  # Remove sites that are not plotted
  df <- df[!is.na(df$EMZ),]
  
  # Remove unused profile data
  # df <- filter(df, grepl('\\(', `Sample Type`))
  
  # Create matrix of < values to use for halving non-detects
  nds <- as.data.frame(which(sapply(df, grepl, pattern = "<"), arr.ind = T))
  nds$col <- nds$col + 1 # add 1 to column index to point to value column rather than sign column
  
  # For loop to run through row and column indexes of non-detects matrix and halve values 
  for(i in 1:nrow(nds)){ 
    r <- nds[i, 1]
    c <- nds[i, 2]
    #df[r, c] <- df[r, c] / 2
    df[r, c] <- as.numeric(df[r, c]) / 2
  }
  
  # Create matrix of deactivated values to remove from the dataset
  deactivated <- as.data.frame(which(sapply(df, grepl, pattern = "Deactivated"), arr.ind = T))
  deactivated <- deactivated %>% filter(!col %in% c(114, 117)) # Ignore sonde Chl-a columns because they are always deactivated
  deactivated$col <- deactivated$col - 1 # subtract 1 from column index to point to value column rather than status column
  
  # For loop to run through row and column indexes of deactivated matrix and remove values
  for(i in 1:nrow(deactivated)){ 
    r <- deactivated[i, 1]
    c <- deactivated[i, 2]
    df[r, c] <- NA
  }
  
  # Boxplot analytes
  Columns.To.Boxplot <- c("N (tot) {TN pTN} (ug/L)", # Total N
                          "NH3-N/NH4-N (sol) (ug/L)", # Dissolved Ammonia
                          "N (sum sol ox) {NOx-N TON} (ug/L)", # Total oxidised nitrogen
                          "N (sum sol org) {DON} (ug/L)", # Dissolved organic nitrogen
                          "P (tot) {TP pTP} (ug/L)", # Total P
                          "PO4-P (sol react) {SRP FRP} (ug/L)", # Filterable reactive P
                          "SiO2-Si (sol react) (ug/L)", # Reactive silica
                          "C (sol org) {DOC DOC as NPOC} (ug/L)", # Dissolved organic carbon
                          "Suspended Solids (Total) {TSS} (mg/L)", # Total suspended solids
                          "Alkalinity (tot) (CaCO3) (ug/L)", # Alkalinity
                          "Chlorophyll a (by vol) (mg/L)", # Chl a
                          "O2-{DO conc} (mg/L)", # Dissolved oxygen
                          "Cond @ 25 deg C (uS/cm)", # Sp conducdivity
                          "Temperature (deg C)", # Temp
                          "pH (no units)", # pH
                          "Secchi depth (m)" # Secchi
  )
  
  Columns.To.Convert <- c("N (tot) {TN pTN} (ug/L)", # Total N
                          "NH3-N/NH4-N (sol) (ug/L)", # Dissolved Ammonia
                          "N (sum sol ox) {NOx-N TON} (ug/L)", # Total oxidised nitrogen
                          "N (sum sol org) {DON} (ug/L)", # Dissolved organic nitrogen
                          "P (tot) {TP pTP} (ug/L)", # Total P
                          "PO4-P (sol react) {SRP FRP} (ug/L)", # Filterable reactive P
                          "SiO2-Si (sol react) (ug/L)", # Reactive silica
                          "C (sol org) {DOC DOC as NPOC} (ug/L)", # Dissolved organic carbon
                          "Alkalinity (tot) (CaCO3) (ug/L)", # Alkalinity
                          "Cond @ 25 deg C (uS/cm)" # Sp conducdivity
  )
  
  
  # Make sure all plotting variables are numeric
  df <- df %>% mutate_at(Columns.To.Boxplot, as.numeric)
  
  # Convert ug/L to mg/L
  df[,Columns.To.Convert] <- df[,Columns.To.Convert]/1000
  
  # Rename relevant columns to something easier to work with
  df <- rename(df, 
               TN = `N (tot) {TN pTN} (ug/L)`, # Total N
               NH4 = `NH3-N/NH4-N (sol) (ug/L)`, # Dissolved Ammonia
               TON = `N (sum sol ox) {NOx-N TON} (ug/L)`, # Total oxidised nitrogen
               DON = `N (sum sol org) {DON} (ug/L)`, # Dissolved organic nitrogen
               TP = `P (tot) {TP pTP} (ug/L)`, # Total P
               FRP = `PO4-P (sol react) {SRP FRP} (ug/L)`, # Filterable reactive P
               Si = `SiO2-Si (sol react) (ug/L)`, # Reactive silica
               DOC = `C (sol org) {DOC DOC as NPOC} (ug/L)`, # Dissolved organic carbon
               TSS = `Suspended Solids (Total) {TSS} (mg/L)`, # Total suspended solids
               Alk = `Alkalinity (tot) (CaCO3) (ug/L)`, # Alkalinity
               Chla = `Chlorophyll a (by vol) (mg/L)`, # Chl a
               DO = `O2-{DO conc} (mg/L)`, # Dissolved oxygen
               SpCond = `Cond @ 25 deg C (uS/cm)`, # Sp conducdivity
               TempC = `Temperature (deg C)`, # Temp
               pH = `pH (no units)`, # pH
               Secchi = `Secchi depth (m)` # Secchi)
  )
  
  # Custom boxplot function that allows for custom quartiles to be defined ----
  
  stat_boxplot_custom <- function(mapping = NULL, data = NULL,
                                  geom = "boxplot", position = "dodge",
                                  ...,
                                  qs = c(.05, .25, 0.5, 0.75, 0.95),
                                  na.rm = FALSE,
                                  show.legend = NA,
                                  inherit.aes = TRUE) {
    layer(
      data = data,
      mapping = mapping,
      stat = StatBoxplotCustom,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        qs = qs,
        ...
      )
    )
  }
  
  StatBoxplotCustom <- ggproto("StatBoxplotCustom", Stat,
                               required_aes = c("x", "y"),
                               non_missing_aes = "weight",
                               
                               setup_params = function(data, params) {
                                 params$width <- ggplot2:::"%||%"(params$width, (resolution(data$x) * 0.75))
                                 
                                 if (is.double(data$x) && !ggplot2:::has_groups(data) && any(data$x != data$x[1L])) {
                                   warning(
                                     "Continuous x aesthetic -- did you forget aes(group=...)?",
                                     call. = FALSE)
                                 }
                                 
                                 params
                               },
                               
                               compute_group = function(data, scales, width = NULL, na.rm = FALSE, qs = c(.05, .25, 0.5, 0.75, 0.95)) {
                                 
                                 if (!is.null(data$weight)) {
                                   mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
                                   stats <- as.numeric(stats::coef(mod))
                                 } else {
                                   stats <- as.numeric(stats::quantile(data$y, qs))
                                 }
                                 names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
                                 iqr <- diff(stats[c(2, 4)])
                                 
                                 outliers <- (data$y < stats[1]) | (data$y > stats[5])
                                 #if (any(outliers)) {
                                 #  stats[c(1, 5)] <- range(c(stats[2:4], data$y[!outliers]), na.rm = TRUE)
                                 #}
                                 
                                 if (length(unique(data$x)) > 1)
                                   width <- diff(range(data$x)) * 0.9
                                 
                                 df <- as.data.frame(as.list(stats))
                                 df$outliers <- list(data$y[outliers])
                                 
                                 if (is.null(data$weight)) {
                                   n <- sum(!is.na(data$y))
                                 } else {
                                   # Sum up weights for non-NA positions of y and weight
                                   n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
                                 }
                                 
                                 df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
                                 df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)
                                 
                                 df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
                                 df$width <- width
                                 df$relvarwidth <- sqrt(n)
                                 df
                               }
  )
  
  
  
  # Plotting using custom boxplot function ----
  
  # Set factors of sample type
  df$`Sample Type` <- factor(df$`Sample Type`, levels = c("Standard sample (Surface)", "Standard sample (Bottom)", "Standard sample"))
  
  # Set factors of EMZ 
  df$EMZ <- factor(df$EMZ, levels = c('Lower Swan Canning Estuary', 'Middle Swan Estuary', 'Upper Swan Estuary', 'Swan River', 'Canning Estuary', 'Lower Canning River'), ordered = T)
  
  # Specify plot labels for each analyte
  Plot.Labels <- c("TN" = "Median TN (mg/L)", # Total N
                   "NH4" = expression('Median NH'[3]*'-N (mg/L)'), # Dissolved Ammonia
                   "TON" = expression('Median NO'[x]*' (mg/L)'), # Total oxidised nitrogen
                   "DON" = 'Median DOrgN (mg/L)', # Dissolved organic nitrogen
                   "TP" = 'Median TP (mg/L)', # Total P
                   "FRP" = 'Median FRP (mg/L)', # Filterable reactive P
                   "Si" = 'Median Si (mg/L)', # Reactive silica
                   "DOC" = 'Median DOC (mg/L)', # Dissolved organic carbon
                   "TSS" = 'Median TSS (mg/L)', # Total suspended solids
                   "Alk" = 'Median Alk (mg/L)', # Alkalinity
                   "Chla" = 'Median Chl-a (mg/L)', # Chl a
                   "DO" = 'Median DO (mg/L)', # Dissolved oxygen
                   "SpCond" = 'Median Sp. Cond. (mS/cm)', # Sp conducdivity
                   "TempC" = 'Median Temp (°C)', # Temp
                   "pH" = 'Median pH', # pH
                   "Secchi"  = 'Median Secchi depth (m)' # Secchi       
  )
  
  # Function to get plot limits for setting location of n value labels
  get_plot_limits <- function(plot) {
    gb = ggplot_build(plot)
    xmin = gb$layout$panel_params[[1]]$x.range[1]
    xmax = gb$layout$panel_params[[1]]$x.range[2]
    ymin = gb$layout$panel_params[[1]]$y.range[1]
    ymax = gb$layout$panel_params[[1]]$y.range[2]
    list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  }
  
  # Analyte list to plot through
  analytes <- c("TN", # Total N
                "NH4", # Dissolved Ammonia
                "TON", # Total oxidised nitrogen
                "DON", # Dissolved organic nitrogen
                "TP", # Total P
                "FRP", # Filterable reactive P
                "Si", # Reactive silica
                "DOC", # Dissolved organic carbon
                "TSS", # Total suspended solids
                "Alk", # Alkalinity
                "Chla", # Chl a
                "DO", # Dissolved oxygen
                "SpCond", # Sp conducdivity
                "TempC", # Temp
                "pH", # pH
                "Secchi") # Secchi
  
  # System to plot - i.e. Swan or Canning
  system <- c('Swan', 'Canning')
  
  # Set box plot limits
  quartiles <- c(0.1, 0.25, 0.5, 0.75, .9)
  
  # for (a in analytes){
  #   for (b in system){
  #     
  #     
  #     # Code to exclude TSS and Alk if processing a year that wasn't sampling them in the Canning
  #     if ((Reporting.Period.Start < 2023 & a %in% c('TSS', 'Alk') & b == 'Canning') == T) {next}
  
  a <- plotDataVar
  b <- plotDataRegion
      
      # Filter data for plotting
      analyte_name <- a
      dffiltered <- if (b == 'Swan' & a %in% c("TN", "NH4", "TON","DON","TP","FRP","Si","DOC","Alk", 'TSS', "Chla") == T){ 
        df %>% filter(System %in% b, !EMZ %in% 'Swan River', `Sample Type` %in% c('Standard sample (Surface)', 'Standard sample (Bottom)') | `Data Category` %in% 'Field obs / rdgs')
      } else {df %>% filter(System %in% b, `Sample Type` %in% c('Standard sample (Surface)', 'Standard sample (Bottom)') | `Data Category` %in% 'Field obs / rdgs')}
      
      # Read secchi readings as 'surface' sample type
      dffiltered[dffiltered$`Data Category` == 'Field obs / rdgs', 17] <- 'Standard sample (Surface)'
      
      # Manually label facets
      facet_labels <- c('Standard sample (Surface)' = 'Surface','Standard sample (Bottom)'= 'Bottom')
      
      # Create facet object based on how many factors the sample type has
      facet_obj <- if (length(unique(dffiltered[!is.na(dffiltered %>% select(`Sample Type`, analyte_name))[,2], 17])) >= 2) {
        facet_grid(factor(EMZ, levels = c('Lower Swan Canning Estuary', 'Middle Swan Estuary', 'Upper Swan Estuary', 'Swan River', 'Canning Estuary', 'Lower Canning River'))~`Sample Type`, 
                   labeller = labeller(`Sample Type` = as_labeller(facet_labels)))
      } else {facet_grid(factor(EMZ, levels = c('Lower Swan Canning Estuary', 'Middle Swan Estuary', 'Upper Swan Estuary', 'Swan River', 'Canning Estuary', 'Lower Canning River'))~.)
      }
      
      fill_colours <- if (length(unique(dffiltered[!is.na(dffiltered %>% select(`Sample Type`, analyte_name))[,2], 17])) >= 2) {
        scale_fill_manual(values = c('slategray3', '#ED915C'))
      } else {scale_fill_manual(values = 'slategray3')
      }
      
      scale_colours <- if (length(unique(dffiltered[!is.na(dffiltered %>% select(`Sample Type`, analyte_name))[,2], 17])) >= 2) {
        scale_colour_manual(values = c('slategray3', '#ED915C'))
      } else {scale_colour_manual(values = 'slategray3')
      }
      
      
      # Plot object
      p <- ggplot2::ggplot(data = dffiltered, aes(x = `Month Name`, y = get(analyte_name), fill = `Sample Type`, colour = `Sample Type`)) +
        stat_boxplot_custom(data = dffiltered %>% filter(`Financial Year Start` %in% Relevant.Years[! Relevant.Years %in% Reporting.Period.Start]), 
                            geom = "errorbar", 
                            width = 0.25, 
                            qs = quartiles) +
        stat_boxplot_custom(data = dffiltered %>% filter(`Financial Year Start` %in% Relevant.Years[! Relevant.Years %in% Reporting.Period.Start]), 
                            alpha = 0.3, 
                            outlier.alpha = 1, 
                            outlier.size = .3, 
                            qs = quartiles) +
        facet_obj +
        fill_colours +
        scale_colours +
        theme(legend.position = 'none', 
              panel.background = element_blank(), 
              panel.border = element_rect(linetype = 'solid', fill= NA),
              axis.title = element_text(size = 14),  # for axis titles
              axis.text = element_text(size = 12),   # for axis labels
              strip.text = element_text(size = 14))+  # for facet grid labels
        ylab(analyte_name)
      
      # Get axis ranges for plot
      axis.ranges <- get_plot_limits(p)
      plot.parameters <- ggplot_build(p)
      y.axis.lower.limit <- plot.parameters[["layout"]][["panel_scales_y"]][[1]][["range"]][["range"]][1]
      
      # Set n value label location
      n.label.y.value = as.numeric(axis.ranges['ymin']) * 0.8
      
      # Function to annotate n value on plots
      give.n <- function(x){
        return(data.frame(y = n.label.y.value, label = paste0('n=', length(x))))}
      
      # Format ANZECC data to work with facet grid h line
      ANZECC_longer <- ANZECC #%>% pivot_longer(cols=colnames(ANZECC)[-1],    #pivot_longer() does not work with shinyproxy/docker, so the wide format has been manually converted to long
                              #                 names_to='Analytes',
                              #                 values_to='Values')
      
      # ANZECC line values (singe line or double for pH)
      if (analyte_name == 'pH') {
        ANZECC_line_upper <- geom_hline(data = ANZECC_longer[ANZECC_longer$Analytes == 'pH upper limit' & ANZECC_longer$EMZ %in% unique(plot.parameters[["layout"]][["layout"]][["factor(...)"]]),], 
                                        aes(yintercept = Values), 
                                        colour = 'red',
                                        linetype = 'dashed',
                                        size = .5)
        
        ANZECC_line_lower <- geom_hline(data = ANZECC_longer[ANZECC_longer$Analytes == 'pH lower limit' & ANZECC_longer$EMZ %in% unique(plot.parameters[["layout"]][["layout"]][["factor(...)"]]),], 
                                        aes(yintercept = Values), 
                                        colour = 'red',
                                        linetype = 'dashed',
                                        size = .5)
        
      } else {
        ANZECC_line_upper <- geom_hline(data = ANZECC_longer[ANZECC_longer$Analytes == plot.parameters$plot$labels$y & ANZECC_longer$EMZ %in% unique(plot.parameters[["layout"]][["layout"]][["factor(...)"]]),], 
                                        aes(yintercept = Values), 
                                        colour = 'red',
                                        linetype = 'dashed',
                                        size = .5)
        
        ANZECC_line_lower <- NULL
        
      }
      
      
      # Plot with additional information
      plot.to.print <- p + stat_summary(data = dffiltered %>% filter(`Financial Year Start` %in% Relevant.Years[! Relevant.Years %in% Reporting.Period.Start]), 
                                        aes(colour = `Sample Type`), 
                                        fun.data = give.n, 
                                        geom = "text", 
                                        fun.y = n.label.y.value, 
                                        position = position_dodge(width = 0.75),
                                        size = 3, alpha = 1) +
        ANZECC_line_upper + ANZECC_line_lower  +
        xlab('Month') +
        ylab(Plot.Labels[p[["labels"]][["y"]]]) + 
        #theme(axis.text.x = element_text(size=8)) + 
        stat_summary(data = dffiltered %>% filter(`Financial Year Start` %in% Reporting.Period.Start),
                     aes(group = 1),
                     fun.y = "median",
                     geom = 'line',
                     linetype = 'dashed',
                     colour = 'black',
                     size = .5) +
        stat_summary(data = dffiltered %>% filter(`Financial Year Start` %in% Reporting.Period.Start),
                     fun.y = "median",
                     col = "black",
                     size = .3,
                     shape = 20,
                     fill = "black",) #+ 
        #scale_y_continuous(guide = guide_prism_minor())  #this does not work in docker
      
      file.year.label <- paste0(Reporting.Period.Start, '-', Reporting.Period.Start+1)
      
      plot.to.print
      
      # ggsave(filename = paste0(file.year.label, ' ', a, ' ', b, ' plot.svg'), 
      #        #path = '.\\Plot output', 
      #        path = plotdir, 
      #        device = 'svg',
      #        width = 600, 
      #        height = 700,
      #        unit = 'px',
      #        plot = plot.to.print,
      #        dpi = 80)
      
      
}
#}
  
#}

