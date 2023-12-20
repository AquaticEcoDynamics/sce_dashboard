#' plotCatchSum
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.

#' @noRd
#' 

# s3Connect <- function(filename){
#   
#   library('aws.s3')
#   
#   # To enforce HTTPS, should be set to TRUE
#   Sys.setenv('USE_HTTPS' = TRUE)
#   
#   # Set details for bucket origin
#   Sys.setenv(
#     'AWS_DEFAULT_REGION' = '', 
#     'AWS_S3_ENDPOINT' = 'projects.pawsey.org.au', 
#     'AWS_ACCESS_KEY_ID' = '2f1a9d81bdf24a178b2bd18d530e959b', 
#     'AWS_SECRET_ACCESS_KEY' = 'e062073c1faf488cb4209ba8de2eb483'
#   )
#   bucket <- 'scevo-data'
#   fetchedData <- aws.s3::s3read_using(FUN = utils::read.csv,
#                                       check.names = FALSE,
#                                       encoding = "UTF-8",
#                                       # show_col_types = FALSE,
#                                       # lazy = FALSE,
#                                       # progress = FALSE,
#                                       object = filename,
#                                       bucket = bucket,
#                                       filename = basename(filename),
#                                       opts = list(
#                                         base_url = "projects.pawsey.org.au",
#                                         region = ""))
#   
#   return(fetchedData)
# }
#Rawdata <- read.csv(file = 'www/DBCA_data_export/DBCA_data_export_2023-07-19_1615.csv', check.names = FALSE)
#Rawdata <- awss3Connect(filename = 'arms/wiski.csv')
wiskiEnable <- as.logical(get_golem_config("enable", config = "wiski_connection"))

if(isTRUE(wiskiEnable))
{
  Rawdata <- awss3Connect(filename = 'arms/wiski.csv')
}else{
  Rawdata <- read.csv(file = 'www/wiski.csv', check.names = FALSE)
}

#Rawdata <- read.csv(file = 'C:/Users/00101765/AED Dropbox/Sherry Zhai/SCEVO/data/WQ_Jo/wiski.csv', check.names = FALSE)

dat<-Rawdata%>%
  select(1,2,3,4,5,6,7,8,11,12,13,14,21,24,27,30,33,36,39,47,48,50,51,53,54,56,57,59,60,63,66,68,69,71,72,77,78,86,87)


#1.3 rename columns to something easier to code with
# dat <-dat %>% rename(Time = Collect.Time, Sample.Date = Collect.Date, Month = Collect.Month, Year = Collect.Year,
#                      Project = Program.Code, Site = Program.Site.Ref, Sal.ppt =Salinity..ppt., Temp=Temperature..deg.C., 
#                      ODO.mgl=O2..DO.conc...mg.L.,ODO.sat=O2..DO..sat....., pH=pH..no.units.,Turbidity.NTU=Turbidity..NTU.,
#                      TON.mgl=N..sum.sol.ox...NOx.N.TON...ug.L.,NH3.mgl=NH3.N.NH4.N..sol...ug.L.,DON.mgl=N..sum.sol.org...DON...ug.L.,
#                      TKN.mgl=N..tot.kjel...TKN...ug.L.,NO2.mgl=NO2.N..sol...ug.L.,NO3.mgl=NO3.N..sol...ug.L.,TP.mgl=P..tot...TP.pTP...ug.L.,
#                      FRP.mgl=PO4.P..sol.react...SRP.FRP...ug.L.,DOC.mgl=C..sol.org...DOC.DOC.as.NPOC...ug.L.,TSS.mgl=Suspended.Solids..Total...TSS...mg.L.,TN.mgl=N..tot...TN.pTN...ug.L.,
#                      SPCcondus.cm = Cond...25.deg.C..uS.cm., COC = Primary.CoC.Number)
dat <-dat %>% rename(#Site.Ref = `ï»¿Site Ref`,
                     Site.Ref = `Site Ref`,
                     Time = `Collect Time`,
                     Sample.Date = `Collect Date`, 
                     Month = `Collect Month`, 
                     Year = `Collect Year`,
                     Project = `Program Code`, 
                     Site = `Program Site Ref`, 
                     Sal.ppt = `Salinity (ppt)`, 
                     Temp= `Temperature (deg C)`, 
                     ODO.mgl=`O2-{DO conc} (mg/L)`,
                     ODO.sat=`O2-{DO %sat} (%)`, 
                     pH = `pH (no units)`,
                     Turbidity.NTU= `Turbidity (NTU)`,
                     TON.mgl= `N (sum sol ox) {NOx-N TON} (ug/L)`,
                     NH3.mgl= `NH3-N/NH4-N (sol) (ug/L)`,
                     DON.mgl= `N (sum sol org) {DON} (ug/L)`,
                     TKN.mgl= `N (tot kjel) {TKN} (ug/L)`,
                     NO2.mgl= `NO2-N (sol) (ug/L)`,
                     NO3.mgl= `NO3-N (sol) (ug/L)`,
                     TP.mgl = `P (tot) {TP pTP} (ug/L)`,
                     FRP.mgl= `PO4-P (sol react) {SRP FRP} (ug/L)`,
                     DOC.mgl= `C (sol org) {DOC DOC as NPOC} (ug/L)`,
                     TSS.mgl= `Suspended Solids (Total) {TSS} (mg/L)`,
                     TN.mgl = `N (tot) {TN pTN} (ug/L)`,
                     SPCcondus.cm = `Cond @ 25 deg C (uS/cm)`, 
                     COC = `Primary CoC Number`)

#1.4 Convert analyte units from ug/L to mg/L.                  
dat$TN.mgl<-dat$TN.mgl*0.001
dat$TON.mgl<-dat$TON.mgl*0.001
dat$NH3.mgl<-dat$NH3.mgl*0.001
dat$DON.mgl<-dat$DON.mgl*0.001
dat$TKN.mgl<-dat$TKN.mgl*0.001
dat$TP.mgl<-dat$TP.mgl*0.001
dat$FRP.mgl<-dat$FRP.mgl*0.001
dat$DOC.mgl<-dat$DOC.mgl*0.001

#1.5 combine Sign columns with analyte columns to account for '<' symbols in data and halve them later on
# dat<-dat%>%
#   unite(TN.mgl, c("TN.Sign","TN.mgl"))%>%
#   unite(TON.mgl, c("NOx.N.Sign","TON.mgl"))%>%
#   unite(NH3.mgl, c("NH3.4.N.Sign","NH3.mgl"))%>%
#   unite(DON.mgl, c("DON.Sign","DON.mgl"))%>%
#   unite(TKN.mgl, c("TKN.Sign","TKN.mgl"))%>%
#   unite(TP.mgl, c("TP.Sign","TP.mgl"))%>%
#   unite(FRP.mgl, c("FRP.Sign","FRP.mgl"))%>%
#   unite(TSS.mgl, c("TSS.Sign","TSS.mgl"))%>%
#   unite(DOC.mgl, c("DOC.Sign","DOC.mgl"))

dat<-dat%>%
  unite(TN.mgl, c("TN Sign","TN.mgl"))%>%
  unite(TON.mgl, c("NOx-N Sign","TON.mgl"))%>%
  unite(NH3.mgl, c("NH3/4-N Sign","NH3.mgl"))%>%
  unite(DON.mgl, c("DON Sign","DON.mgl"))%>%
  unite(TKN.mgl, c("TKN Sign","TKN.mgl"))%>%
  unite(TP.mgl, c("TP Sign","TP.mgl"))%>%
  unite(FRP.mgl, c("FRP Sign","FRP.mgl"))%>%
  unite(TSS.mgl, c("TSS Sign","TSS.mgl"))%>%
  unite(DOC.mgl, c("DOC Sign","DOC.mgl"))

#1.6 remove "_" between '<' symbol and analyte value 
dat$TN.mgl<-gsub("_","",as.character(dat$TN.mgl))
dat$TON.mgl<-gsub("_","",as.character(dat$TON.mgl))
dat$NH3.mgl<-gsub("_","",as.character(dat$NH3.mgl))
dat$DON.mgl<-gsub("_","",as.character(dat$DON.mgl))
dat$TKN.mgl<-gsub("_","",as.character(dat$TKN.mgl))
dat$TP.mgl<-gsub("_","",as.character(dat$TP.mgl))
dat$FRP.mgl<-gsub("_","",as.character(dat$FRP.mgl))
dat$TSS.mgl<-gsub("_","",as.character(dat$TSS.mgl))
dat$DOC.mgl<-gsub("_","",as.character(dat$DOC.mgl))


#1.7 halving less than values (standard procedure) so they can be converted into numerics
dat$TN.mgl[dat$TN.mgl == "<0.025"] <- "0.0125"
dat$TSS.mgl[dat$TSS.mgl == "<1"] <- "0.5"
dat$FRP.mgl[dat$FRP.mgl == "<0.005"] <- "0.0025"
dat$TON.mgl[dat$TON.mgl == "<0.01"] <- "0.005"
dat$NH3.mgl[dat$NH3.mgl == "<0.01"] <- "0.005"
dat$TP.mgl[dat$TP.mgl == "<0.005"] <- "0.0025"
dat$TN.mgl[dat$TN.mgl == "<0.025"] <- "0.5"
dat$DON.mgl[dat$DON.mgl == "<0.025"] <- "0.0125"
dat$TKN.mgl[dat$TKN.mgl == "<0.025"] <- "0.0125"
dat$DOC.mgl[dat$DOC.mgl == "<1"] <- "0.5"


#1.8 remove "NA"s from analyte columns
dat[dat=="NA"]<-""
# str(dat)

#1.9 removes records with no sample numbers (essentially sites that weren't sampled due to them not flowing etc.) from 2100 to 1841
#dat<-dat[!dat$Sample.Number=="",];
#dat<-dat[!dat$`Sample Number`=="",];
#head(dat)
sapply(dat, class)


plotCatchSum <- function(plotDataSite, plotDataYear, plotDataVar){

  # plotDataYear <- 2023
  # plotDataVar <- 'Sal.ppt'
  # plotDataSite <- "AIRSMD"

 
  
  #1.11 filter out reporting year SWANCATCH & SCWQIP Sonde and laboratory Data
  Reporting.Period.Start <- as.numeric(plotDataYear)
  histyear <- (Reporting.Period.Start-5):(Reporting.Period.Start-1)
  
  # histdat<-dat%>%
  #   filter(Project %in% c("SG-C-SWANCATCH", "SG-C-SCWQIP")) %>%
  #   filter(Year %in% histyear)%>%
  #   filter(Data.Category %in% c("Instrument log", "Laboratory results"))
  histdat<-dat%>%
    filter(Project %in% c("SG-C-SWANCATCH","SG-C-SCCATCH", "SG-C-SCWQIP")) %>%
    filter(Year %in% histyear)%>%
    filter(`Data Category` %in% c("Instrument log", "Laboratory results"))
  
  # yrdat<-dat%>%
  #   filter(Project %in% c("SG-C-SWANCATCH", "SG-C-SCWQIP")) %>%
  #   filter(Year %in% Reporting.Period.Start)%>%
  #   filter(Data.Category %in% c("Instrument log", "Laboratory results"))
  yrdat<-dat%>%
    filter(Project %in% c("SG-C-SWANCATCH", "SG-C-SCCATCH", "SG-C-SCWQIP")) %>%
    filter(Year %in% Reporting.Period.Start)%>%
    filter(`Data Category` %in% c("Instrument log", "Laboratory results"))

  # 1.1x convert sonde data values to characters in prep for summary ( they are currently numerics) - we will convert back to numerics later on
  #sapply(dat2, class)
  #cols.num <- c("Sal.ppt", "Temp", "pH", "SPCcondus.cm", "ODO.sat", "ODO.mgl","NO2.mgl", "NO3.mgl")
  #dat2[cols.num] <- sapply(dat2[cols.num],as.character)
  #sapply(dat2, class)
  
  
  #1.12 Contcatenate all data by sample number (joins sonde and lab data by sample number into single row)
  # histdat<-histdat %>% group_by( Sample.Date,Site.Ref, Year, Month,  COC,  Project, Site, `Depth measuring point`)%>%  #`Sample Number`,
  #   summarise(DOC.mgl = max(DOC.mgl, na.rm = T),
  #             SPCcondus.cm = max(SPCcondus.cm, na.rm = T),
  #             DON.mgl = max(DON.mgl, na.rm = T),
  #             TN.mgl = max(TN.mgl, na.rm = T),
  #             TON.mgl = max(TON.mgl, na.rm = T),
  #             NH3.mgl = max(NH3.mgl, na.rm = T),
  #             ODO.sat = max(ODO.sat, na.rm = T),
  #             ODO.mgl = max(ODO.mgl, na.rm = T),
  #             TP.mgl = max(TP.mgl, na.rm = T),
  #             pH = max(pH, na.rm = T),
  #             FRP.mgl = max(FRP.mgl, na.rm = T),
  #             Sal.ppt = max(Sal.ppt, na.rm = T),
  #             Temp = max(Temp, na.rm = T),
  #             TSS.mgl = max(TSS.mgl, na.rm = T),
  #             TKN.mgl = max(TKN.mgl, na.rm = T))%>%
  #   as.data.frame()
  # 
  # yrdat<-yrdat %>% group_by( Sample.Date,Site.Ref, Year, Month,  COC,  Project, Site, `Depth measuring point`)%>%  #`Sample Number`,
  #   summarise(DOC.mgl = max(DOC.mgl, na.rm = T),
  #             SPCcondus.cm = max(SPCcondus.cm, na.rm = T),
  #             DON.mgl = max(DON.mgl, na.rm = T),
  #             TN.mgl = max(TN.mgl, na.rm = T),
  #             TON.mgl = max(TON.mgl, na.rm = T),
  #             NH3.mgl = max(NH3.mgl, na.rm = T),
  #             ODO.sat = max(ODO.sat, na.rm = T),
  #             ODO.mgl = max(ODO.mgl, na.rm = T),
  #             TP.mgl = max(TP.mgl, na.rm = T),
  #             pH = max(pH, na.rm = T),
  #             FRP.mgl = max(FRP.mgl, na.rm = T),
  #             Sal.ppt = max(Sal.ppt, na.rm = T),
  #             Temp = max(Temp, na.rm = T),
  #             TSS.mgl = max(TSS.mgl, na.rm = T),
  #             TKN.mgl = max(TKN.mgl, na.rm = T))%>%
  #   as.data.frame()

  histdat<-histdat %>% group_by( Sample.Date,Site.Ref, Year, Month,  COC,  Project, Site, `Depth measuring point`)%>%  #`Sample Number`,
    summarise(DOC.mgl = ifelse(all(is.na(DOC.mgl)), NA, max(DOC.mgl, na.rm = TRUE)),
              SPCcondus.cm = ifelse(all(is.na(SPCcondus.cm)), NA, max(SPCcondus.cm, na.rm = TRUE)),
              DON.mgl = ifelse(all(is.na(DON.mgl)), NA, max(DON.mgl, na.rm = TRUE)),
              TN.mgl  = ifelse(all(is.na(TN.mgl)), NA, max(TN.mgl, na.rm = TRUE)),
              TON.mgl = ifelse(all(is.na(TON.mgl)), NA, max(TON.mgl, na.rm = TRUE)),
              NH3.mgl = ifelse(all(is.na(NH3.mgl)), NA, max(NH3.mgl, na.rm = TRUE)),
              ODO.sat = ifelse(all(is.na(ODO.sat)), NA, max(ODO.sat, na.rm = TRUE)),
              ODO.mgl = ifelse(all(is.na(ODO.mgl)), NA, max(ODO.mgl, na.rm = TRUE)),
              TP.mgl  = ifelse(all(is.na(TP.mgl)), NA, max(TP.mgl, na.rm = TRUE)),
              pH      = ifelse(all(is.na(pH)), NA, max(pH, na.rm = TRUE)),
              FRP.mgl = ifelse(all(is.na(FRP.mgl)), NA, max(FRP.mgl, na.rm = TRUE)),
              Sal.ppt = ifelse(all(is.na(Sal.ppt)), NA, max(Sal.ppt, na.rm = TRUE)),
              Temp    = ifelse(all(is.na(Temp)), NA, max(Temp, na.rm = TRUE)),
              TSS.mgl = ifelse(all(is.na(TSS.mgl)), NA, max(TSS.mgl, na.rm = TRUE)),
              TKN.mgl = ifelse(all(is.na(TKN.mgl)), NA, max(TKN.mgl, na.rm = TRUE)))%>%
    as.data.frame()
  
  yrdat<-yrdat %>% group_by( Sample.Date,Site.Ref, Year, Month,  COC,  Project, Site, `Depth measuring point`)%>%  #`Sample Number`,
    summarise(DOC.mgl = ifelse(all(is.na(DOC.mgl)), NA, max(DOC.mgl, na.rm = TRUE)),
              SPCcondus.cm = ifelse(all(is.na(SPCcondus.cm)), NA, max(SPCcondus.cm, na.rm = TRUE)),
              DON.mgl = ifelse(all(is.na(DON.mgl)), NA, max(DON.mgl, na.rm = TRUE)),
              TN.mgl  = ifelse(all(is.na(TN.mgl)), NA, max(TN.mgl, na.rm = TRUE)),
              TON.mgl = ifelse(all(is.na(TON.mgl)), NA, max(TON.mgl, na.rm = TRUE)),
              NH3.mgl = ifelse(all(is.na(NH3.mgl)), NA, max(NH3.mgl, na.rm = TRUE)),
              ODO.sat = ifelse(all(is.na(ODO.sat)), NA, max(ODO.sat, na.rm = TRUE)),
              ODO.mgl = ifelse(all(is.na(ODO.mgl)), NA, max(ODO.mgl, na.rm = TRUE)),
              TP.mgl  = ifelse(all(is.na(TP.mgl)), NA, max(TP.mgl, na.rm = TRUE)),
              pH      = ifelse(all(is.na(pH)), NA, max(pH, na.rm = TRUE)),
              FRP.mgl = ifelse(all(is.na(FRP.mgl)), NA, max(FRP.mgl, na.rm = TRUE)),
              Sal.ppt = ifelse(all(is.na(Sal.ppt)), NA, max(Sal.ppt, na.rm = TRUE)),
              Temp    = ifelse(all(is.na(Temp)), NA, max(Temp, na.rm = TRUE)),
              TSS.mgl = ifelse(all(is.na(TSS.mgl)), NA, max(TSS.mgl, na.rm = TRUE)),
              TKN.mgl = ifelse(all(is.na(TKN.mgl)), NA, max(TKN.mgl, na.rm = TRUE)))%>%
    as.data.frame()
  
  histdat$Month<-month.abb[histdat$Month]
  yrdat$Month<-month.abb[yrdat$Month]
  # write.csv(histdat,"histdat.csv")
  # write.csv(yrdat,"2020dat.csv")
  histdat[9:23]<- sapply(histdat[9:23],as.numeric)
  yrdat[9:23]<- sapply(yrdat[9:23],as.numeric)
  

  
  ############################################################################# loops to plot figures for all sites ###############################################################
  
  
  ###################create custom boxplot function to specify unique quartiles later#########################
  stat_boxplot_custom <- function(mapping = NULL, data = NULL,
                                  geom = "boxplot", position = "dodge",
                                  ...,
                                  #qs = c(.05, .25, 0.5, 0.75, 0.95),
                                  qs=c(0.1, 0.25, 0.5, 0.75, .9),
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
                               #compute_group = function(data, scales, width = NULL, na.rm = FALSE, qs = c(.05, .25, 0.5, 0.75, 0.95)) {
                                 compute_group = function(data, scales, width = NULL, na.rm = FALSE, qs = c(0.1, 0.25, 0.5, 0.75, .9)) {
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
                                 #? stats[c(1, 5)] <- range(c(stats[2:4], data$y[!outliers]), na.rm = TRUE)
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
  
  
  
  
  ##########################################begin plotting loop for TN################################
  #Read in Data 
  # histdat<-read.csv("histdat.csv")
  # yrdat<-read.csv("2020dat.csv")
  #Sites <-unique(histdat$Site)
  
  i <- plotDataSite
  
  # for (i in Sites){
    # df.i <- as.data.frame(histdat)%>% 
    #   filter(Site == i)
    #filter(Site == i)
    
    #filter for each site
    # class(yrdat$Month)
    # median.TN<-yrdat %>%
    #   group_by(Month,Site)%>%
    #   filter(Site== i)%>%
    #   summarise(TN.mgl = median(TN.mgl, na.rm = TRUE))
    #class(yrdat$Month)
    median.plotDataVar<-yrdat %>%
      group_by(Month,Site)%>%
      filter(Site== i)%>%
      summarise(plotDataVar = median(!!sym(plotDataVar), na.rm = TRUE))
    
    histfiltered<-histdat%>%
      filter(Site== i)
   
    
    # g1<-histdat%>%
    #   filter(Site== i)%>%
      g1 <- ggplot(histfiltered, aes(x = as.factor(Month), y =!!sym(plotDataVar)))+
      stat_boxplot_custom(qs=c(0.1, 0.25, 0.5, 0.75, .9), colour="skyblue2",alpha=0.5, width = .5,fill="skyblue2") +
      stat_boxplot_custom(geom="errorbar",linewidth = 0.5, width = 0.25, colour="skyblue2")+
      theme(panel.background = element_blank(),panel.border = element_rect(colour = "black",fill = NA,size = .1))+
      ggtitle(i)+
      xlab("Month")+
      ylab(paste0("Median ",plotDataVar))+
      theme(axis.title.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            title = element_text(size=14), 
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12))+
      #scale_y_continuous(expand = expansion(c(0.02, 0.1)))+
      #scale_y_continuous(expand = expansion(add = 0, mult = 0))+
      scale_x_discrete(limits = month.abb)+
      #stat_n_text(color = "skyblue2", y.pos = -0.2, size=3)+   #this function is not supported in shinyproxy/docker
      geom_point(data = median.plotDataVar, aes(x = as.factor(Month), y = plotDataVar,group="median"))+
      geom_line(data = median.plotDataVar,linetype= "dashed", aes(x = as.factor(Month), y = plotDataVar,group="median"))
    #geom_hline(yintercept = 1.2, linetype= "dashed", col="Red" )+
    #geom_hline(yintercept = 0, linetype= "solid", col="black" )+
    #scale_fill_manual(name="2015-2015", values="skyblue2")
    
    # 
    # path = paste0(dir,"/plots")
    # print(i)
    # ggsave(path=path,width=3.7, height=3.7, units ="in", dpi =500,g1,file=paste0(i,".TN.png",sep=" "),device = png)
  #}
      
      
      #g1
      
      
      #Testing code below to replace stat_n_text()
      # Function to get plot limits for setting location of n value labels
      get_plot_limits <- function(plot) {
        gb = ggplot_build(plot)
        xmin = gb$layout$panel_params[[1]]$x.range[1]
        xmax = gb$layout$panel_params[[1]]$x.range[2]
        ymin = gb$layout$panel_params[[1]]$y.range[1]
        ymax = gb$layout$panel_params[[1]]$y.range[2]
        list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
      }
      # Get axis ranges for plot
      axis.ranges <- get_plot_limits(g1)
      # Set n value label location
      n.label.y.value = as.numeric(axis.ranges['ymin']) * 0.8
      # Function to annotate n value on plots
      give.n <- function(x){
        return(data.frame(y = n.label.y.value, label = paste0('n=', length(x))))}
      
      plot.to.print <- g1 + stat_summary(
                                        fun.data = give.n, 
                                        geom = "text", 
                                        fun.y = n.label.y.value, 
                                        position = position_dodge(width = 0.75),
                                        colour = "skyblue2",
                                        size = 3, alpha = 1)
      
      plot.to.print
      
  
  
  
}
