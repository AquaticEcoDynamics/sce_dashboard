#s1 <- read.csv("C:\\Users\\AminaSaeed\\Desktop\\old_one\\inst\\extdata\\sensorslist.csv") ### pcik DO for the list
#s1 <- read.csv("inst/extdata/sensorInfo.csv")
s1 <- read.csv("www/sensorslist.csv")
#### remove unnecessary columns for catchment
S2<-s1[!grepl("Site Ref", s1$s_graph_value),]
S3<-S2[!grepl("Sample Type", S2$s_graph_value),]
S4<-S3[!grepl("Program Site Ref", S3$s_graph_value),]
S5<-S4[!grepl("Program Code", S4$s_graph_value),]
S6<-S5[!grepl("Collection Method", S5$s_graph_value),]
S7<-S6[!grepl("Depth measuring point", S6$s_graph_value),]
S8<-S7[!grepl("Depth _s", S7$s_graph_value),]
S9<-S8[!grepl("Depth", S8$s_graph_value),]
S10<-S9[!grepl("Depth To", S9$s_graph_value),]
S11<-S10[!grepl("Security Level", S10$s_graph_value),]
S12<-S11[!grepl("Null reading", S11$s_graph_value),]
S13<-S12[!grepl("Cloud cover", S12$s_graph_value),]
S14<-S13[!grepl("Flow status", S13$s_graph_value),]
S15<-S14[!grepl("Tide status", S14$s_graph_value),]
S15<-S14[!grepl("Secchi depth", S14$s_graph_value),]

sensorslist <-S15
usethis::use_data(sensorslist, overwrite = TRUE)


methydro <- read.csv("www/Met_Hydro_sites.csv")
sensorslist_hydromet <- methydro[,1:3]
usethis::use_data(sensorslist_hydromet, overwrite = TRUE)

sensorslist_est <- read.csv("www/sensorslist_est.csv")
usethis::use_data(sensorslist_est, overwrite = TRUE)

sensorslist_oxy <- read.csv("www/sensorslist_oxy.csv")
usethis::use_data(sensorslist_oxy, overwrite = TRUE)


