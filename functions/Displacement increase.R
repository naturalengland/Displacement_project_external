load("./processed_data/tacsatEflalo_ID.RData")
tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
vessel_list <- unique(tacsatEflalo$VE_REF)
Final_summary <- data.frame(Location = "Ten", Total_Time = 0)
for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  time_sum <- sum(subset_df$TIME)
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_time_sum <- sum(subset_total$TIME)
  summary <- subset_vessel%>%
    filter(Location != "Five") %>%
    group_by(Location) %>%
    summarize(Total_Time = sum(TIME))
  summary$Total_Time <- ((summary$Total_Time * time_sum) / (total_time_sum))
  Final_summary<- rbind(Final_summary,summary)}

summary <-  Final_summary%>%
  group_by(Location) %>%
  summarize(Total_Time = sum(Total_Time))
#####SAR surface
load("./processed_data/tacsatEflalo_ID.RData")
load("./processed_data/eflaloClean.RData")##calculate SAR
tacsatEflalo$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
vessel_list <- unique(tacsatEflalo$VE_REF)
Final_summary <- data.frame(Location = "Ten", TR_AREA = 0)
for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  swept_sum <- sum(subset_df$TR_AREA)
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_swept_sum <- sum(subset_total$TR_AREA)
  summary <- subset_vessel%>%
    filter(Location != "Five") %>%
    group_by(Location) %>%
    summarize(TR_AREA = sum(TR_AREA))
  summary$TR_AREA <- ((summary$TR_AREA * swept_sum) / (total_swept_sum))
  Final_summary<- rbind(Final_summary,summary)}

summary <-  Final_summary%>%
  group_by(Location) %>%
  summarize(TR_AREA = sum(TR_AREA))
five<-as.numeric((st_area(five_site))/1000000)
ten<-as.numeric((st_area(ten_site)-st_area(five_site))/1000000)
twenty<-as.numeric((st_area(twenty_site)-st_area(ten_site))/1000000)
Boundary<-as.numeric((st_area(boundary)-st_area(twenty_site))/1000000)
Boundary<-sum(Boundary)
summary <- summary%>%
  mutate(
    TR_AREA = case_when(
      Location == "Ten" ~ TR_AREA / ten,
      Location == "Twenty" ~ TR_AREA / twenty,
      Location == "Boundary" ~ TR_AREA / Boundary,
      TRUE ~ TR_AREA # Default case, if location doesn't match any of the above
    )
  )
###SUB SAR
load("./processed_data/tacsatEflalo_ID.RData")
load("./processed_data/eflaloClean.RData")##calculate SAR
tacsatEflalo$LE_WIDTH <- eflalo$LE_SUB[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
vessel_list <- unique(tacsatEflalo$VE_REF)
Final_summary <- data.frame(Location = "Ten", TR_AREA = 0)
for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  swept_sum <- sum(subset_df$TR_AREA)
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_swept_sum <- sum(subset_total$TR_AREA)
  summary <- subset_vessel%>%
    filter(Location != "Five") %>%
    group_by(Location) %>%
    summarize(TR_AREA = sum(TR_AREA))
  summary$TR_AREA <- ((summary$TR_AREA * swept_sum) / (total_swept_sum))
  Final_summary<- rbind(Final_summary,summary)}

summary <-  Final_summary%>%
  group_by(Location) %>%
  summarize(TR_AREA = sum(TR_AREA))
five<-as.numeric((st_area(five_site))/1000000)
ten<-as.numeric((st_area(ten_site)-st_area(five_site))/1000000)
twenty<-as.numeric((st_area(twenty_site)-st_area(ten_site))/1000000)
Boundary<-as.numeric((st_area(boundary)-st_area(twenty_site))/1000000)
Boundary<-sum(Boundary)
summary <- summary%>%
  mutate(
    TR_AREA = case_when(
      Location == "Ten" ~ TR_AREA / ten,
      Location == "Twenty" ~ TR_AREA / twenty,
      Location == "Boundary" ~ TR_AREA / Boundary,
      TRUE ~ TR_AREA # Default case, if location doesn't match any of the above
    )
  )


#Now for SSR
load("./processed_data/tacsatEflalo_ID.RData")
load("./processed_data/eflaloClean.RData")##calculate SAR
tacsatEflalo$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
Hab_Type<-st_read("./raw_data/clipped_NESSST_outputs.gpkg", layer = "BenHabSens_fishing_Filtered_inshore")
Hab_Type<- transform_and_make_valid(Hab_Type)
names(Hab_Type)[names(Hab_Type) == "sens_Z10_6_D6"] <- "Pressure"#rename your pressure column
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
Title<-"Sensitivity: abrasion from demersal trawling"
result <- Stack_sens_over(tacsatEflalo,Hab_Type,Pressure,Title, order_locations)#penetration
tacsatEflalo<-result$tacsatEflalo#associates each ping with a pressure  
tacsatEflalo$Pressure<-as.numeric(tacsatEflalo$Pressure)
tacsatEflalo$Score <- ifelse(tacsatEflalo$Pressure == 1, 3,
                       ifelse(tacsatEflalo$Pressure == 2, 2,
                       ifelse(tacsatEflalo$Pressure == 3, 1, 
                       ifelse(tacsatEflalo$Pressure == 4, 3, 
                       ifelse(tacsatEflalo$Pressure == 5, 3, 
                       ifelse(tacsatEflalo$Pressure == 6, 0,
                       ifelse(tacsatEflalo$Pressure == 7, 0, 
                       ifelse(tacsatEflalo$Pressure == 8, 0, 
                       ifelse(tacsatEflalo$Pressure == 0, 3, 3)))))))))#Next we find the swept area
tacsatEflalo$Total<-tacsatEflalo$Score*tacsatEflalo$TR_AREA
summary <- tacsatEflalo%>%
  group_by(Location) %>%
  summarize(Total = sum(Total))
summary <- st_drop_geometry(summary)
summary <- summary%>%
  mutate(
    Total = case_when(
      Location == "Five" ~ Total / five,
      Location == "Ten" ~ Total / ten,
      Location == "Twenty" ~ Total/ twenty,
      Location == "Boundary" ~ Total / Boundary,
      TRUE ~ Total # Default case, if location doesn't match any of the above
    )
  )

tacsatEflalo <- tacsatEflalo %>%
  mutate(
    High = if_else(Score == 3, TR_AREA, 0),
    Medium = if_else(Score == 2, TR_AREA, 0),
    Low = if_else(Score == 1, TR_AREA, 0),
    Not_relevant = if_else(Score == 0, TR_AREA, 0)
  )#separating based on scores

tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
vessel_list <- unique(tacsatEflalo$VE_REF)
Final_summary <- data.frame(Location = "Ten", TR_AREA = 0)
for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  swept_sum <- sum(subset_df$TR_AREA)
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_swept_sum <- sum(subset_total$TR_AREA)
  subset_total$Low <-((subset_total$Low * swept_sum) / (total_swept_sum))  # now for time increase
  subset_total$Medium <-  ((subset_total$Medium * swept_sum) / (total_swept_sum))  # now for time increase
  subset_total$High <-((subset_total$High * swept_sum) / (total_swept_sum))  # now for time increase
  subset_total$Not_relevant <- ((subset_total$Not_relevant * swept_sum) / (total_swept_sum))  # now for time increase
  
  subset_total$Low<-subset_total$Low*1
  subset_total$Medium <-subset_total$Medium*2
  subset_total$High <-subset_total$High*3
  subset_total$Not_relevant <-subset_total$Not_relevant*0
  subset_total$total<- subset_total$Low + subset_total$Medium + subset_total$High
  
  summary <- subset_total%>%
    filter(Location != "Five") %>%
    group_by(Location) %>%
    summarize(TR_AREA = sum(total))
  summary <- st_drop_geometry(summary)
  Final_summary<-rbind(Final_summary,summary)}

summary <-  Final_summary%>%
  group_by(Location) %>%
  summarize(TR_AREA = sum(TR_AREA))

summary <- summary%>%
  mutate(
    TR_AREA = case_when(
      Location == "Five" ~ TR_AREA / five,
      Location == "Ten" ~ TR_AREA / ten,
      Location == "Twenty" ~ TR_AREA/ twenty,
      Location == "Boundary" ~ TR_AREA / Boundary,
      TRUE ~ TR_AREA # Default case, if location doesn't match any of the above
    )
  )

#Now for subsurface SSR
load("./processed_data/tacsatEflalo_ID.RData")
load("./processed_data/eflaloClean.RData")##calculate SAR
tacsatEflalo$LE_WIDTH <- eflalo$LE_SUB[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
Hab_Type<-st_read("./raw_data/clipped_NESSST_outputs.gpkg", layer = "BenHabSens_fishing_Filtered_inshore")
Hab_Type<- transform_and_make_valid(Hab_Type)
names(Hab_Type)[names(Hab_Type) == "sens_Z10_6_D2"] <- "Pressure"#rename your pressure column
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
Title<-"Sensitivity: penetration from demersal trawling"
result <- Stack_sens_over(tacsatEflalo,Hab_Type,Pressure,Title, order_locations)#penetration
tacsatEflalo<-result$tacsatEflalo#associates each ping with a pressure  
tacsatEflalo$Pressure<-as.numeric(tacsatEflalo$Pressure)
tacsatEflalo$Score <- ifelse(tacsatEflalo$Pressure == 1, 3,
                             ifelse(tacsatEflalo$Pressure == 2, 2,
                                    ifelse(tacsatEflalo$Pressure == 3, 1, 
                                           ifelse(tacsatEflalo$Pressure == 4, 3, 
                                                  ifelse(tacsatEflalo$Pressure == 5, 3, 
                                                         ifelse(tacsatEflalo$Pressure == 6, 0,
                                                                ifelse(tacsatEflalo$Pressure == 7, 0, 
                                                                       ifelse(tacsatEflalo$Pressure == 8, 0, 
                                                                              ifelse(tacsatEflalo$Pressure == 0, 3, 3)))))))))#Next we find the swept area
tacsatEflalo$Total<-tacsatEflalo$Score*tacsatEflalo$TR_AREA
summary <- tacsatEflalo%>%
  group_by(Location) %>%
  summarize(Total = sum(Total))
summary <- st_drop_geometry(summary)
summary <- summary%>%
  mutate(
    Total = case_when(
      Location == "Five" ~ Total / five,
      Location == "Ten" ~ Total / ten,
      Location == "Twenty" ~ Total/ twenty,
      Location == "Boundary" ~ Total / Boundary,
      TRUE ~ Total # Default case, if location doesn't match any of the above
    )
  )


tacsatEflalo <- tacsatEflalo %>%
  mutate(
    High = if_else(Score == 3, TR_AREA, 0),
    Medium = if_else(Score == 2, TR_AREA, 0),
    Low = if_else(Score == 1, TR_AREA, 0),
    Not_relevant = if_else(Score == 0, TR_AREA, 0)
  )#separating based on scores

tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
vessel_list <- unique(tacsatEflalo$VE_REF)
Final_summary <- data.frame(Location = "Ten", TR_AREA = 0)
for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  swept_sum <- sum(subset_df$TR_AREA)
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_swept_sum <- sum(subset_total$TR_AREA)
  subset_total$Low <-((subset_total$Low * swept_sum) / (total_swept_sum))  # now for time increase
  subset_total$Medium <-  ((subset_total$Medium * swept_sum) / (total_swept_sum))  # now for time increase
  subset_total$High <-((subset_total$High * swept_sum) / (total_swept_sum))  # now for time increase
  subset_total$Not_relevant <- ((subset_total$Not_relevant * swept_sum) / (total_swept_sum))  # now for time increase
  
  subset_total$Low<-subset_total$Low*1
  subset_total$Medium <-subset_total$Medium*2
  subset_total$High <-subset_total$High*3
  subset_total$Not_relevant <-subset_total$Not_relevant*0
  subset_total$total<- subset_total$Low + subset_total$Medium + subset_total$High
  
  summary <- subset_total%>%
    filter(Location != "Five") %>%
    group_by(Location) %>%
    summarize(TR_AREA = sum(total))
  summary <- st_drop_geometry(summary)
  Final_summary<-rbind(Final_summary,summary)}

summary <-  Final_summary%>%
  group_by(Location) %>%
  summarize(TR_AREA = sum(TR_AREA))

summary <- summary%>%
  mutate(
    TR_AREA = case_when(
      Location == "Five" ~ TR_AREA / five,
      Location == "Ten" ~ TR_AREA / ten,
      Location == "Twenty" ~ TR_AREA/ twenty,
      Location == "Boundary" ~ TR_AREA / Boundary,
      TRUE ~ TR_AREA # Default case, if location doesn't match any of the above
    )
  )
