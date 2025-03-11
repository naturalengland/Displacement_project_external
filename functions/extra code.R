unique_vessels <- filtered_ve_ref%>%
  summarise(unique_types = round(sum(TIME/3), 3))  # Round to 2 decimal places

# Step 1: Identify rows containing "LE_EURO"
rows_to_sum <- grepl("LE_KG", colnames(location_five_vessels))

le_columns <- grep("LE_KG", names(location_five_vessels), value = TRUE)

# Find column names containing "LE_EURO"
columns_to_sum <- grep("LE_KG", names(df), value = TRUE)

# Sum all rows beneath the columns containing "LE_EURO"
sum_of_columns <- location_five_vessels%>%
  group_by(SI_MONTH)%>%
  summarise_at(vars(le_columns), funs(sum))

print(sum_of_columns)

location_five_vessels<-filtered_ve_ref %>%
  filter(Location == "Five")



five_vessels<-tacsatEflalo_ID %>%
  filter(Location == "Five")

view(results$`Value (£)`)


filtered_ve_ref <- tacsatEflalo%>%
  filter(VE_REF %in% vessels_in_location_five)

area<-area(tacsatEflalo)
grouped_vessels <- location_five_vessels %>%
  group_by(VE_REF)%>%
  summarize(total_value = sum(TIME))
mean(grouped_vessels$total_value)

kruskal.test(total_value ~ SI_MONTH, data = grouped_vessels)

View(results$`Time (hrs)`)


library(lubridate)
library(dplyr)

MESH_twenty<- st_intersection(MESH, twenty_site)
tacsatEflalo$SI_MONTH <- month(tacsatEflalo$SI_DATIM)
tacsatEflalo$SI_MONTH <- factor(tacsatEflalo$SI_MONTH,
                        levels = 1:12,
                        labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


# Convert date_column to Date class
tacsatEflalo$date_column <- mdy(tacsatEflalo$SI_DATE)

# Create a new column for month
tacsatEflalo <- tacsatEflalo %>% 
  mutate(month = month(date_column))

plot_individual_VE(tacsatEflalo, "Five")

result$plot
View(results$`Value (£)`)


grouped_vessels <- results$`Time (hrs)`%>%
  group_by(Location)%>%
  summarize(total_value = sd(valPerc))


grouped_vessels <- MESH_twenty%>%
  group_by(STACK_MESH_confidence_score)%>%
  summarize(total_value = sum(area))


sd(grouped_vessels$total_value)



A5.35
48.877847782
23.4408915028
Five

A5.44
0.120714219
1.4940523511
Five
tacsatEflalo$LE_WIDTH <-as.numeric(tacsatEflalo$LE_WIDTH)

tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME / 60) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)

grouped_vessels1 <- tacsatEflalo%>%
  group_by(Location, sens_Z10_5_D2, LE_GEAR)%>%
  summarize(total_value = sum(TIME/60))

View(result$Hab_final)


0
975147.4
Five

1
18039659.9
Five

2
5920135.6
Five



#sens_Z10_5_D2:penetration from dredges








Title<-"Sensitivity: penetration from dredging"

plot_sensitivity_map(Hab_twenty, sens_Z10_5_D2, Title, additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), additional_colors = list("darkblue","black", "blue", "yellow"), x_limits = c(-3.6,-3.2), y_limits = c(50.4,50.7))


#we can also do a bar graph like in the habitat type section above
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
result <- Stack_sens(Hab_Type,sens_Z10_5_D2,Title,five_site, ten_site, twenty_site, boundary, order_locations)

result$plot

Real<-tacsatEflalo
load("./processed_data/tacsatlocation.RData")#loading tacsateflalo based on location
#And plot fishing effort and ping number
tacsatEflalo<- tacsatEflalo[tacsatEflalo$LE_GEAR == 'DRB', ]
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")

result <- Stack_sens_over(tacsatEflalo,Hab_Type,sens_Z10_5_D2,Title, order_locations)
result$plot
result$plot2

createMap_sens(tacsatEflalo, sens_Z10_5_D2, Title, additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), additional_colors = list("darkblue","black", "blue", "yellow"), x_limits = c(-3.6,-3.2), y_limits = c(50.4,50.7))
