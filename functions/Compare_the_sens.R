Hab_Type<-st_read("./raw_data/clipped_NESSST_outputs.gpkg", layer = "BenHabSens_fishing_Filtered_inshore")
Hab_Type<- transform_and_make_valid(Hab_Type)
#Filter down to relative sensitivity columns
#Require z10
#Require sens
#Require_5_ or _6_ as we only want trawling and dredging

required_sens_columns <- grep("sens_Z10(_5_|_6_)", names(Hab_Type), value = TRUE)

filtered_sens <- Hab_Type[, required_sens_columns]
filtered_sens <- filtered_sens %>% st_drop_geometry() #drop geometry


are.cols.identical <- function(col1, col2) identical(filtered_sens[,col1], filtered_sens[,col2])

identical.mat <- outer(colnames(filtered_sens), colnames(filtered_sens),
                       FUN = Vectorize(are.cols.identical))

identical.mat


library(cluster)
distances <- as.dist(!identical.mat)
tree      <- hclust(distances)
cut       <- cutree(tree, h = 0.5)
cut

col_pairs<-split(colnames(filtered_sens), cut)


add_numbers <- function(names_list) {
  result <- vector(mode = "list", length = length(names_list))
  counter <- 1
  for (i in seq_along(names_list)) {
    if (length(names_list[[i]]) > 1) {
      new_names <- paste0(counter, "_", names_list[[i]])
      counter <- counter + 1
    } else {
      new_names <- paste0("unique_", names_list[[i]])
    }
    result[[i]] <- new_names
  }
  return(result)
}
modified_column_names <- add_numbers(col_pairs)
your_dataframe <- data.frame(do.call(rbind, modified_column_names ))
replace_column_headings <- function(column_names, dataframe) {
  new_column_names <- sapply(colnames(dataframe), function(x) {
    matching_name <- grep(paste0(substr(x, nchar(x) - 12, nchar(x))), column_names, value = TRUE)
    if (length(matching_name) > 0) {
      matching_name
    } else {
      x
    }
  })
  colnames(dataframe) <- new_column_names
  return(dataframe)
}


# Replace column headings in df
filtered_sens<- replace_column_headings(your_dataframe$X1, filtered_sens)
filtered_sens<- replace_column_headings(your_dataframe$X2, filtered_sens)

df_long <- pivot_longer(filtered_sens, cols = everything(), names_to = "Pressure", values_to = "Sensitivity")
df_long$Count<-1


ggplot(df_long, aes(x = Pressure, y = Count, fill = as.character(Sensitivity))) +
  geom_col(position = "fill")+
  scale_fill_manual(name = "Habitat sensitivity", values = c("0" = "#495057",
                                                             "1" = "#ee4040",
                                                             "2" = "#ffc037",
                                                             "3" = "#ebff91",
                                                             "4" = "#3c096c",
                                                             "5" = "#9d4edd",
                                                             "6" = "#b7eaf0",
                                                             "7" = "#40916c",
                                                             "8" = "#1b4332"
                                                             ), labels = c("Null values","High", "Medium", "Low",                                                  
                                                                                          "Insufficient sensitivity evidence",                    
                                                                                          "No sensitivity asessment carried out",                 
                                                                                          "Not sensitive", "No direct effects",                                    
                                                                                          "Activity-pressure combination not relevant to biotope"))+
  coord_flip() 
  
#need to fix the colours

