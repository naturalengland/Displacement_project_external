library(sf)

process_pings_list_combined <- function(pings_data, site_list, inside = FALSE) {
  # Convert pings_data to sf
  pings_sf <- st_as_sf(pings_data, coords = c("SI_LONG", "SI_LATI")) %>% st_set_crs(4326)
  
  # Initialize an empty list to store individual site results
  result_list <- list()
  
  # Loop through each site in the list
  for (site_name in names(site_list)) {
    # Find the number of pings in the current site area
    site_pings <- st_over(pings_sf, site_list[[site_name]])
    
    # Assign the site-specific pings to the global environment
    assign(paste(site_name, "Pings", sep = "_"), site_pings, envir = .GlobalEnv)
    
    # Create a table of the number of non-NA pings at the site
    table_result <- table(site_pings, useNA = "no")
    
    # Add the site name to the table output
    result_list[[site_name]] <- table_result
  }
  
  # Combine the results into a single table
  combined_table <- do.call(cbind, result_list)
  
  # Summing up the counts for each site
  combined_table <- colSums(combined_table, na.rm = TRUE)
  
  # If inside is TRUE, perform the subtraction
  if (inside) {
    combined_table <- c(combined_table[1], 
                        combined_table[2] - combined_table[1], 
                        combined_table[3] - combined_table[2], 
                        combined_table[4] - combined_table[3])
  }
  
  # Rename the combined table
  names(combined_table) <- paste(names(site_list), "Pings", sep = "_")
  
  # Return both the combined result table and pings_sf
  return(list(combined_table = combined_table, pings_sf = pings_sf))
}



