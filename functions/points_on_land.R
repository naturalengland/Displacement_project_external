
# Define the function
removePointsOnLand <- function(data, boundary) {
  idx <- pointOnLand(data, boundary, st_crs = 4326)  # Identify points on land
  pol <- data[which(idx == 1), ]  # Extract points on land
  
  # Plot points on land
  plot(st_geometry(boundary), col = "darkgreen", xlim = c(-4, 10), ylim = c(48, 62),
       xlab = "Longitude", ylab = "Latitude", main = "Points on land")
  axis(1); axis(2)
  points(x = pol$SI_LONG, y = pol$SI_LATI, col = "red", pch = 19, cex = 0.5)
  
  # Create an sf object for points on land
  outside <- st_as_sf(pol, coords = c("SI_LONG", "SI_LATI"))
  
  # Remove points on land from the original dataset
  removed_points <- data[which(idx == 1), ]
  data <- data[which(idx == 0), ]
  
  # Calculate the number of removed points
  num_removed <- nrow(removed_points)
  cat("Number of points removed:", num_removed, "\n")
  
  # Return the updated dataset
  return(data)
}


