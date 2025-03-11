removePointsInHarbour <- function(data, harbour_data) {
  idx <- pointInHarbour(data$SI_LONG, data$SI_LATI, harbour_data, saveHarbourList = FALSE)
  
  points_in_harbour <- data[which(idx == 1), ]
  points_removed <- length(which(idx == 1))
  
  plot(st_geometry(europa), col = "darkgreen", xlim = c(-4, 10), ylim = c(48, 62))
  axis(1); axis(2)
  points(x = points_in_harbour$SI_LONG, y = points_in_harbour$SI_LATI, col = "red", pch = 19, cex = 0.5)
  
  cat(paste("Number of points in harbour removed:", points_removed, "\n"))
  
  return(data[which(idx == 0), ])
}


