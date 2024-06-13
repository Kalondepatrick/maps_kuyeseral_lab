library(sf)
library(dplyr)
library(geosphere) 

facilities = read_sf("inputs/facilities/distancematrix/facilities_cleaned.shp")


unique_clusters <- unique(facilities$Cluster)

clustered_facilities <- split(facilities, facilities$Cluster)

for (cluster in names(clustered_facilities)) {
  df_name <- paste0("facilities_", gsub(" ", "_", cluster))
  assign(df_name, clustered_facilities[[cluster]])
}

# Generation of facilitity to facility distances (within each of the clusters)

distance_summaries <- list()

for (cluster in names(clustered_facilities)) {

  cluster_data <- clustered_facilities[[cluster]]
  
  coords <- st_coordinates(cluster_data)
  
  distance_matrix <- dist(coords)  
  
  distance_matrix <- as.matrix(distance_matrix)
  
  mean_distance_m <- mean(distance_matrix[distance_matrix > 0])
  min_distance_m <- min(distance_matrix[distance_matrix > 0])
  max_distance_m <- max(distance_matrix)
  
  mean_distance_km <- mean_distance_m / 1000
  min_distance_km <- min_distance_m / 1000
  max_distance_km <- max_distance_m / 1000
  
  distance_summaries[[cluster]] <- data.frame(
    Cluster = cluster,
    Mean_Distance_km = round(mean_distance_km, 2),
    Min_Distance_km = round(min_distance_km, 2),
    Max_Distance_km = round(max_distance_km, 2)
  )
}

distance_summary_df <- do.call(rbind, distance_summaries)

print(distance_summary_df)




# Export 

distance_summary_df <- do.call(rbind, distance_summaries)
output_txt_path <- "outputs/distance_summary.txt"
file_conn <- file(output_txt_path)
writeLines(capture.output(print(distance_summary_df, row.names = FALSE)), file_conn)
close(file_conn)

