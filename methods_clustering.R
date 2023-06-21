library(stats)

# ==== LOADING DATA FROM votingExperiments ====

num_candidates <- c(3, 4, 5, 7, 9, 14)
num_voters <- c(9, 15, 21, 51, 101, 1001)
distribution <- "unif" # OU beta, norm
matrix_list <- list()
cpt <- 0
for (num_c in num_candidates) {
  for (num_v in num_voters) {
    file_path <- paste0("experiments_output_data/nc_nv_evolving/",distribution,"/", num_v, "_voters_", num_c, "_candidates_1000_simus.RData")
    if(cpt == 0){
      global_matrix <- dissimilarity_matrix
    }else{
      # additionner toutes les matrices
      global_matrix <- global_matrix + dissimilarity_matrix
    }
    cpt <- cpt +1
  }
}

View(global_matrix)
m_dist <- as.dist(global_matrix)


# ==== CAH ====
cah <- hclust(m_dist)
plot(cah)

# ==== MDS 2D ====
library(ggplot2)
library(plotly)

# Calcul de la MDS
mds <- cmdscale(global_matrix, k = 2)
coord_x <- mds[, 1]
coord_y <- mds[, 2]
df <- data.frame(x = coord_x, y = coord_y, labels = rownames(global_matrix))
p <- ggplot(df, aes(x, y, label = labels)) + geom_point(color = "blue", size = 3) +
  geom_text(aes(label = labels), hjust = 0.5,vjust = -0.5) +
  labs(x = "Dimension 1", y = "Dimension 2", title = paste0("Global Proximities - ",distribution," - n_c (3 => 14) - n_v (9 => 1001) - 1000 simulations"))
p <- ggplotly(p)
print(p)

# ==== Clustering K-means ====
df <- data.frame(x = coord_x, y = coord_y)
kmeans_result <- kmeans(df, centers = 3)
# Obtention des attributions de cluster
cluster_labels <- kmeans_result$cluster
# Affichage des résultats
plot(df, col = cluster_labels, pch = 16)



