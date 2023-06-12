library(stats)

load("experiments_output_data/dissimilarity_matrix_all_10_simus.RData")

m_dist <- as.dist(dissimilarity_matrix)

# ==== CAH ====
cah <- hclust(m_dist)
plot(cah)

# ==== MDS 2D ====
library(ggplot2)
library(plotly)

# Calcul de la MDS
mds <- cmdscale(dissimilarity_matrix, k = 2)
coord_x <- mds[, 1]
coord_y <- mds[, 2]
df <- data.frame(x = coord_x, y = coord_y, labels = rownames(dissimilarity_matrix))
p <- ggplot(df, aes(x, y, label = labels)) + geom_point(color = "blue", size = 3) +
  geom_text(aes(label = labels), hjust = 0.5,vjust = -0.5)
p <- ggplotly(p)
print(p)

# ==== Clustering K-means ====
df <- data.frame(x = coord_x, y = coord_y)
kmeans_result <- kmeans(df, centers = 3)
# Obtention des attributions de cluster
cluster_labels <- kmeans_result$cluster
# Affichage des résultats
plot(df, col = cluster_labels, pch = 16)



