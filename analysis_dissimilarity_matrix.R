library(stats)

load("dissimilarity_matrix.RData")
View(dissimilarity_matrix)

m_dist <- as.dist(dissimilarity_matrix)
print(m_dist)


# == CAH ==
cah <- hclust(m_dist)
# Affichage du dendrogramme
plot(cah)


# ==== MDS 2D ====
library(ggplot2)
library(plotly)

# Calcul de la MDS
mds <- cmdscale(dissimilarity_matrix, k = 2)
# Récupération des coordonnées x et y
coord_x <- mds[, 1]
coord_y <- mds[, 2]
# Affichage des points en 2D
# plot(coord_x, coord_y, type = "n", xlab = "Coordonnée X", ylab = "Coordonnée Y", main = "MDS")
# # Ajout des points avec des étiquettes
# points(coord_x, coord_y, pch = 16, col = "blue")
# text(coord_x, coord_y, labels = rownames(dissimilarity_matrix), pos = 4)
df <- data.frame(x = coord_x, y = coord_y, labels = rownames(dissimilarity_matrix))
p <- ggplot(df, aes(x, y, label = labels)) + geom_point(color = "blue", size = 3) +
  geom_text(aes(label = labels), hjust = 0.5,vjust = -0.5)
p <- ggplotly(p)
print(p)

# Convertir toutes les colonnes en type numérique
df <- data.frame(x = coord_x, y = coord_y)
# ==== Clustering avec K-means ====
kmeans_result <- kmeans(df, centers = 3)
# Obtention des attributions de cluster
cluster_labels <- kmeans_result$cluster
# Affichage des résultats
plot(df, col = cluster_labels, pch = 16)

#  ==== MDS 3D ====
library(rgl)
mds <- cmdscale(dissimilarity_matrix, k = 3)
coord_x <- mds[, 1]
coord_y <- mds[, 2]
coord_z <- mds[, 3]
plot3d(coord_x, coord_y, coord_z, type = "n", xlab = "Coordonnée X", ylab = "Coordonnée Y", zlab = "Coordonnée Z", main = "MDS")
points3d(coord_x, coord_y, coord_z, col = "blue", size = 2)
text3d(coord_x, coord_y, coord_z, text = rownames(dissimilarity_matrix), adj = c(1.5, 1.5))
rglwidget()
