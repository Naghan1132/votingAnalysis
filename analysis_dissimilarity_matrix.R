library(stats)

load("dissimilarity_matrix.RData")
#View(dissimilarity_matrix)

m_dist <- as.dist(dissimilarity_matrix)

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

# ==== Évolution dissimilarité entre types de distribution ====
load("experiments_output_data/dissimilarity_matrix_beta_30_simus.RData")
matrix_beta <- dissimilarity_matrix
load("experiments_output_data/dissimilarity_matrix_unif_30_simus.RData")
matrix_unif <- dissimilarity_matrix
load("experiments_output_data/dissimilarity_matrix_norm_30_simus.RData")
matrix_norm <- dissimilarity_matrix

distributions <- c("beta","unif","norm")
n_distributions <- length(distributions)
d_matrix <- matrix(0,n_distributions , n_distributions)
colnames(d_matrix) <- distributions
rownames(d_matrix) <- distributions

# Calculer la distance entre les distributions
for (i in 1:(n_distributions - 1)) {
  for (j in (i + 1):n_distributions) {
    # fonction distance
    similarity <- sqrt(sum((as.vector(get(paste0("matrix_", distributions[i]))) - as.vector(get(paste0("matrix_", distributions[j]))))^2))
    d_matrix[i, j] <- similarity
    d_matrix[j, i] <- similarity
  }
}

mds <- cmdscale(d_matrix, k = 2)
coord_x <- mds[, 1]
coord_y <- mds[, 2]
df <- data.frame(x = coord_x, y = coord_y, labels = rownames(d_matrix))
p <- ggplot(df, aes(x, y, label = labels)) + geom_point(color = "blue", size = 3) +
  geom_text(aes(label = labels), hjust = 0.5,vjust = -0.5)
p <- ggplotly(p)
print(p)


# ==== Évolution dissimilarité avec n_candidats ====

load("experiments_output_data/dissimilarity_matrix_3_candidates_150_simus.RData")
matrix_3_candidates <- dissimilarity_matrix
load("experiments_output_data/dissimilarity_matrix_4_candidates_150_simus.RData")
matrix_4_candidates <- dissimilarity_matrix
load("experiments_output_data/dissimilarity_matrix_5_candidates_150_simus.RData")
matrix_5_candidates <- dissimilarity_matrix
load("experiments_output_data/dissimilarity_matrix_5_candidates_150_simus.RData")
matrix_7_candidates <- dissimilarity_matrix
load("experiments_output_data/dissimilarity_matrix_5_candidates_150_simus.RData")
matrix_9_candidates <- dissimilarity_matrix
load("experiments_output_data/dissimilarity_matrix_14_candidates_150_simus.RData")
matrix_14_candidates <- dissimilarity_matrix

candidates <- c("3_candidates","4_candidates","5_candidates","7_candidates","9_candidates","14_candidates")
n_candidates_evolution_matrix <- matrix(0,length(candidates),length(candidates))
colnames(n_candidates_evolution_matrix) <- candidates
rownames(n_candidates_evolution_matrix) <- candidates

for (i in 1:(length(candidates) - 1)) {
  for (j in (i + 1):length(candidates)) {
    # fonction distance
    similarity <- sqrt(sum((as.vector(get(paste0("matrix_", candidates[i]))) - as.vector(get(paste0("matrix_", candidates[j]))))^2))
    n_candidates_evolution_matrix[i, j] <- similarity
    n_candidates_evolution_matrix[j, i] <- similarity
  }
}

mds <- cmdscale(n_candidates_evolution_matrix, k = 2)
coord_x <- mds[, 1]
coord_y <- mds[, 2]
df <- data.frame(x = coord_x, y = coord_y, labels = rownames(n_candidates_evolution_matrix))
p <- ggplot(df, aes(x, y, label = labels)) + geom_point(color = "blue", size = 3) +
  geom_text(aes(label = labels), hjust = 0.5,vjust = -0.5)
p <- ggplotly(p)
print(p)
