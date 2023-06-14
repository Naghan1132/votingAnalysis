library(ggplot2)
library(plotly)
library(gridExtra)

# ==== Évolution dissimilarité entre types de distribution ====

load("experiments_output_data/distribution_fixed/dissimilarity_matrix_beta_30_simus.RData")
matrix_beta <- dissimilarity_matrix
load("experiments_output_data/distribution_fixed/dissimilarity_matrix_unif_30_simus.RData")
matrix_unif <- dissimilarity_matrix
load("experiments_output_data/distribution_fixed/dissimilarity_matrix_norm_30_simus.RData")
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


# ==== Évolution dissimilarité avec n_candidats variant ====

load("experiments_output_data/n_candidates_fixed/dissimilarity_matrix_3_candidates_150_simus.RData")
matrix_3_candidates <- dissimilarity_matrix
load("experiments_output_data/n_candidates_fixed/dissimilarity_matrix_4_candidates_150_simus.RData")
matrix_4_candidates <- dissimilarity_matrix
load("experiments_output_data/n_candidates_fixed/dissimilarity_matrix_5_candidates_150_simus.RData")
matrix_5_candidates <- dissimilarity_matrix
load("experiments_output_data/n_candidates_fixed/dissimilarity_matrix_7_candidates_150_simus.RData")
matrix_7_candidates <- dissimilarity_matrix
load("experiments_output_data/n_candidates_fixed/dissimilarity_matrix_9_candidates_150_simus.RData")
matrix_9_candidates <- dissimilarity_matrix
load("experiments_output_data/n_candidates_fixed/dissimilarity_matrix_14_candidates_150_simus.RData")
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

# Diviser la fenêtre graphique en deux sous-graphiques côte à côte
par(mfrow = c(1, 2))

mds <- cmdscale(n_candidates_evolution_matrix, k = 2)
print(mds)
coord_x <- mds[, 1]
coord_y <- mds[, 2]
df <- data.frame(x = coord_x, y = coord_y, labels = rownames(n_candidates_evolution_matrix))
p <- ggplot(df, aes(x, y, label = labels)) + geom_point(color = "blue", size = 3) +
  geom_text(aes(label = labels), hjust = 0.5,vjust = -0.5) +
  geom_path(color = "red", alpha = 0.5, size = 0.5, linetype = "dashed")
p <- ggplotly(p)
print(p)


# ==== Évolution dissimilarité avec n_voters variant ====

load("experiments_output_data/n_voters_fixed/dissimilarity_matrix_9_voters_175_simus.RData")
matrix_9_voters <- dissimilarity_matrix
load("experiments_output_data/n_voters_fixed/dissimilarity_matrix_15_voters_175_simus.RData")
matrix_15_voters <- dissimilarity_matrix
load("experiments_output_data/n_voters_fixed/dissimilarity_matrix_21_voters_175_simus.RData")
matrix_21_voters <- dissimilarity_matrix
load("experiments_output_data/n_voters_fixed/dissimilarity_matrix_51_voters_175_simus.RData")
matrix_51_voters <- dissimilarity_matrix
load("experiments_output_data/n_voters_fixed/dissimilarity_matrix_101_voters_175_simus.RData")
matrix_101_voters <- dissimilarity_matrix
load("experiments_output_data/n_voters_fixed/dissimilarity_matrix_1001_voters_175_simus.RData")
matrix_1001_voters <- dissimilarity_matrix
load("experiments_output_data/n_voters_fixed/dissimilarity_matrix_10001_voters_175_simus.RData")
matrix_10001_voters <- dissimilarity_matrix

voters <- c("9_voters","15_voters","21_voters","51_voters","101_voters","1001_voters","10001_voters")
n_voters_evolution_matrix <- matrix(0,length(voters),length(voters))
colnames(n_voters_evolution_matrix) <- voters
rownames(n_voters_evolution_matrix) <- voters

for (i in 1:(length(voters) - 1)) {
  for (j in (i + 1):length(voters)) {
    # fonction distance
    similarity <- sqrt(sum((as.vector(get(paste0("matrix_", voters[i]))) - as.vector(get(paste0("matrix_", voters[j]))))^2))
    n_voters_evolution_matrix[i, j] <- similarity
    n_voters_evolution_matrix[j, i] <- similarity
  }
}

mds <- cmdscale(n_voters_evolution_matrix, k = 2)
coord_x <- mds[, 1]
coord_y <- mds[, 2]
df <- data.frame(x = coord_x, y = coord_y, labels = rownames(n_voters_evolution_matrix))
p <- ggplot(df, aes(x, y, label = labels)) + geom_point(color = "blue", size = 3) +
  geom_text(aes(label = labels), hjust = 0.5,vjust = -0.5) +
  geom_path(color = "red", alpha = 0.5, size = 0.5, linetype = "dashed")
p <- ggplotly(p)
print(p)
