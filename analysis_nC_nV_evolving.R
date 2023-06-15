# tableau 6*7 => avec tous les cas possibles , analyser par colonnes/lignes
# faire avec lois, beta, unif, norm  (donc 3 grands tableau de 6*7)

library(ggplot2)
library(plotly)
library(gridExtra)

# ==== LOADING DATA FROM votingExperiments::all_cases() ====

num_candidates <- c(3, 4, 5, 7, 9, 14)
num_voters <- c(9, 15, 21, 51, 101, 1001, 10001)

matrix_list <- list()
for (num_c in num_candidates) {
  for (num_v in num_voters) {
    # Loading files
    file_path <- paste0("experiments_output_data/all_cases/beta/", num_v, "_voters_", num_c, "_candidates_1000_simus.RData")
    load(file_path)
    matrix_name <- paste0("matrix_", num_v, "_voters_", num_c, "_candidates")
    matrix_list[[matrix_name]] <- dissimilarity_matrix
  }
}
for (num_v in num_voters) {
  for (num_c in num_candidates) {
    # Construire le nom de la variable avec un nom dynamique
    var_name <- paste0("matrix_", num_v, "_voters_", num_c, "_candidates")
    # Assigner la matrice de dissimilarité correspondante à la variable
    assign(var_name, matrix_list[[var_name]])
  }
}

# ==== MDS ====

# Créer une liste pour stocker les résultats de MDS
mds_list <- list()
for (num_c in num_candidates) {
  for (num_v in num_voters) {
    matrix_name <- paste0("matrix_", num_v, "_voters_", num_c, "_candidates")
    dissimilarity_matrix <- matrix_list[[matrix_name]]
    mds_result <- cmdscale(dissimilarity_matrix, k = 2)
    mds_list[[matrix_name]] <- mds_result
  }
}

# ==== PLOTS ====

plot_list <- list()
for (num_v in num_voters) {
  for (num_c in num_candidates) {
    matrix_name <- paste0("matrix_", num_v, "_voters_", num_c, "_candidates")
    mds_name <- paste0("mds_", num_v, "_voters_", num_c, "_candidates")

    dissimilarity_matrix <- matrix_list[[matrix_name]]
    mds_result <- mds_list[[matrix_name]]

    # Créer le graphique MDS
    plot <- ggplot(as.data.frame(mds_result), aes(x = V1, y = V2, label = rownames(dissimilarity_matrix))) +
      geom_point(color = "blue", size = 3) +
      geom_text(hjust = 0.5, vjust = -0.5) +
      geom_path(color = "red", alpha = 0.5, size = 0.5, linetype = "dashed") +
      labs(x = "Dimension 1", y = "Dimension 2", title = paste0("MDS - ", num_v, " voters, ", num_c, " candidates"))

    plot_list[[paste0("plot_", num_v, "_voters_", num_c, "_candidates")]] <- plot
  }
}

# Convertir la liste de graphiques en une liste de grobs
grobs <- lapply(plot_list, ggplotGrob)

# # Parcourir les graphiques et les enregistrer en tant qu'images individuelles
# for (i in 1:length(grobs)) {
#   filename <- paste0("MDS_", num_voters[i], "_voters_", num_candidates[i], "_candidates.png")
#   ggsave(filename, plot = grobs[[i]], width = X, height = Y)  # Remplacez X et Y par la largeur et la hauteur souhaitées en pouces
# }

# Afficher les graphiques côte à côte
library(gridExtra)
grid.arrange(grobs = grobs, ncol = length(num_candidates), nrow = length(num_voters))
