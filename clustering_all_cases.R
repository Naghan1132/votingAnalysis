# tableau 6*7 => avec tous les cas possibles , analyser par colonnes/lignes


# faire avec lois, beta, unif, norm  (donc 3 grands tableau de 6*7)

# ==== LOADING DATA ====

# ==== ====

# === pour grand tableau
plot1 <- ggplot(as.data.frame(mds), aes(x = V1, y = V2, label = rownames(n_voters_evolution_matrix))) +
  geom_point(color = "blue", size = 3) +
  geom_text(hjust = 0.5, vjust = -0.5) +
  geom_path(color = "red", alpha = 0.5, size = 0.5, linetype = "dashed") +
  labs(x = "Dimension 1", y = "Dimension 2", title = "MDS 2")

plot2 <- ggplot(as.data.frame(mds), aes(x = V1, y = V2, label = rownames(n_voters_evolution_matrix))) +
  geom_point(color = "blue", size = 3) +
  geom_text(hjust = 0.5, vjust = -0.5) +
  geom_path(color = "red", alpha = 0.5, size = 0.5, linetype = "dashed") +
  labs(x = "Dimension 1", y = "Dimension 2", title = "MDS 2")

# Afficher les graphiques côte à côte
grid.arrange(plot1, plot2, ncol = 2)

# Sélectionner une seule colonne
colonne1 <- arrangeGrob(grid, 1:3, 1, main = "Colonne 1")

# Sélectionner une seule ligne
ligne2 <- arrangeGrob(grid, 2, 1:3, main = "Ligne 2")

# Afficher la grille modifiée
grid.draw(colonne1)  # Afficher une seule colonne
grid.draw(ligne2)    # Afficher une seule ligne

