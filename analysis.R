# === Analysis ===
# ACM
# CAH
# MDS

# ====

# clustering entre les méthodes (en excluant les autres colonnes)
# ACM sur toute les variables ?
# mettre en facteur les resultats de méthodes ????

# Les candidats n'ont tous pas la même chance d'être élu sur chaque ligne, les petits candidats "1,2,3 etc.."
# sont plus souvent dans les simulations, et même vu que nous n'avons pas tous les mêmes options disponible
# pour les candidats dans tout le dataframe, ça fausse tout, il faudrait faire ligne par ligne l'analyse


# === Loading data ===
library("readxl")
df <- read_excel("experiments.xlsx")

# === Prepare data ===

# choose number of candidates
df <- df[df$nCandidates == 14,]

df$Simu <- NULL # on enlève la variable inutile
df$nVoters = factor(df$nVoters)
#df$nVoters = NULL
df$nCandidates = factor(df$nCandidates)
#df$nCandidates = NULL
df$typeSimu = factor(df$typeSimu)
#df$typeSimu = NULL
df$uninominal1T = factor(df$uninominal1T)
df$uninominal2T = factor(df$uninominal2T)
df$successif_elimination = factor(df$successif_elimination)
df$bucklin = factor(df$bucklin)
df$borda = factor(df$borda)
df$nanson = factor(df$nanson)
df$minimax = factor(df$minimax)
df$copeland = factor(df$copeland)
df$condorcet = factor(df$condorcet)
df$range_voting = factor(df$range_voting)
df$approval = factor(df$approval)
df$majority_jugement = factor(df$majority_jugement)
head(df)


# === Analysis ===
summary(df)
library(FactoMineR)

# === ACM ===

# echantillon <- df[sample(nrow(df), 100), ]
# res.mca <- MCA(echantillon)
# couleurs <- c("red", "blue", "green")[as.numeric(factor(df$typeSimu))]
# plot(res.mca,col.ind = couleurs, habillage = "typeSimu", title = "Nuage d'individus (Échantillon)")

res.mca <- MCA(df)
eig.val <- res.mca$eig # nuage individus
barplot(eig.val[, 2],
        names.arg = 1:nrow(eig.val),
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2],
      type = "b", pch = 19, col = "red")

plot(res.mca, autoLab = "yes") # => nuage individus + label groupe

# === Heatmap ===
library(ggplot2)

similarity_matrix <- table(df$uninominal1T, df$uninominal2T)
ggplot(data = as.data.frame(similarity_matrix)) +
  geom_tile(aes(Var1, Var2, fill = Freq), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "uninominal1T", y = "uninominal2T", title = "Graphique de proximité") +
  theme_minimal()

similarity_matrix <- table(df$majority_jugement, df$uninominal2T)
ggplot(data = as.data.frame(similarity_matrix)) +
  geom_tile(aes(Var1, Var2, fill = Freq), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "majority_jugement", y = "uninominal2T", title = "Graphique de proximité") +
  theme_minimal()

similarity_matrix <- table(df$nanson, df$minimax)
ggplot(data = as.data.frame(similarity_matrix)) +
  geom_tile(aes(Var1, Var2, fill = Freq), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "nanson", y = "minimax", title = "Graphique de proximité") +
  theme_minimal()

