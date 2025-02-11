install.packages("readxl")
install.packages('Hmisc')
library(readcsv)
data <- read.csv(file.choose(), header = TRUE, sep = ";")
View(data)

###########
######################Icc and CCT
# Calculate the Pearson correlation coefficient
cor_test_result <- cor.test(data$Icc1, data$CCT1, method = "pearson")
print(cor_test_result)
# Quadratic regression 
install.packages("ggplot2")
library(ggplot2)
###########
quadratic_model_simple <- lm(RoCoF3 ~ Icc3 + I(Icc3^2), data = data)
summary(quadratic_model_simple)


# Coefficients du modèle quadratique
intercept <- 1.95814
beta_1 <- -0.79736
beta_2 <- 0.08726

# Valeur de Icc1
Icc1_value <- 6.98

# Calcul de la CCT pour cette valeur de Icc1
CCT_pred <- intercept + beta_1 * Icc1_value + beta_2 * (Icc1_value^2)

# Affichage du résultat
cat("La valeur prédite de la CCT pour Icc1 =", Icc1_value, "est :", CCT_pred, "\n")
plot_1 <-ggplot(data = data, aes(x = Icc1, y = CCT1)) + 
  geom_point()+ geom_smooth(method = 'lm',formula = y ~ poly(x, 2), color = "Blue")+  
  theme_minimal() + labs (x='Short-circuit curent (kA)', y='CCT (ms)') 
plot_1

model <- lm(data$CCT_0 ~ Icc_0, data = data)
# Tracer les graphiques de diagnostic de régression
par(mfrow = c(2, 2)) # Disposer les graphiques en 2x2
plot(quadratic_model_simple)

# Ajuster les paramètres graphiques pour agrandir les textes et réduire les marges
par(mfrow = c(2, 2),  # Disposer les graphiques en 2x2
    mar = c(4, 4, 3, 1) + 0.1,  # Réduire les marges autour des graphiques (bas, gauche, haut, droite)
    cex = 1.2,  # Agrandir le texte à l'intérieur des graphiques
    cex.axis = 1.5,  # Agrandir les nombres sur les axes
    cex.lab = 1.5,  # Agrandir les étiquettes des axes
    cex.main = 2.5)  # Agrandir les titres des graphiques

# Tracer les graphiques de diagnostic pour le modèle
plot(quadratic_model)  # ou plot(linear_model) selon le modèle que vous utilisez

# Sélectionner les 3 variables d'intérêt
selected_data <- data[, c("Icc_12", "CCT_12", "RoCoF_12")]

# Calculer la matrice de corrélation
cor_matrix <- cor(selected_data)

# Afficher la matrice de corrélation
print(cor_matrix)

# Optionnel : Visualiser la matrice de corrélation avec un heatmap
# Vous pouvez utiliser ggplot2, pheatmap, ou corrplot comme montré précédemment

# Exemple avec pheatmap
install.packages("pheatmap")  # Installer pheatmap si nécessaire
library(pheatmap)
pheatmap(cor_matrix, 
         color = colorRampPalette(c("blue", "white", "red"))(50),
    
         display_numbers = TRUE)
