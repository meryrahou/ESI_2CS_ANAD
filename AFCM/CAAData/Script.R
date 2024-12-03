# Install missing packages
install.packages("data.table")
install.packages("mltools")
install.packages("factoextra")  # For enhanced visualizations
install.packages("Factoshiny")

# Loading libraries
library(FactoMineR)  # For MCA and CA analysis
library(factoextra)  # For enhanced visualizations
library(mltools)
library(data.table)
library(Factoshiny)

library(ggplot2)  # For data visualization
library(dplyr)    # For data manipulation
library(ggplot2)

# Define the output directory
output_dir <- "/Users/mery/GitHub/ESI_2CS_ANAD/TP_AFCM2/Plots"

# 1. Import the data
data <- read.csv("/Users/mery/GitHub/ESI_2CS_ANAD/TP_AFCM2/Data2.csv")  
contingency_table <- read.csv("/Users/mery/GitHub/ESI_2CS_ANAD/AFC/data.csv")  
t <- read.csv("/Users/mery/GitHub/ESI_2CS_ANAD/AFC/d.csv")  

data <- as.data.table(data)
data$Rate.your.knowledge.in.the.field.of.the.workshop <- as.character(data$Rate.your.knowledge.in.the.field.of.the.workshop)

# University Distribution
ggplot(data, aes(x = University)) +
  geom_bar(fill = "skyblue", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Répartition des Universités des Participants", x = "Université", y = "Nombre de Participants")

# Level of Study Distribution
ggplot(data, aes(x = Level.of.your.study)) + 
  geom_bar(fill = "lightgreen", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Répartition des Niveaux d'Études des Participants", x = "Niveau d'Études", y = "Nombre de Participants")
# Rename columns if needed (example)
colnames(data)[colnames(data) == "Level of Study"] <- "Level_of_Study"


# Workshop Selection Distribution
ggplot(data, aes(x = Select.the.workshop.you.wish.to.attend)) +
  geom_bar(fill = "coral", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Répartition des Choix de Workshops", x = "Workshop Choisi", y = "Nombre de Participants")

# Source of Information Distribution
ggplot(data, aes(x = How.did.you.hear.of.CSE.Around.Algeria.)) +
  geom_bar(fill = "purple", color = "black") +
  labs(x = "Source d'Information", y = "Nombre de Participants")


colnames(data)

# Custom plot with prettier colors from RColorBrewer
ggplot(data, aes(x = Select.the.workshop.you.wish.to.attend, fill = as.factor(Rate.your.knowledge.in.the.field.of.the.workshop))) +
  geom_bar(color = "black", position = "stack") +
  scale_fill_brewer(palette = "Blues", labels = c("Low", "Medium", "High")) +  # Use a color palette from RColorBrewer
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Répartition des Participants par Niveau de Connaissance et Workshop", 
       x = "Workshop", 
       y = "Nombre de Participants", 
       fill = "Niveau de Connaissance") +
  theme_minimal()


ggplot(data, aes(x = Select.the.workshop.you.wish.to.attend, fill = as.factor(Rate.your.knowledge.in.the.field.of.the.workshop))) +
  geom_bar(position = "fill", color = "black") +  # "fill" makes the bars proportional
  scale_fill_brewer(palette = "Set3", labels = c("Low", "Medium", "High")) +
  labs(title = "Proportion des Participants par Workshop et Niveau de Connaissance", 
       x = "Workshop", 
       y = "Proportion de Participants", 
       fill = "Niveau de Connaissance") +
  theme_minimal()


ggplot(data, aes(x = as.factor(Rate.your.knowledge.in.the.field.of.the.workshop), fill = as.factor(Rate.your.knowledge.in.the.field.of.the.workshop))) +
  geom_bar(color = "black") +
  facet_wrap(~ Select.the.workshop.you.wish.to.attend, scales = "free_y") +
  scale_fill_brewer(palette = "Set2", labels = c("Low", "Medium", "High")) +
  labs(title = "Répartition des Niveaux de Connaissance par Workshop", 
       x = "Niveau de Connaissance", y = "Nombre de Participants") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Compute the count of each workshop
workshop_count <- table(data$Select.the.workshop.you.wish.to.attend)

# Create a pie chart
ggplot(data = as.data.frame(workshop_count), aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_viridis_d() +
  labs(title = "Répartition des Participants par Workshop") +
  theme_void()  # Removes axes and background




data_subset <- data[, -c(1, 2)]

head(data_subset)# 2. Study basic statistics of the data (frequencies and summaries)
summary(data)  # Summary statistics for each variable


fatoshiny <- Factoshiny(data_subset)
Factoshiny(fatoshiny)

res.MCA<-MCA(data_subset,graph=FALSE)
plot.MCA(res.MCA, choix='var')
plot.MCA(res.MCA,label =c('ind','var'))

summary(res.MCA)


# Scree plot visualization
library(factoextra)
fviz_screeplot(res.MCA, addlabels = TRUE, ylim = c(0, 20)) +
  labs(title = "Scree Plot of MCA Eigenvalues",
       x = "Dimensions", y = "Percentage of Variance Explained")


# Biplot des dimensions 1 et 2
library(factoextra)
fviz_mca_biplot(res.MCA, repel = TRUE,
                title = "Biplot : Dimension 1 vs Dimension 2",
                axes = c(1, 2))

fviz_screeplot(res.MCA, addlabels = TRUE, main = "Scree Plot")


# Plot the factor map of individuals
fviz_mca_ind(res.MCA, 
             col.ind = "blue",  # Color for individuals
             title = "Factor Map of Individuals")

# Plot the factor map of variables
fviz_mca_var(res.MCA, 
             col.var = "red",  # Color for variables
             title = "Factor Map of Variables")

# Correlation circle of variables
fviz_mca_var(res.MCA, 
             col.var = "red", 
             geom = "point", 
             axes = c(1, 2), 
             title = "Correlation Circle")


# Contributions of individuals to dimensions
fviz_contrib(res.MCA, choice = "ind", axes = 1)  # Contribution of individuals to Dimension 1
fviz_contrib(res.MCA, choice = "ind", axes = 2)  # Contribution of individuals to Dimension 2

# Contributions of variables to dimensions
fviz_contrib(res.MCA, choice = "var", axes = 1)  # Contribution of variables to Dimension 1
fviz_contrib(res.MCA, choice = "var", axes = 2)  # Contribution of variables to Dimension 2



# Récupérer les coordonnées des modalités
mod_coord <- res.MCA$var$coord

# Récupérer les contributions des modalités à chaque dimension
mod_contrib <- res.MCA$var$contrib

# Visualiser les 10 premières modalités les plus contributives à la première dimension
head(mod_contrib[,1], 10)

# Visualiser les coordonnées des modalités pour les 2 premières dimensions
head(mod_coord, 10)
# Visualisation des associations entre les modalités et les dimensions
fviz_mca_var(res.MCA, col.var = "black", title = "Association entre les Modalités")


summary(data_subset)

sapply(data_subset, nlevels)

data_subset[] <- lapply(data_subset, as.factor)



mca1 = MCA(data)


colSums(is.na(data_subset))










# 4. Transform the data table into a disjunctive table (dummy variables)
categorical_columns <- names(data)[sapply(data, is.character) | sapply(data, is.factor)]
categorical_columns_to_encode <- categorical_columns[-1]  # Exclude the first column

categorical_columns
categorical_columns_to_encode
# Ensure the columns to encode are factors
data[, (categorical_columns_to_encode) := lapply(.SD, as.factor), .SDcols = categorical_columns_to_encode]

newdata <- one_hot(data, cols = categorical_columns_to_encode)

# Convert character columns to factors, and retain only factor columns in newdata
newdata <- newdata[, lapply(.SD, function(x) if (is.character(x)) as.factor(x) else x)]
newdata <- newdata[, sapply(newdata, is.factor), with = FALSE]  # Retain only factor columns

# 5. Perform MCA (AFCM in French)
afcm <- MCA(newdata, graph = FALSE)  # Perform MCA analysis without initial plots


# 13. Create a contingency table of two selected questions
# Choose two relevant questions, say, Q1 and Q2
contingency_table <- table(data$Level.of.your.study, data$Select.the.workshop.you.wish.to.attend)

t <- table(data$University, data$Select.the.workshop.you.wish.to.attend)


print(contingency_table)
print(t)

contingency_table_matrix <- as.matrix(contingency_table)
contingency_table_df <- as.data.frame(contingency_table_matrix)

contingency_table_matrix
contingency_table
summary(contingency_table)


# Row sums (sums across different workshops)
row_totals <- margin.table(contingency_table, 1)

# Column sums (sums across different levels of study)
col_totals <- margin.table(contingency_table, 2)

# Print row and column totals
row_totals
col_totals


ress <- Factoshiny(contingency_table)
tt <- Factoshiny(t)



afc <- CA(contingency_table, graph = FALSE)
# Summary of the results
summary(ca_result)

# Plot the results
library(FactoExtra)
fviz_ca_biplot(afc, label = "all")
                                   
                                   
                                   
fatoshiny <- Factoshiny(contingency_table)
Factoshiny(fatoshiny)


# 14. Perform Correspondence Analysis (AFC) on the contingency table
afc <- CA(contingency_table, graph = FALSE)

# Visualize and interpret AFC results
fviz_ca_biplot(afc, repel = TRUE) + 
  ggtitle("CA Biplot for Selected Questions")
fviz_contrib(afc, choice = "row", axes = 1, top = 5) +
  ggtitle("Contribution of Rows to Dim 1 in CA")
fviz_contrib(afc, choice = "col", axes = 1, top = 5) +
  ggtitle("Contribution of Columns to Dim 1 in CA")

# Save plots to output directory
ggsave(file.path(output_dir, "Eigenvalues_Scree_Plot.png"), plot = last_plot())

