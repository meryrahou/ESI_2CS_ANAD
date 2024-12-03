# Install missing packages
install.packages("data.table")
install.packages("mltools")
install.packages("factoextra")  # For enhanced visualizations

# Loading libraries
library(FactoMineR)  # For MCA and CA analysis
library(factoextra)  # For enhanced visualizations
library(mltools)
library(data.table)

# Define the output directory
output_dir <- "/Users/mery/GitHub/ESI_2CS_ANAD/TP_AFCM/Plots"



# 1. Import the data
data <- read.csv("/Users/mery/GitHub/ESI_2CS_ANAD/TP_AFCM/Data1.csv")  # Replace with your actual file path
data <- as.data.table(data)
class(data)  # This should return "data.table" and "data.frame"

# Plot histograms for numeric variables (knowledge scale, for example)
# Open a PNG device
png(filename = file_path)
hist(data$On.a.scale.of.1.5..how.much.do.you.know.about.data.science..analysis..engineering..AI..and.or.machine.learning.)
# Close the PNG device
dev.off()
cat("Histograms saved to:", output_dir)


# 3. Visualize the frequency of categories for categorical variables
# Frequency tables for each relevant categorical variable
table(data$City)
table(data$How.would.you.describe.your.current.situation.)
table(data$Current.school.)
table(data$Academic.Year)
table(data$On.what.device.s..will.you.use.DataCamp.)
table(data$Are.you.currently.receiving.any.other.forms.of.financial.assistance.or.scholarships.)
table(data$On.a.scale.of.1.5..how.much.do.you.know.about.data.science..analysis..engineering..AI..and.or.machine.learning.)
table(data$How.much.time.will.you.dedicate.to.use.DataCamp.regularly.)
table(data$After.using.DataCamp.for.6.12.months..are.you.willing.and.able.to.fill.out.and.submit.this.survey.about.your.experience.)

# 4. Transform the data table into a disjunctive table (dummy variables)
categorical_columns <- names(data)[sapply(data, is.character) | sapply(data, is.factor)]
categorical_columns_to_encode <- categorical_columns[-1]  # Exclude the first column
categorical_columns_to_encode

# Ensure the columns to encode are factors
data[, (categorical_columns_to_encode) := lapply(.SD, as.factor), .SDcols = categorical_columns_to_encode]

# Convert selected columns to factors directly
for (col in categorical_columns_to_encode) {
  data[[col]] <- as.factor(data[[col]])
}

newdata <- one_hot(data, cols = categorical_columns_to_encode)

# 5. Perform MCA (Multiple Correspondence Analysis)
afcm <- MCA(newdata, graph = FALSE)

# 6. Study the eigenvalues to understand the variance explained by each dimension
afcm$eig  # View the eigenvalues to see the explained variance

# 7. Create a biplot of individuals and variables
plot.MCA(afcm, choix = "ind", label = "all")  # Individuals
plot.MCA(afcm, choix = "var", label = "all")  # Variables

# 8. Analyze the contributions of each variable to the principal axes
afcm$contrib  # Display the contributions of each variable

# 9. Additional visualizations
# Plot both individuals and variables in one plot to study associations
plot.MCA(afcm, choix = "ind", label = "none")  # Customize labels as needed
plot.MCA(afcm, choix = "var", label = "none")

# 10. Examine associations between categories based on MCA contributions
# Use the contributions tables and biplots to identify strong associations

# 11. Interpretation
# Interpretation should be based on examining the positions of categories
# and the most contributing variables to the axes

# 12. Identify questions (variables) best represented by the MCA
# Use the contributions and variance tables to decide which questions (axes) are significant

# 13. Cross-tabulate two relevant questions
# Replace 'question1' and 'question2' with specific variable names, e.g. 'City' and 'Academic.Year'
table(data$City, data$Academic.Year)

# 14. Perform Correspondence Analysis (CA) on selected variables
ca_result <- CA(data[, c('City', 'Academic.Year')])  # Adjust variables as needed

# 15. Visualize and interpret CA results
plot.CA(ca_result)

# 16. Use the 'factoextra' package for additional visualization options
# Visualize individuals
fviz_mca_ind(afcm)

# Visualize variables
fviz_mca_var(afcm)

# Biplot of both individuals and variables
fviz_mca_biplot(afcm)

# Additional interpretations should be based on how the individuals and variables group together on the plots
