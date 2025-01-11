#insert data
marketing <- read.csv("marketing_campaign.csv",sep = "\t", header = T)

library(tidyverse)

options(warn=-1)
df=data.frame(marketing)

#checking the data
head(df)

#checking for the missing values
missing.values <- df %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels =(missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot = missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('#adcae6', 'red'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x ='Variable', y = "% of missing values")
percentage.plot

df.clean <- na.omit(df)

#######################
# visualisasi sebelom feature selection


# Menghitung usia berdasarkan Year_Birth
df$Age <- 2024 - df$Year_Birth

# Menghitung total pengeluaran
df$Spending <- df$MntWines + df$MntFruits + df$MntMeatProducts + 
  df$MntFishProducts + df$MntSweetProducts + df$MntGoldProds

# Menghitung rata-rata pengeluaran berdasarkan usia
avg_spending_before <- aggregate(df$Spending, by = list(Age = df$Age), FUN = mean)

# Membuat barplot
barplot(avg_spending_before$x, 
        names.arg = avg_spending_before$Age, 
        col = rainbow(length(unique(df$Age))), 
        cex.axis = 0.6, 
        cex.names = 0.6,
        las = 2,
        xlab = "Age", 
        ylab = "Average Spending", 
        main = "Correlation Between Customers' Age and Average Spendings (Before Feature Selection)")


#######################

#feature selection
df.clean['Age'] <- 2024-df.clean$Year_Birth

df.clean <- df.clean %>% filter(Age <= 80)

df.clean['Child'] <- df.clean$Kidhome + df.clean$Teenhome

df.clean['Spending'] <- df.clean$MntWines + df.clean$MntFruits + df.clean$MntMeatProducts + df.clean$MntFishProducts +
  df.clean$MntSweetProducts + df.clean$MntGoldProds

df.clean['AcceptedPromotion'] <- df.clean$AcceptedCmp1 + df.clean$AcceptedCmp2 + df.clean$AcceptedCmp3 + 
  df.clean$AcceptedCmp4 + df.clean$AcceptedCmp5

df.clean['Edu'] <- ifelse(df.clean$Education %in% c("Basic", "2n Cycle"), "Undergraduate",
                          ifelse(df.clean$Education == "Graduation", "Graduate",
                                 ifelse(df.clean$Education %in% c("Master", "PhD"), "Postgraduate", "")))

df.clean['EducationLevel'] <- as.numeric(ifelse(df.clean$Edu == "Undergraduate",1,
                                                ifelse(df.clean$Edu == "Graduate", 2,
                                                       ifelse(df.clean$Edu == "Postgraduate", 3, 0))))

df.clean['MaritalStat'] <- ifelse(df.clean$Marital_Status == "Married" | df.clean$Marital_Status == "Together", "Partner",
                                  ifelse(df.clean$Marital_Status %in% c("Absurd", "Widow", "YOLO", "Divorced", "Single"), "Alone", ""))

df.clean['RelStatus'] <- ifelse(df.clean$MaritalStat == "Partner", 2, 1)

df.clean['Purchase'] <- df.clean$NumDealsPurchases + df.clean$NumWebPurchases + df.clean$NumCatalogPurchases + df.clean$NumStorePurchases

df.clean=df.clean[c(-1,-2,-3,-6,-10,-11,-12,-13,-14,-15.-16,-17,-18,-19,-21,-22,-23,-24,-25)]
head(df.clean)
df.clean=df.clean[c(-1,-4,-6, -7,-16,-18)]

#graph
hist(df.clean$Income, col = rainbow(length(unique(df.clean$Income))), xlim = c(0, 250000), ylim = c(0,1200),xlab = "income", 
     main = "Histogram of Income")
hist(df.clean$Purchase, col = rainbow(length(unique(df.clean$Purchase))),xlab = "Purchase", main = "Histogram of Purchase")
hist(df.clean$Spending, col = rainbow(length(unique(df.clean$Spending))),xlab = "Spending", main = "Histogram of Spending")
hist(df.clean$Age, col = rainbow(length(unique(df.clean$Age))),xlab = "Age", main = "Histogram of Age")

avg_spending <- aggregate(df.clean$Spending, by = list(Age = df.clean$Age), FUN = mean)

barplot(avg_spending$x, 
        names.arg = avg_spending$Age, 
        col = rainbow(length(unique(df.clean$Age))), 
        cex.axis = 0.6, 
        cex.names = 0.6,
        las = 2,
        xlab = "Age", 
        ylab = "Average Spending", 
        main = "Correlation Between Customers' Age and Average Spendings")

boxplot(df.clean$Income~df.clean$Education, col = rainbow(length(unique(df.clean$Education))), 
        cex.axis = 0.7, 
        xlab = "Education", 
        ylab = "Income", 
        main = "Correlation Between Customers' Education and Income", 
        ylim = c(0,150000))

avg_purchase <- aggregate(df.clean$Income, by = list(Purchase = df.clean$Purchase), FUN = mean)

barplot(avg_purchase$x, 
        names.arg = avg_purchase$Purchase, 
        col = rainbow(length(unique(df.clean$Purchase))), 
        cex.axis = 0.6,
        cex.names = 0.6,
        las = 2,
        xlab = "Purchase", 
        ylab = "Income", 
        main = "Correlation Between Customers' Income and Purchase")

boxplot(df.clean$Purchase~df.clean$AcceptedPromotion, col = rainbow(length(unique(df.clean$AcceptedPromotion))),
        ylab = "Purchase", xlab = "Promotion")

library(corrplot)

#Getting correlation matrix 
cust_cor <- cor(df.clean[, !(names(df.clean) %in% c("Z_CostContact", "Z_Revenue"))])

# Visualize correlation matrix
corrplot(cust_cor, method = "color", addCoef.col = "white")

#KMeans<---------------
library(factoextra)

#sum(is.na(df.clean))  # cek nilai NA
#sum(is.nan(as.matrix(df.clean)))  # cek nilai NaN
#sum(is.infinite(as.matrix(df.clean)))  # cek nilai Inf

fviz_nbclust(df.clean, kmeans, method="wss")+geom_vline(xintercept=3,linetype=2)

set.seed(123)
km.res <- kmeans(df.clean, 3, nstart = 10)

print(km.res$centers)
print(km.res$size)
print(km.res$betweenss/km.res$totss)

df.clean <- df.clean[, sapply(df.clean, function(x) var(x, na.rm = TRUE) != 0)]

fviz_cluster(km.res, df.clean, geom = "point", ellipse.type = "norm", repel = TRUE)


#ploting  #income , spending, purchase
df.clean['cluster']=as.factor(km.res$cluster)
head(df.clean)
attach(df.clean)

# Plot for Income
incomeplot = ggplot(df.clean, aes(x=cluster, y=Income, fill=cluster)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=TRUE) +
  ggtitle("Clusters' Boxplot of Income") +
  labs(x = "Cluster", y = "Income") +
  ylim(0, 200000)

# Plot for Spending
spenplot = ggplot(df.clean, aes(x=cluster, y=Spending, fill=cluster)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=T) +
  ggtitle("Clusters' Boxplot of Spending") +
  labs(x = "Cluster", y = "Spending")

# Plot for Purchase
purchaseplot = ggplot(df.clean, aes(x=cluster, y=Purchase, fill=cluster)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=T) +
  ggtitle("Clusters' Boxplot of Purchase") +
  labs(x = "Cluster", y = "Purchase")

# Display the plots
incomeplot
spenplot
purchaseplot

#####################################################################################################################################
# UNTUK K-MEANS SCORE using Silhuette and DB-Index
library(cluster)  # Untuk silhouette score
library(factoextra)  # Untuk clustering visualizations
library(clusterSim)  # Untuk Davies-Bouldin Index

#Silhouette Score
silhouette_score <- silhouette(km.res$cluster, dist(df.clean[, -ncol(df.clean)]))  # Kecualikan kolom 'cluster'
avg_silhouette <- mean(silhouette_score[, 3])  # Rata-rata Silhouette Score

#Davies-Bouldin Index
db_index <- index.DB(df.clean[, -ncol(df.clean)], km.res$cluster)$DB  # Kecualikan kolom 'cluster'

#Skala nilai DB Index
db_index_scaled <- 1 / (1 + db_index)  # Nilai DB Index biasanya lebih baik jika kecil, jadi gunakan skala inversi

#Data untuk barplot
scores <- data.frame(
  Metric = c("Silhouette Score", "Davies-Bouldin Index"),
  Value = c(avg_silhouette, db_index_scaled)
)

ggplot(scores, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
  scale_y_continuous(limits = c(0, 1)) +  # Atur rentang y dari 0 hingga 1
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(
    title = "Clustering Evaluation Metrics (K-Means)",
    x = "Metric",
    y = "Score"
  ) +
  theme_minimal()

##################################################################################################################################

library(dbscan)
library(ggplot2)
library(cluster)
library(clusterSim)

# Normalize and scale the data
df_numeric <- df.clean[, sapply(df.clean, is.numeric)]  # Select only numeric columns
df_numeric <- na.omit(df_numeric)  # Remove any rows with NA values
df.scaled2 <- scale(df_numeric)  # Standardize the data

# Apply DBSCAN clustering with adjusted parameters
dbscan_result <- dbscan(df.scaled2, eps = 2, MinPts = 50)

# Add group labels from DBSCAN
df.clean$dbscan_group <- dbscan_result$cluster

# Dynamically check and re-map clusters into exactly 3 groups
# Step 1: Count original clusters
unique_clusters <- unique(df.clean$dbscan_group[df.clean$dbscan_group > 0])  # Exclude noise (cluster 0)
num_clusters_dbscan <- length(unique_clusters)

# Step 2: If the number of clusters is more than 3, reduce clusters to 3
if (num_clusters_dbscan > 3) {
  # Map original clusters into 3 groups based on their size
  cluster_sizes <- sort(table(df.clean$dbscan_group[df.clean$dbscan_group > 0]), decreasing = TRUE)
  top_clusters <- names(cluster_sizes)[1:3]  # Select top 3 clusters by size
  df.clean$dbscan_group <- ifelse(df.clean$dbscan_group %in% as.numeric(top_clusters), 
                                  df.clean$dbscan_group, 0)  # Merge other clusters into noise (0)
}

# Step 3: Re-label clusters into exactly 3 groups: 1, 2, 3
final_clusters <- unique(df.clean$dbscan_group)
df.clean$dbscan_group <- factor(df.clean$dbscan_group, levels = final_clusters)
levels(df.clean$dbscan_group) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Outliers")  # Label outliers if any

# Use PCA to visualize the DBSCAN clusters
pca_result_dbscan <- prcomp(df.scaled2, center = TRUE, scale. = TRUE)

# Create a dataframe for PCA results
pca_data_dbscan <- as.data.frame(pca_result_dbscan$x)
pca_data_dbscan$dbscan_group <- df.clean$dbscan_group

# Plot DBSCAN results
dbscan_plot <- ggplot(data = pca_data_dbscan, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = dbscan_group)) +
  scale_color_manual(values = c("red", "green", "blue", "black")) +  # Customize colors
  ggtitle("DBSCAN Clustering") +
  theme_minimal()

# Display the DBSCAN plot
print(dbscan_plot)

# Silhouette Score and Davies-Bouldin Index for DBSCAN
# Filter out noise points (if any) for evaluation metrics
valid_indices <- which(df.clean$dbscan_group != "Outliers")  # Exclude outliers
valid_clusters <- as.numeric(factor(df.clean$dbscan_group[valid_indices]))  # Convert to numeric
df_filtered <- df.scaled2[valid_indices, ]  # Filter scaled data

# Silhouette Score
silhouette_score_dbscan <- silhouette(valid_clusters, dist(df_filtered))  
avg_silhouette_dbscan <- mean(silhouette_score_dbscan[, 3])  # Mean silhouette score

# Davies-Bouldin Index
db_index_dbscan <- index.DB(df_filtered, valid_clusters)$DB  

# Scale DB Index to be in the range 0-1
db_index_scaled_dbscan <- 1 / (1 + db_index_dbscan)

# Create data for plotting DBSCAN metrics
scores_dbscan <- data.frame(
  Metric = c("Silhouette Score", "Davies-Bouldin Index"),
  Value = c(avg_silhouette_dbscan, db_index_scaled_dbscan)
)

# Plot DBSCAN evaluation metrics
ggplot(scores_dbscan, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(
    title = "Clustering Evaluation Metrics (DBSCAN)",
    x = "Metric",
    y = "Score"
  ) +
  theme_minimal()



######################################################################

library(dbscan)
library(ggplot2)

# Menyaring data hanya untuk kolom numerik
df_numeric <- df.clean[, sapply(df.clean, is.numeric)]  # Pilih kolom numerik
df_numeric <- na.omit(df_numeric)  # Hilangkan NA
df.scaled <- scale(df_numeric)  # Skala data

# Terapkan OPTICS
optics_result <- optics(df.scaled, minPts = 5)

# Ekstrak kluster menggunakan metode Xi
res_xi <- extractXi(optics_result, xi = 0.05)

# Menampilkan hasil kluster untuk melihat distribusi kluster
print(table(res_xi$cluster))

# Memilih 3 kluster terbesar (atau pilih kluster yang relevan)
top_3_clusters <- sort(table(res_xi$cluster), decreasing = TRUE)[1:3]  # Memilih 3 kluster terbesar
keep_clusters <- names(top_3_clusters)

# Gabungkan cluster lainnya 
df.clean$cluster <- res_xi$cluster
df.clean$cluster[!(df.clean$cluster %in% keep_clusters)] <- 0  

# Visualisasi dengan PCA
pca_result <- prcomp(df.scaled, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x)
pca_data$cluster <- factor(df.clean$cluster)

# Plot hasil clustering menggunakan warna manual untuk setiap cluster
optics_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  labs(title = "OPTICS Clustering") +
  scale_color_manual(
    values = c("red", "green", "blue","black"),
    labels = c("Cluster 1", "Cluster 2", "Cluster 3","outliers")  
  ) +
  theme_minimal()

# Display the OPTICS plot
print(optics_plot)

# Ensure OPTICS clustering is applied first
optics_result <- optics(df.scaled2, minPts = 5)

# Extract clusters using Xi method
res_xi_optics <- extractXi(optics_result, xi = 0.05)

# Convert OPTICS cluster labels to numeric for silhouette score calculation
res_xi_optics_numeric <- as.numeric(res_xi_optics$cluster)

# Silhouette Score for OPTICS
silhouette_score_optics <- silhouette(res_xi_optics_numeric, dist(df.scaled2))  
avg_silhouette_optics <- mean(silhouette_score_optics[, 3])

# Davies-Bouldin Index for OPTICS
db_index_optics <- index.DB(df.scaled2, res_xi_optics_numeric)$DB  

# Scale DB Index to be in the range 0-1
db_index_scaled_optics <- 1 / (1 + db_index_optics)

# Create data for plotting OPTICS metrics
scores_optics <- data.frame(
  Metric = c("Silhouette Score", "Davies-Bouldin Index"),
  Value = c(avg_silhouette_optics, db_index_scaled_optics)
)

# Plot Evaluation Metrics
metrics_plot <- ggplot(scores_optics, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(
    title = "Clustering Evaluation Metrics (OPTICS)",
    x = "Metric",
    y = "Score"
  ) +
  theme_minimal()

print(metrics_plot)




