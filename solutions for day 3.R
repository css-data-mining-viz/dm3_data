# SOLUTIONS for in-class exercises on Day 3


# Hierarchical agglomerative clustering

# Load a few libraries
library(tidyverse)
library(skimr)
library(dendextend) # for "cutree" function
library(ggdendro)
library(patchwork)

# For questions 1-4, we will be using data from the 1977 US census statistical abstract, `state.x77`.

# 1. The data is currently stored as a matrix. Munge the data a bit accordingly: 
#     * Store as a tibble
#     * Display the summary statistics using the `skimr` package
#     * Restrict the data to include only: Income, Illiteracy, `Life Exp`, `HS Grad`
#     * Standardize the new restricted tibble
#     * Create a distance matrix using Euclidean distance

s <- as.data.frame(state.x77)

skim(s)

s_sub <- s %>% 
  select(Income, Illiteracy, `Life Exp`, `HS Grad`) %>% 
  scale() %>% 
  dist()

s_sub # inspect to make sure features are on the same scale

# 2. Fit and visualize (in a single pane) four HAC models with the following linkage methods respectively: single, complete, average, and centroid.
h1 <- hclust(s_sub, 
             method = "single")
hc_single <- ggdendrogram(h1) + labs(title = "Single")

h2 <- hclust(s_sub, 
             method = "complete")
hc_complete <- ggdendrogram(h2) + labs(title = "Complete")

h3 <- hclust(s_sub, 
             method = "average")
hc_average <- ggdendrogram(h3) + labs(title = "Average")

h4 <- hclust(s_sub,
             method = "centroid")
hc_centroid <- ggdendrogram(h4) + labs(title = "Centroid")

plots <- hc_single +
  hc_complete +
  hc_average +
  hc_centroid

plots + plot_annotation(
  title = 'Four Dendrograms for 1977 Census Data',
  subtitle = 'Hierarchical Aggplomerative Clustering Results')


# 3. Suppose we were unsure of the optimal number of clusters, between 3 and 4. Using the complete HAC fit, cut the tree and present the assignments for each scenario numerically (as a table).
cuts <- cutree(h2, 
               k = c(3,4)); cuts

# 4. Display the results from the previous question as a confusion atrix. How would you interpret the differences between 3 and 4 clusters? Where does the main split happen?
table(`3 Clusters` = cuts[,1], 
      `4 Clusters` = cuts[,2])




## k-means clustering

# For questions 5-14, use the 2012 presidential election Dem vote shares by state. Suppose we collecetd the vote shares by state form the 2012 presential election and normalized these data relative to votes for the Democratic candidate (Obama). We might want to see whether the 50 states cluster in two groups (for each major party, but with no party labels) or not. 

# Load data and clean up the column names for better interpretation
pres <- read_csv(file.choose()) 
colnames(pres)[colnames(pres)=="X1"] <- "State" 
colnames(pres)[colnames(pres)=="dem_vs"] <- "DVS"

head(pres)

# 5. Fit a k-means algorithm, initialized at k = 2 (be sure to set the seed). And inspect the structure of the kmeans. What output to you get?
set.seed(634)

kmeans <- kmeans(pres[ ,2], 
                 centers = 2,
                 nstart = 15)
str(kmeans)

kmeans$cluster
kmeans$centers
kmeans$size

# 6. Save the cluster assignments as factor and attach this new feature to the `pres` data frame for visualization in a bit.
pres$Cluster <- as.factor(kmeans$cluster) 

t <- as.table(kmeans$cluster)
(t <- data.frame(t))
rownames(t) <- pres$State
colnames(t)[colnames(t)=="Freq"] <- "Assignment"
t$Var1 <- NULL

head(t, 10)

# 7. Plot the distribution of states based on their cluster assignment. Be sure to vary color by cluster assignment, and think carefully about the best plot type for these univariate data.
ggplot(pres, aes(DVS, fill = Cluster)) + 
  geom_histogram(binwidth = 3) + 
  theme_minimal() +
  scale_fill_manual(values=c("darkblue", "darkred")) +
  labs(x = "Democratic Vote Share",
       y = "Count of States") +
  geom_vline(xintercept = 50, linetype="solid", 
             color = "darkgray", size=1.2)

# 8. Hopefully you saw that there was a state less than 50% (of democratic vote shares), but assigned to the other Democratic-voting states' cluster. Which state was it? 
which(pres$DVS < 50 & pres$DVS > 47) 

pres[19,]



# GMMs

# We will now use GMMs to cluster the same presidential data, `pres`. 

# Load a few extra libraries
library(mixtools)
library(plotGMM)

# Take a look at the density
ggplot(pres, aes(x = DVS)) +
  geom_density() + 
  xlim(min(pres$DVS)-10, max(pres$DVS)+10) +
  theme_minimal() +
  labs(x = "Democratic Vote Share")

# Best guess at component means (from the lecture notes)
ggplot(pres, aes(x = DVS)) +
  geom_density() + 
  xlim(min(pres$DVS)-10, max(pres$DVS)+10) +
  theme_minimal() +
  labs(x = "Democratic Vote Share") +
  geom_vline(xintercept = 41, col = "darkred") + 
  geom_vline(xintercept = 53, col = "darkblue")

# 9. Start by fitting and visualizing a 2-component gmm (again, assuming Dem and Rep voting states), using the `mixtools` package. Remember, the data are univariate and also set the seed for reproducibility. For plotting, construct this manually using skills obtained from the first part of the course. You will need to plot the density of the data, and then overlay the estimated component curves. *Hint*: Look into the `plot_mix_comps()` function and think about the `stat_function()` function in `ggplot2`.

set.seed(123) 

library(mixtools)
library(plotmm)
library(plotGMM)

gmm1 <- normalmixEM(pres$DVS, k = 2)
gmm1$lambda
ggplot(data.frame(x = gmm1$x)) +
  geom_histogram(aes(x, ..density..), fill = "darkgray") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(gmm1$mu[1], gmm1$sigma[1], lam = gmm1$lambda[1]),
                colour = "darkred") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(gmm1$mu[2], gmm1$sigma[2], lam = gmm1$lambda[2]),
                colour = "darkblue") +
  xlab("Democratic Vote Shares") +
  ylab("Density") + 
  theme_minimal()


# 10. That doesn't seem to fit too well. There seems to be an outlier. So now, update the fit to include 3 components to try and account for the outlier state. Again, manually visualize these component curves overlaid on the density of the data (i.e., density on the Y axis and DVS on the X axis). Is this any better?

set.seed(123) 

gmm2 <- normalmixEM(pres$DVS, k = 3)

ggplot(data.frame(x = gmm2$x)) +
  geom_histogram(aes(x, ..density..), fill = "darkgray") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(gmm2$mu[1], gmm2$sigma[1], lam = gmm2$lambda[1]),
                colour = "darkred") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(gmm2$mu[2], gmm2$sigma[2], lam = gmm2$lambda[2]),
                colour = "darkblue") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(gmm2$mu[3], gmm2$sigma[3], lam = gmm2$lambda[3]),
                colour = "black") +
  xlab("Democratic Vote Shares") +
  ylab("Density") + 
  theme_minimal()


# 11. Maybe a little better, but the outlier is definitely an issue impacting the fit. Search for the outlier, omit it, and refit the 2-component GMM on the data without the outlier. Visualize these results in the same way as before. Any better?

which(pres$DVS > 80)
pres[17, ] 

pres2 <- pres[-c(17), ]

withDC <- head(pres$DVS, 20)
withoutDC <- head(pres2$DVS, 20)

head(data.frame(cbind(withDC, withoutDC)), 20)

set.seed(123)

dvs.nodc <- pres2$DVS
gmm.nodc <- normalmixEM(dvs.nodc, k = 2)

ggplot(data.frame(x = gmm.nodc$x)) +
  geom_histogram(aes(x, ..density..), fill = "darkgray", bins = 20) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(gmm.nodc$mu[1], gmm.nodc$sigma[1], lam = gmm.nodc$lambda[1]),
                colour = "darkred") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(gmm.nodc$mu[2], gmm.nodc$sigma[2], lam = gmm.nodc$lambda[2]),
                colour = "darkblue") +
  xlab("Democratic Vote Shares") +
  ylab("Density") + 
  theme_minimal()

# 12. Explore component densities a bit. First create a data frame of the posterior densities for each component (i.e., each component's densities should be in individual columns).
posterior <- data.frame(cbind(gmm.nodc$x, gmm.nodc$posterior)) %>% 
  rename(DVS = V1)
rownames(posterior) <- pres2$State
round(head(posterior, 10), 3)

# 13. Create a new dichotomous feature called `Component` where 1 = a value in component 1 greater than 0.3, else = 2 (putting all likely "Republican" states in group 1, and all likely Democratic states in group 2) -- necessary step for plotting.
posterior$component <- ifelse(posterior$comp.1 > 0.3, 1, 2)
table(posterior$component) # 24 states in comp 1, and 26 states in comp 2 == the actual election outcome

# 14. Visualize the density of DVS, with color varying by component. What do you see? 
ggplot(posterior, aes(x = DVS)) + 
  geom_histogram(aes(fill = factor(component)), stat ="bin", binwidth = 3) +
  labs(x = "Democratic Vote Share",
       y = "Count of States",
       title = "Gaussian Mixture Model") +
  scale_fill_manual(values=c("darkred", "darkblue"),
                    name="Component",
                    breaks=c("1", "2"),
                    labels=c("1", "2")) +
  geom_vline(xintercept = 50, linetype="solid", 
             color = "darkgray", size=1.2) +
  theme_minimal()

## Fuzzy c-means

# For the following implementations of fuzzy C-means and DBSCAN in questions 15 - 20, use the state legislative professionalism data (read in .Rdata as `x`, then munge).

st <- x %>% 
  filter(sessid == "2009/10") %>% 
  select(-c(fips, stateabv, sessid, mds1, mds2, year)) %>%
  na.omit(st); skim(st)

st_scale <- data.frame(scale(st[,2:5]))

states <- st$state # save state names for plot

# 15. Fit an FCM model initialized at `centers = 2` (searching for 2 clusters: professional state legislatures vs. non-professional legislatures - its an exploratory assumption, hence the validity for this exercise).
cm <- cmeans(st_scale, 
             centers = 2)

# 16. Append the cluster assignments to the scaled data set (`st_scale`), and relevel the factor categories from {1,2} to {2,1} to make "professional" states in the higher cluster 2 for intuition, though this isn't technically required.
st_scale$Cluster <- cm$cluster 
st_scale$Cluster <- as.factor( 
  ifelse(st_scale$Cluster == 1, 2, 1)
) 

table(st_scale$Cluster) 

# 17. Visualize the FCM cluster assignments over the features salaries (`salary_real`) and expenditures (`expend`). What do you see? Does this seem reasonable, based on whatever you might know about these states?
ggplot(st_scale, aes(salary_real, expend, 
                     color = Cluster, 
                     label = states)) +
  geom_jitter() +
  geom_label(aes(label = states, 
                 color = Cluster), 
             size = 3) + 
  xlim(-1.3, 3.7) +
  labs(x = "Salary",
       y = "Expenditures",
       title = "State Legislative Professionalism by Expenditures & Salary",
       subtitle = "Clusters from Fuzzy C-Means Algorithm")

# 18. Display the numic output of the fractional assignments of each state to each cluster. Discuss the output. Does it corroborate the visual output? Why or why not? 
states <- st$state
membership <- as.data.frame(cm$membership[1:10,])
rownames(membership) <- states[1:10]
membership <- membership[ ,c(2,1)]
colnames(membership) <- c("Cluster 1", "Cluster 2")
round(membership, 2)



# 
## DBSCAN

# First, update the data a bit for DBSCAN application (e.g., read as matrix, change feature names for plot, etc.).
st_scale <- data.frame(scale(st[,2:5])) %>% 
  rename(`Total Length` = t_slength,
         `Regular Length` = slength,
         `Salary` = salary_real,
         `Expenditures` = expend) %>% 
  as.matrix()


# The first step in DBSCAN is to determine the optimal epsilon value. Recall that \epsilon (`eps` in `dbscan()`) is the maximum distance between two points, which informs clustering. The `kNNdistplot()` function plots the average distance between every point and its nearest neighbors, and are arranged in ascending order. Though a bit ambiguous, we are looking for an "elbow" or a notable change in average distance, suggesting distance between points is growing, and is thus signaling the possibility of outliers, which informs the threshold for the neighborhood size.
kNNdistplot(st_scale, 
            k = 4) # number of nearest neighbors
abline(h = 1.2, # looks like the elbow is around 1.2
       col = "red")

# you can also inspect the average distances via `kNNdist(st_scale, k = 4)`

# 19. Fit a DBSCAN model, initialized at your selected `eps` and `minPts` values previously covered. 
dbscan_fit <- dbscan(st_scale, 
                     eps = 1.2, 
                     minPts = 4)

# 20. Visualize your results, first across all features, then across the first two dimensions (*Hint*: consider using `fviz_cluster()` from `factoextra` for this one).
pairs(st_scale, 
      col = ifelse(dbscan_fit$cluster == 1, "#F8766D", "#00BFC4"), 
      pch = 19)

# note: blue points/states are treated as outliers/noise, which is at the heart of the difference in the DBSCAN approach to clustering.

rownames(st_scale) <- st$state

fviz_cluster(dbscan_fit, st_scale, 
             repel = TRUE,
             show.clust.cent = FALSE,
             outlier.color = "#00BFC4", 
             labelsize = 6,
             pointsize = 1.5) +
  labs(title = "State Legislative Professionalism Cluster Assignments",
       subtitle = "Clusters from DBSCAN Algorithm") +
  theme_minimal()
