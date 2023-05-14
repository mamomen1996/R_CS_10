# Case-Study Title: Brand Perceptual Map (use PCA for data interpretation)
# Data Analysis methodology: CRISP-DM
# Dataset: 100 Customers answered 9 questions about 10 brands by likerd spectrum
# Case Goal: Knowing market atmosphere and Brand Positioning (know situation of 10 different Brands)


### Required Libraries ----
install.packages('pls')
install.packages('ggplot2')
install.packages('corrplot')
library('pls')
library('ggplot2')
library('corrplot')


### Read Data from File ----
data <- read.csv('CS_10.csv', header = T)
dim(data)  # 1000 records, 10 variables
#we have 10 brands and 9 features (100 persons evaluate each brand in 9 features by likerd spectrum)


### Step 1: Business Understanding ----
 # know business process and issues
 # know the context of the problem
 # know the order of numbers in the business


### Step 2: Data Understanding ----
### Step 2.1: Data Inspection (Data Understanding from Free Perspective) ----
## Dataset variables definition
colnames(data)

#perform  Brand has strong performance
#leader   Brand is a leader in the market
#latest   Brand has the latest products in market
#fun      Brand is fun
#serious  Brand is serious
#bargain  Brand products are a bargain (are cheap)
#value    Brand products are a good value
#trendy   Brand is trendy (is trend of market)
#rebuy    I would buy from Brand again


### Step 2.2: Data Exploring (Data Understanding from Statistical Perspective) ----
## Overview of Dataframe
summary(data)
data$brand <- as.factor(data$brand)
str(data)

## Correlation Analysis
corr_table <- round(cor(data[, 1:9]), 2)  # calculate correlations between continuous variables
View(corr_table)
corrplot::corrplot(corr_table)

## Descriptive Analysis on Brands
brand_mean <- aggregate(data[, 1:9], list(data$brand), mean)  # mean score of every brand in each feature
View(brand_mean)  # which brand is better in what?

rownames(brand_mean) <- brand_mean[, 1]
brand_mean <- brand_mean[,-1]  # remove 'brand' column
brand_mean

#prepare data for heat-map in ggplot2
brand_mean_df <- data.frame(brand = rep(rownames(brand_mean), 9),  # brand names
				var = rep(colnames(brand_mean), each = 10),  # feature names
				mean_value = unlist(brand_mean))  # means
rownames(brand_mean_df) <- NULL  # remove rownames
brand_mean_df

#plot heat-map (relative value of numbers are shown by colors)
hm_plot <- ggplot(data = brand_mean_df, aes(x = brand, y = var, fill = mean_value)) +
		geom_tile() +
		scale_fill_gradient(low = 'yellow', high = 'dark green')
hm_plot  # try to measure customers emotions about brands (each brand is known as what from customers mindset?), to know our brand's position in the market related to other brands.


### Step 3: Data PreProcessing ----
## Scale data for PCA
data_sc <- data
data_sc[, 1:9] <- scale(data[, 1:9])  # can not scale factor variable ('brand' column)
summary(data_sc)  # bring mean = 0 and distribution around mean


### Step 4: Modeling ----
# Model: PCA Analysis
#Run 1
pca_res <- prcomp(data_sc[, 1:9])
summary(pca_res)  # we had 9 variables, so PCA gives us 9 PC
#but, all of these 9 PCs do not explain same amount of data Variance and we don't want to use all of them. our goal is Dimension Reduction
 #PC1 explains 33% of data Variance 
 #PC2 explains 23% of data Variance
 #we can explain 56.4% of data Variance with PC1 and PC2 (Cumulative Proportion): so we just consider PC1 and PC2

#in Perceptual Map, we just consider 2 or 3 Principal Component to define brand-positions in 2D or 3D plots

#plot the results (each PC explains how much percentage of data Variance?)
plot(summary(pca_res)$importance[2,],
	xaxt = 'n',
	yaxt = 'n',
	ylab = 'Proportion of Variance',
	xlab = '# of Components')
axis(1, at = 1:9, labels = 1:9)
axis(2, at = seq(0, 0.35, 0.05), labels = seq(0, 0.35, 0.05))

biplot(pca_res)  # Preceptual Map for products (show data in PC1-PC2 space)
 #bottom-horizontal-axis: PC1
 #top-horizontal-axis: weight of PC1 in main variables
 #right-vertical-axis: weight of PC2 in main variables
 #left-vertical-axis: PC2

#according to the bi-plot, the space has been divided to 4 regions (same red-vectors (having same weight in of PC1 and PC2) make a region)
 #four regions (cluster variables based-on their weight in PC1 and PC2): 
 #   1.leadership cluster (serious, leader, perform) -> market leadership
 #   2.value cluster (rebuy, value, bargain) -> benefit/cost ratio
 #   3.trendiness cluster (trendy, latest)
 #   4.fun cluster (fun)

#actually, customers are measure different brands in 4 index

#Run 2
 #simplify the problem by running PCA on 'brand_mean' (10 row) instead of whole dataset (1000 row)
 #this helps us to have coordination of brands on bi-plot

brand_mean_pca <- prcomp(brand_mean, scale = T)
summary(brand_mean_pca)
#we can explain 87.07% of data Variance with PC1 and PC2 (Cumulative Proportion)

biplot(brand_mean_pca, main = 'Brand Positioning', cex = c(1.5, 1))  # Perceptual Map for brands
#now, we can see that the different brands in PC1-PC2 space, are positioned where (near which feature-clusters)?

#some Questions for analysis results:
 #What does the map tell us?
     #brand f and g are valueable brands in customer mindset (are near to value cluster and have good benefit/cost ratio) 
     #brand c and b are leader of market (forerunner) brands in customer mindset
     #brand a and j are fun brands
     #brand d and h and i are innovative brands
 #Suppose that you are the brand manager for brand e
     #your brand is famous as nothing! 
     #from marketing aspect, your brand is known as nothing between customers and this is so bad in marketing; because the customers don't know why should choose brand e (for what reason)?
     #customer who looks for leadership brand and performance, chooses c or b brands.
     #customer who looks for low-cost and good-benefit, chooses f or g brands.
     #customer who looks for latest products, chooses d or h or i brands.
 #What should you do about the position of your brand e? 
     #two possible solutions:
     #    follow other brands (where is other brands position in this industry? choose a position and close yourself to there)
     #    fill a gap (position yourself in a gap): value-leadership
