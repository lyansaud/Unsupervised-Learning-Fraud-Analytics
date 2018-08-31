library(ggplot2)
library(dplyr)
library(scales)
library(knitr)
library(tidyr)
library(lubridate)
library(ggmap)
library(stringr)
library(gdata)

setwd("~/Desktop/DSO 562/Data")

ny_property_merge = read.csv("ny_property_merge.csv")

##Create Expert Variables 

#FULLVAL and ZIP

data_pca_merge = ny_property_merge[,FALSE]



data_pca_merge$Var1 = ny_property_merge$FULLVAL_BLDAREA / ny_property_merge$ZIP_FULLVAL_BLDAREA_MEAN
data_pca_merge$Var2 = ny_property_merge$FULLVAL_BLDVOL / ny_property_merge$ZIP_FULLVAL_BLDVOL_MEAN
data_pca_merge$Var3 = ny_property_merge$FULLVAL_LOTAREA / ny_property_merge$ZIP_FULLVAL_LOTAREA_MEAN

#AVTOT and ZIP

data_pca_merge$Var4 = ny_property_merge$AVTOT_BLDAREA / ny_property_merge$ZIP_AVTOT_BLDAREA_MEAN
data_pca_merge$Var5 = ny_property_merge$AVTOT_BLDVOL / ny_property_merge$ZIP_AVTOT_BLDVOL_MEAN
data_pca_merge$Var6 = ny_property_merge$AVLAND_LOTAREA / ny_property_merge$ZIP_AVLAND_LOTAREA_MEAN

#AVLAND and ZIP

data_pca_merge$Var7 = ny_property_merge$AVLAND_BLDAREA / ny_property_merge$ZIP_AVLAND_BLDAREA_MEAN
data_pca_merge$Var8 = ny_property_merge$AVLAND_BLDVOL / ny_property_merge$ZIP_AVLAND_BLDVOL_MEAN
data_pca_merge$Var9 = ny_property_merge$AVLAND_LOTAREA / ny_property_merge$ZIP_AVLAND_LOTAREA_MEAN

#FULLVAL and ZIP3

data_pca_merge$Var10 = ny_property_merge$FULLVAL_BLDAREA / ny_property_merge$ZIP3_FULLVAL_BLDAREA_MEAN
data_pca_merge$Var11 = ny_property_merge$FULLVAL_BLDVOL / ny_property_merge$ZIP3_FULLVAL_BLDVOL_MEAN
data_pca_merge$Var12 = ny_property_merge$FULLVAL_LOTAREA / ny_property_merge$ZIP3_FULLVAL_LOTAREA_MEAN

#AVTOT and ZIP3

data_pca_merge$Var13 = ny_property_merge$AVTOT_BLDAREA / ny_property_merge$ZIP3_AVTOT_BLDAREA_MEAN
data_pca_merge$Var14 = ny_property_merge$AVTOT_BLDVOL / ny_property_merge$ZIP3_AVTOT_BLDVOL_MEAN
data_pca_merge$Var15 = ny_property_merge$AVLAND_LOTAREA / ny_property_merge$ZIP3_AVLAND_LOTAREA_MEAN

#AVLAND and ZIP3

data_pca_merge$Var16 = ny_property_merge$AVLAND_BLDAREA / ny_property_merge$ZIP3_AVLAND_BLDAREA_MEAN
data_pca_merge$Var17 = ny_property_merge$AVLAND_BLDVOL / ny_property_merge$ZIP3_AVLAND_BLDVOL_MEAN
data_pca_merge$Var18 = ny_property_merge$AVLAND_LOTAREA / ny_property_merge$ZIP3_AVLAND_LOTAREA_MEAN

#FULLVAL and TAXCLASS

data_pca_merge$Var19 = ny_property_merge$FULLVAL_BLDAREA / ny_property_merge$TAXCLASS_FULLVAL_BLDAREA_MEAN
data_pca_merge$Var20 = ny_property_merge$FULLVAL_BLDVOL / ny_property_merge$TAXCLASS_FULLVAL_BLDVOL_MEAN
data_pca_merge$Var21 = ny_property_merge$FULLVAL_LOTAREA / ny_property_merge$TAXCLASS_FULLVAL_LOTAREA_MEAN

#AVTOT and TAXCLASS

data_pca_merge$Var22 = ny_property_merge$AVTOT_BLDAREA / ny_property_merge$TAXCLASS_AVTOT_BLDAREA_MEAN
data_pca_merge$Var23 = ny_property_merge$AVTOT_BLDVOL / ny_property_merge$TAXCLASS_AVTOT_BLDVOL_MEAN
data_pca_merge$Var24 = ny_property_merge$AVLAND_LOTAREA / ny_property_merge$TAXCLASS_AVLAND_LOTAREA_MEAN

#AVLAND and TAXCLASS

data_pca_merge$Var25 = ny_property_merge$AVLAND_BLDAREA / ny_property_merge$TAXCLASS_AVLAND_BLDAREA_MEAN
data_pca_merge$Var26 = ny_property_merge$AVLAND_BLDVOL / ny_property_merge$TAXCLASS_AVLAND_BLDVOL_MEAN
data_pca_merge$Var27 = ny_property_merge$AVLAND_LOTAREA / ny_property_merge$TAXCLASS_AVLAND_LOTAREA_MEAN


#BORO 

data_pca_merge$Var36 = ny_property_merge$FULLVAL_BLDAREA / ny_property_merge$BORO_FULLVAL_BLDAREA_MEAN  
data_pca_merge$Var35 = ny_property_merge$FULLVAL_LOTAREA / ny_property_merge$BORO_FULLVAL_LOTAREA_MEAN  
data_pca_merge$Var34 = ny_property_merge$FULLVAL_BLDVOL / ny_property_merge$BORO_FULLVAL_BLDVOL_MEAN  

data_pca_merge$Var33 = ny_property_merge$AVTOT_BLDAREA / ny_property_merge$BORO_AVTOT_BLDAREA_MEAN  
data_pca_merge$Var32 = ny_property_merge$AVTOT_LOTAREA / ny_property_merge$BORO_AVTOT_LOTAREA_MEAN 
data_pca_merge$Var31 = ny_property_merge$AVTOT_BLDVOL / ny_property_merge$BORO_AVTOT_BLDVOL_MEAN 

data_pca_merge$Var30 = ny_property_merge$AVLAND_BLDAREA / ny_property_merge$BORO_AVLAND_BLDAREA_MEAN  
data_pca_merge$Var29 = ny_property_merge$AVLAND_LOTAREA / ny_property_merge$BORO_AVLAND_LOTAREA_MEAN  
data_pca_merge$Var28 = ny_property_merge$AVLAND_BLDVOL / ny_property_merge$BORO_AVLAND_BLDVOL_MEAN

#ALL

data_pca_merge$Var45 = ny_property_merge$FULLVAL_BLDAREA / ny_property_merge$ALL_FULLVAL_BLDAREA_MEAN  
data_pca_merge$Var44 = ny_property_merge$FULLVAL_LOTAREA / ny_property_merge$ALL_FULLVAL_LOTAREA_MEAN  
data_pca_merge$Var43 = ny_property_merge$FULLVAL_BLDVOL / ny_property_merge$ALL_FULLVAL_BLDVOL_MEAN  

data_pca_merge$Var42 = ny_property_merge$AVTOT_BLDAREA / ny_property_merge$ALL_AVTOT_BLDAREA_MEAN  
data_pca_merge$Var41 = ny_property_merge$AVTOT_LOTAREA / ny_property_merge$ALL_AVTOT_LOTAREA_MEAN 
data_pca_merge$Var40 = ny_property_merge$AVTOT_BLDVOL / ny_property_merge$ALL_AVTOT_BLDVOL_MEAN 

data_pca_merge$Var39 = ny_property_merge$AVLAND_BLDAREA / ny_property_merge$ALL_AVLAND_BLDAREA_MEAN  
data_pca_merge$Var38 = ny_property_merge$AVLAND_LOTAREA / ny_property_merge$ALL_AVLAND_LOTAREA_MEAN  
data_pca_merge$Var37 = ny_property_merge$AVLAND_BLDVOL / ny_property_merge$ALL_AVLAND_BLDVOL_MEAN  

#More expert to reah a total of 51

data_pca_merge$Var46 = ny_property_merge$FULLVAL/ny_property_merge$ZIP_FULLVAL_MEAN
data_pca_merge$Var47 = ny_property_merge$AVTOT/ny_property_merge$ZIP_AVTOT_MEAN
data_pca_merge$Var48 = ny_property_merge$AVLAND/ny_property_merge$ZIP_AVLAND_MEAN
data_pca_merge$Var49 = ny_property_merge$FULLVAL/ny_property_merge$BORO_FULLVAL_MEAN
data_pca_merge$Var50 = ny_property_merge$AVTOT/ny_property_merge$BORO_AVTOT_MEAN
data_pca_merge$Var51 = ny_property_merge$AVLAND/ny_property_merge$BORO_AVLAND_MEAN


View(data_pca_merge)

write.csv(data_pca_merge, 'data_pca_merge.csv')


###Additional expert variables that we dont need (or maybe need later) :  cmd shift C



# Var4 = ny_merge$FULLVAL/ny_merge$TAXCLASS_FULLVAL_MEAN
# Var5 = ny_merge$FULLVAL/ny_merge$ZIP3_FULLVAL_MEAN
# Var4 = ny_merge$FULLVAL/ny_merge$BORO_FULLVAL_MEAN
# Var5 = ny_merge$FULLVAL/ny_merge$EASEMENT_FULLVAL_MEAN
# Var6 = ny_merge$FULLVAL/ny_merge$BLDGCL_FULLVAL_MEAN
# 
# Var7 = ny_merge$FULLVAL*ny_merge$LOTAREA
# Var8 = ny_merge$FULLVAL*ny_merge$BLDAREA
# Var9 = ny_merge$FULLVAL*ny_merge$BLDVOL
# Var10 = ny_merge$FULLVAL*ny_merge$LOTAREA*ny_merge$STORIES
# 
# Var11 = ny_merge$FULLVAL/ny_merge$ZIP_STORIES_MEAN
# 
# #AVTOT 
# Var48 = ny_merge$AVTOT/ny_merge$ZIP_AVTOT_MEAN
# Var13 = ny_merge$AVTOT/ny_merge$TAXCLASS_AVTOT_MEAN
# Var14 = ny_merge$AVTOT/ny_merge$ZIP3_AVTOT_MEAN
# Var15 = ny_merge$AVTOT/ny_merge$BORO_AVTOT_MEAN
# Var16 = ny_merge$AVTOT/ny_merge$EASEMENT_AVTOT_MEAN
# Var17 = ny_merge$AVTOT/ny_merge$BLDGCL_AVTOT_MEAN 
# 
# Var18 = ny_merge$AVTOT * ny_merge$LOTAREA
# Var19 = ny_merge$AVTOT * ny_merge$BLDAREA 
# Var20 = ny_merge$AVTOT * ny_merge$BLDVOL 
# Var21 = ny_merge$AVTOT * ny_merge$LOTAREA * ny_merge$STORIES
# Var22 = ny_merge$AVTOT / ny_merge$ZIP_STORIES_MEAN
# 
# #AVLAND
# Var23 = ny_merge$AVLAND/ny_merge$ZIP_AVLAND_MEAN
# Var24 = ny_merge$AVLAND/ny_merge$TAXCLASS_AVLAND_MEAN
# Var25 = ny_merge$AVLAND/ny_merge$ZIP3_AVLAND_MEAN
# Var26 = ny_merge$AVLAND/ny_merge$BORO_AVLAND_MEAN
# Var27 = ny_merge$AVLAND/ny_merge$EASEMENT_AVLAND_MEAN
# Var28 = ny_merge$AVLAND/ny_merge$BLDGCL_AVLAND_MEAN
# 
# Var29 = ny_merge$AVLAND * ny_merge$LOTAREA
# Var30 = ny_merge$AVLAND * ny_merge$BLDAREA
# Var31 = ny_merge$AVLAND * ny_merge$BLDVOL
# Var32 = ny_merge$AVLAND * ny_merge$LOTAREA * ny_merge$STORIES
# Var33 = ny_merge$AVLAND / ny_merge$ZIP_STORIES_MEAN
# 
# Var34 = mean(ny_merge$AVLAND) * ny_merge$LOTAREA 
# Var35 = mean(ny_merge$AVLAND) * ny_merge$BLDAREA
# Var36 = mean(ny_merge$AVLAND) * ny_merge$BLDVOL
# 
# #FULLVAL
# Var37 = mean(ny_merge$FULLVAL) * ny_merge$LOTAREA
# Var38 = mean(ny_merge$FULLVAL) * ny_merge$BLDAREA
# Var39 = mean(ny_merge$FULLVAL) * ny_merge$BLDVOL
# 
# #AVTOT
# Var40 = mean(ny_merge$AVTOT) * ny_merge$LOTAREA
# Var41 = mean(ny_merge$AVTOT) * ny_merge$BLDAREA
# Var42 = mean(ny_merge$AVTOT) * ny_merge$BLDVOL
# 
# Var43 = ny_merge$AVTOT / ny_merge$BLDVOL
# Var44 = ny_merge$FULLVAL / ny_merge$AVTOT
# Var45 = ny_merge$FULLVAL / ny_merge$AVLAND 




# Z_scale to prepare for feature selection/dimensionality reduction

str(data_pca_merge)

data_pca_z = data_pca_merge
for ( i in 1:ncol(data_pca_z)) {
  mean_pca_z = mean(data_pca_z[,i])
  sd_pca_z = sd(data_pca_z[,i])
  data_pca_z[,i] = ((data_pca_z[,i] - mean_pca_z) / sd_pca_z)
}

View(data_pca_z)

#Different approach
data_pca_z[] <- lapply(data_pca_merge, function(x) if(is.numeric(x)){
  scale(x, center=TRUE, scale=TRUE)
} else x)


data_pca_z_input <- data_pca_z


#PArt 3 reducing dimensionality through prcomp function
?prcomp
data_pca_result <- prcomp(data_pca_z, scale. = T) 
plot(data_pca_result)
sd <- data_pca_result$sdev
loadings <- data_pca_result$rotation
rownames(loadings) <- colnames(data_pca_result)
scores <- data_pca_result$x


head(data_pca_result$rotation)

data_pca_result$x

data_pca_result_1 <- data_pca_result$x #real scores of the entire dataset 
data_pca_result_11 <- data_pca_result$rotation

View(data_pca_result_1)


#Extracting variance of the PCAs looking at the variance

variance_data_pca_result1 <- data_pca_result$sdev^2  #PCAs Dataset

Eigenvalues <- eigen(cor(data_pca_z))$values   #Normalized dataset #checking if both match

variance_data_pca_result1[1:13]
Eigenvalues[1:13]


#Keep the highest PCAs with the highest variance

data_pca_result_merge <- rbind(data_pca_result$x,data_pca_result$sdev^2)

data_pca_result_merge[52,]


View(data_pca_result_merge)

rownames(data_pca_result_merge)[length(rownames(data_pca_result_merge))]<- "Variance"

View(data_pca_result_merge)

new_data_pca_result <- data_pca_result_merge[, c(1:8)] #keeping only the top 8 with the highest variance

new_data_pca_result11 <- data_pca_result_1[,c(1:8)]


#Check if they match

#Part 4 z scale the reduced variable( the PCs) from the top 8 PCs



new_data_pca_result_z = new_data_pca_result
for ( i in 1:ncol(new_data_pca_result_z)) {
  mean_pca_z = mean(new_data_pca_result_z[,i])
  sd_pca_z = sd(new_data_pca_result_z[,i])
  new_data_pca_result_z[,i] = ((new_data_pca_result_z[,i] - mean_pca_z) / sd_pca_z)
}


##Optional dont need to run this 

length(rownames(new_data_pca_result_z))

new_data_pca_result_z <- new_data_pca_result_z[-52,]

View(new_data_pca_result_z)

write.csv(new_data_pca_result_z , 'new_data_pca_result_z.csv')

#Part 5 Fraud score Calculation


#First score function

# For fraud purposes m = 2

m = 2

str(new_data_pca_result_z)


# fraud_score1 <- apply(abs(new_data_pca_result_z)^m, 
#                       function(x) sum) ^ (1/m)

fraud_score1 <- apply(abs(scale(new_data_pca_result_z))^m,1,sum)^(1/m)

fraud_score1 <- data.frame(fraud_score1)

View(fraud_score1)
summary(fraud_score1)


fraud_score2 <- (fraud_score1 - min(fraud_score1)) / (max(fraud_score1)  - min(fraud_score1))

View(fraud_score2)

ggplot(fraud_score2, aes(x = fraud_score1, y = ..density.. ))+ 
  geom_histogram(fill = "dodgerblue3" , colour = "grey60",size=0.1,bins=10)+
  geom_line(stat='density',adjust=2)+
  xlab("Fraud Score")+
  ylab("") +
  ggtitle(" Fraud Score Histogram")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5))
  


##### START FROM HERE updated on Feb12nd, pca analysis, keep 8 pcs, the data_pac_z dataframe is created at the line189
library(psych)
fa.parallel(data_pca_z,fa="pc",n.iter=100) # this function is to decide the optimal numbers of PC, but I couldn't run it, so I choose 8.
  
data_pca_result2 = principal(data_pca_z,nfactors=8,score=TRUE)
data_pca_scores2=data.frame(data_pca_result2$scores)


data_pca_result2$scores

# z-scale the pc score again
data_pca_scores_z2 = data_pca_scores2
for ( i in 1:ncol(data_pca_scores2)) {
  mean_pca_z = mean(data_pca_scores2[,i])
  sd_pca_z = sd(data_pca_scores2[,i])
  data_pca_scores_z2[,i] = ((data_pca_scores_z2[,i] - mean_pca_z) / sd_pca_z)
}

#compute fraud score 11

m=2

fraud_score11 <- apply(abs(scale(data_pca_scores_z2))^m,1,sum)^(1/m)

fraud_score11 <- data.frame(fraud_score11)

View(fraud_score11)

fraud_score21 <- (fraud_score11 - min(fraud_score11)) / (max(fraud_score11)  - min(fraud_score11))


ggplot(fraud_score11, aes(x = fraud_score11, y = ..density.. ))+ 
  geom_histogram(fill = "dodgerblue3" , colour = "grey60",size=0.1,bins=10)+
  geom_line(stat='density',adjust=2)+
  xlab("Fraud Score")+
  ylab("") +
  ggtitle(" Fraud Score Histogram")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5))+
  scale_x_continuous( breaks = seq(0,1,0.00001))


#Updated february 12,2018. Computing fraud_score 111


data_pca_result <- prcomp(data_pca_z, scale. = T) 
data_pca_result_1 <- data_pca_result$x

new_data_pca_result11 <- data_pca_result_1[,c(1:8)]

new_data_pca_result_z1 = new_data_pca_result11
for ( i in 1:ncol(new_data_pca_result_z1)) {
  mean_pca_z = mean(new_data_pca_result_z1[,i])
  sd_pca_z = sd(new_data_pca_result_z1[,i])
  new_data_pca_result_z1[,i] = ((new_data_pca_result_z1[,i] - mean_pca_z) / sd_pca_z)
}


fraud_score111 <- apply(abs(scale(new_data_pca_result_z1))^m,1,sum)^(1/m)

fraud_score111 <- data.frame(fraud_score111)

View(fraud_score111)



### Visualizing the fraud score: frauds_core11 (principal function) and fraud_score111 (prcomp function)
# Updated February 13, 2018

View(new_data_pca_result)
library(reshape)


library(ggfortify)
#Visualizing PCA1 vs PCA2
autoplot(prcomp(data_pca_merge))


ggplot(data = fraud_score111, aes(x = fraud_score111)) +
  geom_histogram(bins = 100, fill = "dodgerblue3" , col = "black") +
  coord_cartesian(xlim=seq(1e-02,1,0.01)) +
  scale_x_log10() +
  ggtitle("Fraud Score 111 of PCA") +
  xlab("Fraud Score") +
  ylab("count") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))+
  theme_classic()
#maybe trying to rescale to range 0 and 1 because nax values is 1385.5364

summary(fraud_score11)

fraud_score11_autoencoder <- (fraud_score11 - min(fraud_score11)) / (max(fraud_score11)  - min(fraud_score11))
summary(fraud_score11_autoencoder)

ggplot(data = fraud_score11_autoencoder, aes(x = fraud_score11_autoencoder)) +
  geom_histogram(bins = 100, fill = "dodgerblue3" , col = "black") +
  scale_x_log10() +
  ggtitle("Outlier detection via z-scores: Fraud Score of Autoencoder") +
  xlab("Fraud Score") +
  ylab("count") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))+
  theme_classic()




#Not sure if this is correct to plot PCA
library(factoextra)
library(FactoMineR)
nyp_pca <- PCA(data_pca_merge, scale.unit = T, graph = F)

fviz_pca_ind(nyp_pca,
             label = "none", # hide individual labels
        # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)



###### AUTOENCODER Februrary 13

#There is a package called h2o for autoencoder ( deep learning)
# learned from this demo : https://shiring.github.io/machine_learning/2017/05/01/fraud

library(h2o)

?h2o.init()

local <- h2o.init()


#Splitting the data into two sets: training set and test sets.
#Checking how a pre trained model perform. 


features  <- colnames(data_pca_scores2)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)

train_unsupervised <-as.h2o(data_pca_scores2, destination_frame = "midata")


model_autoencoder <-h2o.deeplearning(x = features, training_frame = train_unsupervised,
                               autoencoder = TRUE,
                               reproducible = F)
                        


model_autoencoder
#getting each MSE of each records
model_anon = h2o.anomaly(model_autoencoder, train_unsupervised, per_feature=F)

model_anon_error <- as.data.frame(model_anon)
#putting in the highest first (descending order of the error)

names(model_anon_error)
#plotting the 8 PCAs 

ggplot(data = model_anon_error, aes(x = Reconstruction.MSE)) + 
  geom_histogram(bins = 100, fill = "dodgerblue3" , col = "black") + 
  scale_x_log10() +
  scale_y_sqrt() + 
  ggtitle("Distribution of MSE with Autoencoder")+
  theme_classic()+
  scale_y_continuous(breaks = seq(0,100000,20000))

#Different visualization:

model_anon1 = h2o.anomaly(model_autoencoder, train_unsupervised, per_feature=T)

model_anon_error1 <- as.data.frame(model_anon1)

plot(model_anon_error1$reconstr_RC2.SE)
plot(model_anon_error1$reconstr_RC7.SE)
plot(model_anon_error1$reconstr_RC8.SE)

#Other visualization of autoencoder

splits <- h2o.splitFrame(train_unsupervised, 
                         ratios = c(0.4, 0.4), 
                         seed = 42)

train_unsupervised3  <- splits[[1]]
train_supervised  <- splits[[2]]
test <- splits[[3]]

response <- "Class"
features1 <- setdiff(colnames(train_unsupervised), response)


model_nn <- h2o.deeplearning(x = features1,
                             training_frame = train_unsupervised3,
                             model_id = "model_nn",
                             autoencoder = TRUE,
                             reproducible = TRUE, #slow - turn off for real problems
                             ignore_const_cols = FALSE,
                             seed = 42,
                             hidden = c(10, 2, 10), 
                             epochs = 100,
                             activation = "Tanh")

library(RCurl)

anomaly <- h2o.anomaly(model_nn, test) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()


mean_mse <- anomaly %>%
  summarise(mean = mean(Reconstruction.MSE))


#Visualizing differently
ggplot(anomaly, aes(x = as.numeric(rowname), y = Reconstruction.MSE, col = "black")) +
  geom_point(alpha = 0.3, col = "dodgerblue3") +
  geom_hline(data = mean_mse, aes(yintercept = mean))+ 
  labs(x = "instance number")+
  scale_y_log10()+
  theme_classic()


# As we can see in the plot, there is no perfect classification into fraud and non-fraud cases 
 #but the mean MSE is definitely higher for fraudulent transactions than for regular ones.

# Explanation: 
# An autoencoder is trying to learn a nonlinear, reduced representation of the original data.
# It is an unsupervised approach, so it will only consider the features of the data. It is not an approach for classification.
# The mean square error is a way to see how hard it is for the autoencoder to represent the output.
# Anomalies are considered rows/observations with high mean squared error.
# In our case, the rows with the highest MSE should be considered anomalous. 



##### Calculing the fraud score number 2 



#the fraud score is any measure of difference between the original input record and the autoencoder output record.

#testing which formula 
# https://cran.r-project.org/web/packages/autoencoder/autoencoder.pdf
library(autoencoder)
# Set up the autoencoder architecture:
nl=3                          ## number of layers (default is 3: input, hidden, output)
unit.type = "logistic"        ## specify the network unit type, i.e., the unit

# activation function ("logistic" or "tanh")
Nx.patch=10                   ## width of training image patches, in pixels
Ny.patch=10                   ## height of training image patches, in pixels
N.input = Nx.patch*Ny.patch  ## number of units (neurons) in the input layer (one unit per pixel)
N.hidden = 10*10              ## number of units in the hidden layer
lambda = 0.0002               ## weight decay parameter
beta = 6                      ## weight of sparsity penalty term
rho = 0.01                    ## desired sparsity parameter
epsilon <- 0.001              ## a small parameter for initialization of weights
# as small gaussian random numbers sampled from N(0,epsilon^2)
max.iterations = 2000         ## number of iterations in optimizer

data_pca_scores22 <- as.matrix(data_pca_scores2)

autoencoder.object <- autoencode(X.train=data_pca_scores22,nl=nl,N.hidden=N.hidden,
                                 unit.type=unit.type,lambda=lambda,beta=beta,rho=rho,epsilon=epsilon,
                                 optim.method="BFGS",max.iterations=max.iterations,
                                 rescale.flag=TRUE,rescaling.offset=0.001) #If you run, know that it will take a long time ( ~6-8hours)

autoencoder_output <- predict(autoencoder.object, X.input=data_pca_scores22, hidden.output=FALSE)$X.output

m = 2
?predict()

autoencoder_dist <- abs(autoencoder_output - data_pca_scores22)^m

autoencoder_fraudscore <- apply(autoencoder_dist, 1, sum)^(1/m)

autoencoder_fraudscore <- data.frame(autoencoder_fraudscore)

summary(autoencoder_fraudscore)


#rescaling it for range 0 to 1
autoencoder_fraudscore2 <- (autoencoder_fraudscore - min(autoencoder_fraudscore)) / (max(autoencoder_fraudscore)  - min(autoencoder_fraudscore))


summary(autoencoder_fraudscore2)
ggplot(data = autoencoder_fraudscore2, aes(x = autoencoder_fraudscore2)) +
  geom_histogram(bins = 100, fill = "dodgerblue3" , col = "black") +
  ggtitle("Autoencoder Error: Fraud Score of Autoencoder") +
  xlab("Fraud Score") +
  ylab("count") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))+
  theme_classic()+
  scale_x_log10()
  
  


### February 15th comparing the two scores: fraud_score11 and autoencoder_fraudscore2. lets compare the top 20 records 


# First Fraud Score 
outlierdetection_fraudscore11 <- data.frame(cbind(record_id=row.names(fraud_score11_autoencoder),fraud_score11_autoencoder))
outlierdetection_fraudscore11_top20 <- outlierdetection_fraudscore11 %>%
                                        arrange(-fraud_score11) %>%
                                        slice(1:20)

# Second Fraud Score 
autoencoder_fraudscore2_dataframe <- data.frame(cbind(record_id=row.names(autoencoder_fraudscore2),autoencoder_fraudscore2))

autoencoder_fraudscore2_top20 <- autoencoder_fraudscore2_dataframe %>%
                                  arrange(-autoencoder_fraudscore) %>%
                                  slice(1:20)



  

