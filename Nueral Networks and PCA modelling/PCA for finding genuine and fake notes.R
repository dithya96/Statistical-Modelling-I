#########################################################################################
## Homework Assignment 4 - Question 5
## Dithya Sridharan
## Class Number : 48
## Edited: Novemeber 24, 2018
#########################################################################################

########################################################################################
##Loading the libraries
#######################################################################################

install.packages('mclust')
install.packages('ggfortify')
library(mclust)
library(ggfortify)
rm(list=ls())




data(banknote)
?banknote
head(banknote)
summary(banknote)
str(banknote)
ds= banknote

#ds$Status = as.numeric(ds$Status)
ds = ds[,2:7]
str(ds)
summary(ds)
pca_out=prcomp(ds, scale=TRUE)

summary(pca_out)

names(pca_out)
pca_out$center
pca_out$scale
pca_out$rotation

quartz()
autoplot(pca_out, shape=2 , colour=ds$Status, loadings = TRUE, loadings.colour='Orange', loadings.label = TRUE, loadings.label.size = 3)
autoplot(pca_out,x=2,y=3, shape=2 , colour=ds$Status, loadings = TRUE, loadings.colour='Orange', loadings.label = TRUE, loadings.label.size = 3)
pca_var = pca_out$sdev^2
pve = pca_var/sum(pca_var)
plot(pve,xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b')
plot(cumsum(pve),xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b')
pve
cumsum(pve)

##################################
## Genuine Notes
##################################
ds_genuine= banknote[banknote$Status=='genuine',2:7]

pca_out_genuine=prcomp(ds_genuine, scale=TRUE)

quartz()
autoplot(pca_out_genuine, shape=2 , loadings = TRUE, loadings.colour='Orange', loadings.label = TRUE, loadings.label.size = 3,main='PCA for Genuine Notes, PC1 and PC2')
autoplot(pca_out_genuine,x=2,y=3, shape=2 , loadings = TRUE, loadings.colour='Orange', loadings.label = TRUE, loadings.label.size = 3,main='PCA for Genuine Notes, PC2 and PC3')
pca_var_genuine = pca_out_genuine$sdev^2
pve_genuine = pca_var_genuine/sum(pca_var_genuine)
plot(pve_genuine,xlab="Principal Component", ylab="Proportion of Variance Explained ", main='PVE for Genuine Notes', ylim=c(0,1),type='b')
plot(cumsum(pve_genuine),xlab="Principal Component", ylab="Proportion of Variance Explained ",main='Cummulative PVE for Genuine Notes', ylim=c(0,1),type='b')

##################################
## Counterfeit Notes
##################################

ds_counterfeit= banknote[banknote$Status != 'genuine',2:7]

pca_out_counterfeit=prcomp(ds_counterfeit, scale=TRUE)

quartz()
autoplot(pca_out_counterfeit, shape=2 , loadings = TRUE, loadings.colour='Orange', loadings.label = TRUE, loadings.label.size = 3,main='PCA for counterfeit Notes, PC1 and PC2')
autoplot(pca_out_counterfeit,x=2,y=3, shape=2 , loadings = TRUE, loadings.colour='Orange', loadings.label = TRUE, loadings.label.size = 3,main='PCA for counterfeit Notes, PC2 and PC3')
pca_var_counterfeit = pca_out_counterfeit$sdev^2
pve_counterfeit = pca_var_counterfeit/sum(pca_var_counterfeit)
plot(pve_counterfeit,xlab="Principal Component", ylab="Proportion of Variance Explained ", main='PVE for Counterfeit Notes', ylim=c(0,1),type='b')
plot(cumsum(pve_counterfeit),xlab="Principal Component", ylab="Proportion of Variance Explained ",main='Cummulative PVE for Counterfeit Notes', ylim=c(0,1),type='b')
