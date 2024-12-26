#_________________________Aguamiel__________________________________#

agua=read.csv("Compendio final de resultados promedio de aguamiel actualizado 12 abr.csv")
agua

estacion=row.names(agua)
estacion

#We can easily visualize the name of each X sampling region. For aguamiel,
#semilla or starter and joven analyses the real sampling associated with these numbers are
#located in the Relacion de numeros latinos asociados a muestreos para R Studio.xlsx
#document. In this case, the sampling facility code is associated with the following
#latin numbers given by R: 1=4.1, 2=1.1, 3=2.1, 4=3.1, 5=3.2, 6=2.2, 7=5.1,
#8=5.2, 9=4.2 and 10=1.2. Same inital humber (prior to the point), indicate a same
#sampling region (as can be further confirmed in Table 1 in the proposed article or
#in the GitHub description).

names(agua)
#to see the head of the data set columns. We are analyzing 6 features or variables.

apply(agua, 2, mean)
#apply allows us to apply a certain function (mean in this case); 1 will be used
#if we are trying to compute the mean of the rows and 2 for computing the mean of the columns.
##WATCH OUT: the means are vastly different=WE NEED MEAN ZERO AND VARIANCE 1 TO 
#PROCEED WITH PCA

apply(agua, 2, var)
#Let´s check out the variance of each feature/variable. 
#They vary greatly too.

#Now is important to clarify that the units are different among the
#features/factors/variables studied.

#Therefore, first, we need to scale; if not, most of the principal components
#would be driven by the "Brix" feature, since it has the greatest mean and variance.

#Let´s standardize by adjusting mean zero and standard deviation 1 with the following command:

pr.out=prcomp(agua, scale=TRUE)
#Now the variables have mean zero by the prcomp comand and standard deviation (S.D.) 
#1 with scale=TRUE.

names(pr.out)
#center and scale refer to the means and S.D. of the variables used for scaling prior
#to PCA .

#The rotation matrix provides the PCA loadings, each column contains the corresponding 
#principal component loading vector.

#The X refers to the principal component score vectors.

pr.out$center

pr.out$scale

pr.out$rotation

#We got 6 PC, which is expected as there will be min(n-1,p) principal components for
#observations (n) and features (p).

#In this case, PC1 places most of its weight on viscosity, acidity, and ph and the
#first two are correlated; while PC2 places most of its weight on yeasts, lactic acid bacteria
#and brix, both yeasts and brix also correlated.

#pH is inversely correlated with acidity, which is expected.
#Yeasts and lactic acid bacteria are inversely correlated.

#Let´s plot the first two Principal Components (PC):

biplot(pr.out, scale=0)
#scale=0 ensures the arrows are scaled to represent the loadings.
#We can change the sign of the PCs without any relevant consequence, but is not 
#entirely necessary here...

#pr.out$rotation=-pr.out$rotation
#pr.out$x=-pr.out$x
#biplot(pr.out, scale=0)

#The prcomp function also gives us the S.D. of each P.C., being those:

pr.out$sdev

#The variance explained by each P.C. is obtained by squaring:

pr.var=pr.out$sdev^2
pr.var

#Now, the proportion of variance explained by each P.C. is obtained after dividing
#the variance explained by each P.C. by the total variance explained by the 6 P.C.s:

pve=pr.var/sum(pr.var)
pve

#Then, the 1st P.C. explains 49.5% of the variance in the data, the 2nd the 27.2% and so forth.

#Finally, we can plot the PVE explained by each P.C., as well as the cumulative PVE:

par(mfrow=c(1,2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type="b")
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1), type="b")
#cumsum computes the cumulative sum of the elements of a numeric vector. E.g. a=c(1,2,8,-3)
#cumsum(a)=1 3 11 8


#Returning to our analysis, above we have our scree plot to figure out the optimal number of
#P.C.s we'll require.In this case, two should suffice (they both explain approx. 80% of the variance in data).

##########CLUSTERING BY K-MEANS#########

library(ggplot2)

clsagua=pr.out$x[,1:2]
#We select the first two score vectors of each of the two main Principal
#Components!! Then, we can proceed with k-means clustering!!!

clsagua

set.seed(1)
wcss <- vector()
for(i in 1:9){
  wcss[i] <- sum(kmeans(clsagua, i)$withinss)
}

wcss

ggplot() + geom_point(aes(x = 1:9, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:9, y = wcss), color = 'blue') + 
  ggtitle("Elbow Method for Aguamiel k-means") + 
  xlab('Number of centroids k') + 
  ylab('WCSS')

#We´d need to use 6 or 7 centroids...Or maybe 5? Let's check out...

#It means that only a certain amount of samples group together, meaning
#most of the aguamiel sampled is unique...

#Let's try 5 clusters...

set.seed(200)
km.out=kmeans(clsagua, 5, iter.max=1000, nstart=50)
#Here, we specify to work with only 5 clusters. It is suggested to use nstar=20 or 50, and 
##iter.max refers to the maximum iterations to apply to the algorithm and 
#nstart to the amount of centroid groups that are being employed internally or
#multiple initial cluster assignments (20 or 50).
#The last consideration will allow us to find a global optimum, rather than a local one,
#measured in the reduction of within cluster sumed squares, as the nstart value increases.

km.out$cluster
#Here we check the assignments of the n observations. We see how the data is sepa-
#rated into distinctive groups (as expected).

km.out
#We can see the sumary of the clustering run...

km.out$tot.withinss

#Now, we can plot our results...

plot(clsagua, col=(km.out$cluster +1), main="K.Means Clustering with K=5", xlab="Principal Component 1 (49.5%)", ylab="Principal Component 2 (27.2%)", pch=20, cex=2)

#Let´s try to incorporate labels to the clustering plot:

text(clsagua, row.names(agua), cex=0.6, pos=4, col="red")


#Now, 6 clusters...

set.seed(2)
km.out=kmeans(clsagua, 6, iter.max=1000, nstart=50)
#Here, we specify to work with only 6 clusters. It is suggested to use nstar=20 or 50, and 
##iter.max refers to the maximum iterations to apply to the algorithm and 
#nstart to the amount of centroid groups that are being employed internally or
#multiple initial cluster assignments (20 or 50).
#The last consideration will allow us to find a global optimum, rather than a local one,
#measured in the reduction of within cluster sumed squares, as the nstart value increases.

km.out$cluster
#Here we check the assignments of the n observations. We see how the data is sepa-
#rated into distinctive groups (as expected).

km.out
#We can see the sumary of the clustering run...

km.out$tot.withinss

#Now, we can plot our results...

plot(clsagua, col=(km.out$cluster +1), main="K.Means Clustering with K=6", xlab="Principal Component 1 (49.5%)", ylab="Principal Component 2 (27.2%)", pch=20, cex=2)

#Let´s try to incorporate labels to the clustering plot:

text(clsagua, row.names(agua), cex=0.6, pos=4, col="red")


#Let´s try 7 clusters...

set.seed(3)
km.out=kmeans(clsagua, 7, iter.max=1000, nstart=50)
#Here, we specify to work with only 7 clusters. It is suggested to use nstar=20 or 50, and 
##iter.max refers tot the maximum iterations to apply to the algorithm and 
#nstart to the amount of centroid groups that are being employed internally or
#multiple initial cluster assignments (20 or 50).
#The last consideration will allow us to find a global optimum, rather than a local one,
#measured in the reduction of within cluster sumed squares as the nstart value increases.

km.out$cluster
#Here we check the assignments of the n observations. We see how the data is sepa-
#rated into distinctive groups (as expected).

km.out
#We can see the sumary of the clustering run...

km.out$tot.withinss

#Now, we can plot our results...

plot(clsagua, col=(km.out$cluster +1), main="K.Means Clustering with K=7", xlab="Principal Component 1", ylab="Principal Component 2", pch=20, cex=2)

text(clsagua, row.names(agua), cex=0.6, pos=4, col="red")

#Based on the WCSS calculated on each clustering and on the evidence the Elbow method,
#shows, the difference on the wcss between 5 or 6 clusters is aprox. 1.4 and
#between 6 or 7 clusters is 0.535. Therefore, 
#6 clusters could describe/group the data properly. However, as stated by James 
#et al. (2023), analyst's criteria should also be involved in selecting the number
#of clusters; therefore, 5 clusters could suffice since: the difference in WCSS 
#compared to using 6 or 7 clusters is less than 1.5 and 2) the associations observed
#with 5 clusters are relatively expected (some regions that use the same Agave to
#obtain aguamiel group together).

#WATCH OUT: As stated by James et al. (2023), the difference on the WCSS
#values we see between the Elbow method and the k-means estimation per se
#is due to the nstart value of 50 we use on the clustering calculation. In 
#other words and as can be seen on each determination,
#the actual WCSS is even lower than that calculated on the Elbow method,
#which is precisely what the method looks for: minimize the WCSS. More
#iterations translate into the better detection of a global rather than a local
#optimum and it can be appreciated on how the WCSS gets lower on its value :).

#_#_#_#_#_____________________#######_____________________________#_#_#_#_#

#IMPORTANT: Always clear your environment prior to running another sample's code :)

#_____________________Pulque semilla or fermentation starter__________________#
sem=read.csv("Compendio final de resultados promedio de semilla actualizado 12 abr.csv")
sem
#IMPORTANT: on reducing sugars, there are three values calculated as less than, rather
#than an entire number. It is ok, since we report the last quantifiable amount.

estacion=row.names(sem)
estacion

##We can easily visualize the name of each X sampling region. For aguamiel,
#semilla or starter and joven analyses the real sampling associated with these numbers are
#located in the Relacion de numeros latinos asociados a muestreos para R Studio.xlsx
#document. In this case, the sampling facility code is associated with the following
#latin numbers given by R: 1=4.1, 2=1.1, 3=2.1, 4=3.1, 5=3.2, 6=2.2, 7=5.1,
#8=4.2 and 9=1.2. Same initial number (prior to the point), indicates a same
#sampling region (as can be further confirmed in Table 1 in the proposed article or 
#the description in GitHub).

names(sem)
#to see the head of the dataset columns

apply(sem, 2, mean)
#apply allows us to apply a certain function (mean in this case); 1 will be used
#if we are trying to compute the mean of the rows and 2 for computing the mean of the columns.
##WATCH OUT: the means are vastly different=WE NEED MEAN ZERO AND VARIANCE 1 TO 
#PROCEED WITH PCA

apply(sem, 2, var)
#Let´s check out the variance of each feature/variable. 
#They vary greatly too.

#Now is important to clarify that the units are also different among the 
#features.

#Therefore, first, we need to scale; if not, most of the principal components
#would be driven by the "Yeasts", "brix", and "Lacticbacteria" features, since they have 
#one of the greatest means and variances.

#Let´s standardize by adjusting mean zero and standard deviation 1 with the following command:

pr.out=prcomp(sem, scale=TRUE)
#Now the variables have mean zero by the prcomp comand and standard deviation (S.D.) 
#1 with scale=TRUE.

names(pr.out)
#center and scale refer to the means and S.D. of the variables used for scaling prior
#to PCA .

#The rotation matrix provides the PCA loadings, each column contains the corresponding 
#principal component loading vector.

#The X refers to the principal component score vectors.

pr.out$center

pr.out$scale

pr.out$rotation

#We got 7 PCs, which is expected as there will be min(n-1,p) principal components for
#observations (n) and features (p).

#In this case, PC1 places most of its weight on viscosity, brix, and lacticbacteria 
#and they are positively correlated; while PC2 places most of its weight
#on yeasts and ph, and acidity, all also correlate; reducingcarbs is also relevant in this 
#component, but is inversely correlated with the other variables on this PC.

#On PC2, although ph and acidity seem to be positively correlated (an increase
#in acidity would yield an increase in pH, which is not true), is understandable
#since in the original data there is not a clear tendency associating the 
#lowest pH with the highest acidity (e.g. rows number 1 and 3 of the 
#original data set).


#Let´s plot the first two Principal Components (PC):

biplot(pr.out, scale=0)
#scale=0 ensures the arrows are scaled to represent the loadings.
#We can change the sign of the PCs without any relevant consequence, but we
#decide not to. Is clearer this way...

#pr.out$rotation=-pr.out$rotation
#pr.out$x=-pr.out$x
#biplot(pr.out, scale=0)

#The prcomp function also gives us the S.D. of each P.C., being those:

pr.out$sdev

#The variance explained by each P.C. is obtained by squaring:

pr.var=pr.out$sdev^2
pr.var

#Now, the proportion of variance explained by each P.C. is obtained after dividing
#the variance explained by each P.C. by the total variance explained by the 7 P.C.s:

pve=pr.var/sum(pr.var)
pve

#Then, the 1st P.C. explains 38.3% of the variance in the data, the 2nd the 23.4% and so forth.

#Finally, we can plot the PVE explained by each P.C., as well as the cumulative PVE:

par(mfrow=c(1,2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type="b")
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1), type="b")
#cumsum computes the cumulative sum of the elements of a numeric vector. E.g. a=c(1,2,8,-3)
#cumsum(a)=1 3 11 8


#Returning to our analysis, above we have our scree plot to figure out the optimal number of
#P.C.s we'll require.In this case, two should suffice (they both explain approx. 61.7% of the variance in data).

##########CLUSTERING BY K-MEANS#########

library(ggplot2)

clsem=pr.out$x[,1:2]
#We select the first two score vectors of each of the two main Principal
#Components!! Then, we can proceed with k-means clustering!!!

clsem

set.seed(4)
wcss <- vector()
for(i in 1:8){
  wcss[i] <- sum(kmeans(clsem, i)$withinss)
}

wcss

ggplot() + geom_point(aes(x = 1:8, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:8, y = wcss), color = 'blue') + 
  ggtitle("Elbow Method for Fermentation Starter K-means") + 
  xlab('Number of Centroids k') + 
  ylab('WCSS')

#We´d need to use 4 or 5 centroids...

#It means that a certain amount of samples group together in couples.

set.seed(5)
km.out=kmeans(clsem, 4, iter.max=1000, nstart=50)
#Here, we specify to work with only 4 clusters. It is suggested to use nstar=20 or 50, and 
##iter.max refers tot the maximum iterations to apply to the algorithm and 
#nstart to the amount of centroid groups that are being employed internally or
#multiple initial cluster assignments (20 or 50).
#The last consideration will allow us to find a global optimum, rather than a local one,
#measured in the reduction of within cluster sumed squares, as the nstart value increases.

km.out$cluster
#Here we check the assignments of the n observations. We see how the data is sepa-
#rated into distinctive groups (as expected).

km.out
#We can see the sumary of the clustering run...

km.out$tot.withinss

#Now, we can plot our results...

plot(clsem, col=(km.out$cluster +1), main="K.Means Clustering with K=4", xlab="Principal Component 1 (38.3%)", ylab="Principal Component 2 (23.4%", pch=20, cex=2)

#Let´s try to incorporate labels to the clustering plot:

text(clsem, row.names(sem), cex=0.6, pos=4, col="red")


#Let´s try 5 clusters...

set.seed(6)
km.out=kmeans(clsem, 5, iter.max=1000, nstart=50)
#Here, we specify to work with only 5 clusters. It is suggested to use nstar=20 or 50, and 
##iter.max refers tot the maximum iterations to apply to the algorithm and 
#nstart to the amount of centroid groups that are being employed internally or
#multiple initial cluster assignments (20 or 50).
#The last consideration will allow us to find a global optimum, rather than a local one,
#measured in the reduction of within cluster sumed squares as the nstart value increases.

km.out$cluster
#Here we check the assignments of the n observations. We see how the data is sepa-
#rated into distinctive groups (as expected).

km.out
#We can see the sumary of the clustering run...

km.out$tot.withinss

#Now, we can plot our results...

plot(clsem, col=(km.out$cluster +1), main="K.Means Clustering with K=5", xlab="Principal Component 1", ylab="Principal Component 2", pch=20, cex=2)

text(clsem, row.names(sem), cex=0.6, pos=4, col="red")

#Based on the evidence the Elbow method shows and the WCSS calculated
#after each clustering, the difference on the wcss
#between 4 or 5 clusters is 1.029 (based on the WCSS presented on each k.means
#analysis rather than the Elbow method). Therefore, 
#4 clusters could describe/group the data properly.

#WATCH OUT: As stated by James et al. (2023), the difference on the WCSS
#values we see between the Elbow method and the k-means estimation per se
#is due to the nstart value of 50 we use on the clustering calculation. In 
#other words and as can be seen on each of the matrice's determinations,
#the actual WCSS is even lower than that calculated on the Elbow method,
#which is precisely what the method looks for: minimize the WCSS. More
#iterations translate into the better detection of a global rather than a local
#optimum and it can be appreciated on how the WCSS gets lower on its value :).

#_#_#_#_#_____________________#######_____________________________#_#_#_#_#

#IMPORTANT: Always clear your environment prior to running another sample's code :)

#______________________________Joven_____________________#
you=read.csv("Compendio final de resultados promedio de joven actualizado 12 abr.csv")
you
#IMPORTANT: on reducing sugars, one value is calculated as less than, rather
#than an entire number which is ok since we report the last quantifiable 
#concentration. On acacet, in all but the third row we report the detection limit
#of the technique.

estacion=row.names(you)
estacion

#We can easily visualize the name of each X sampling region. For aguamiel,
#semilla and joven analyses the real sampling associated with these numbers are
#located in the Relacion de numeros latinos asociados a muestreos para R Studio.xlsx
#document. In this case, the sampling facility code is associated with the following
#latin numbers given by R: 1=4.1, 2=1.1, 3=2.1, 4=3.1, 5=3.2, 6=2.2, 7=5.1,
#8=5.2, 9=4.2 and 10=1.2. Same initial number (prior to the point), indicates a same
#sampling region (as can be further confirmed in Table 1 in the proposed article 
#or the available description at GitHub).

names(you)
#to see the head of the dataset columns

apply(you, 2, mean)
#apply allows us to apply a certain function (mean in this case); 1 will be used
#if we are trying to compute the mean of the rows and 2 for computing the mean of the columns.
##WATCH OUT: the means are vastly different=WE NEED MEAN ZERO AND VARIANCE 1 TO 
#PROCEED WITH PCA

apply(you, 2, var)
#Let´s check out the variance of each feature/variable. 
#They vary greatly too.

#Now is important to clarify that the units are different among
#the features.

#Therefore, first, we need to scale; if not, most of the principal components
#would be driven by the "Brix", "Yeasts" and "Lacticbacteria" features, since they have one of
#the greatest means and variances.

#Let´s standardize by adjusting mean zero and standard deviation 1 with the following command:

pr.out=prcomp(you, scale=TRUE)
#Now the variables have mean zero by the prcomp comand and standard deviation (S.D.) 
#1 with scale=TRUE.

names(pr.out)
#center and scale refer to the means and S.D. of the variables used for scaling prior
#to PCA .

#The rotation matrix provides the PCA loadings, each column contains the corresponding 
#principal component loading vector.

#The X refers to the principal component score vectors.

pr.out$center

pr.out$scale

pr.out$rotation

#We got 9 PC, which is expected as there will be min(n-1,p) principal components for
#observations (n) and features (p).

#In this case, PC1 places most of its weight on viscosity, reducingcarbs, brix,
#and lacticbacteria and they are all positively correlated; while PC2 places most of 
#its weight on ethanol, yeasts, and acidity, all also correlate positively.

#On PC2, although ph and acidity seem to be positively correlated (an increase
#in acidity would yield an increase in pH, which is not true), is understandable
#since in the original data there is not a clear tendency associating the 
#lowest pH with the highest acidity (e.g. rows number 4 and 8 of the 
#original data set).


#Let´s plot the first two Principal Components (PC):

biplot(pr.out, scale=0)
#scale=0 ensures the arrows are scaled to represent the loadings.
#We don´t change the sign of the PCs in this case. There is no relevant consequence...

#The prcomp function also gives us the S.D. of each P.C., being those:

pr.out$sdev

#The variance explained by each P.C. is obtained by squaring:

pr.var=pr.out$sdev^2
pr.var

#Now, the proportion of variance explained by each P.C. is obtained after dividing
#the variance explained by each P.C. by the total variance explained by the 9 P.C.s:

pve=pr.var/sum(pr.var)
pve

#Then, the 1st P.C. explains 34.5% of the variance in the data, the 2nd the 25.4% and so forth.

#Finally, we can plot the PVE explained by each P.C., as well as the cumulative PVE:

par(mfrow=c(1,2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type="b")
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1), type="b")
#cumsum computes the cumulative sum of the elements of a numeric vector. E.g. a=c(1,2,8,-3)
#cumsum(a)=1 3 11 8


#Returning to our analysis, above we have our scree plot to figure out the optimal number of
#P.C.s we'll require. In this case, two may suffice (they both explain approx. 59.9% of the variance in data).


##########CLUSTERING BY K-MEANS#########

library(ggplot2)

clyou=pr.out$x[,1:2]
#We select the first two score vectors of each of the two main Principal
#Components!! Then, we can proceed with k-means clustering!!!

clyou

set.seed(7)
wcss <- vector()
for(i in 1:9){
  wcss[i] <- sum(kmeans(clyou, i)$withinss)
}

wcss

ggplot() + geom_point(aes(x = 1:9, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:9, y = wcss), color = 'blue') + 
  ggtitle("Elbow method for Pulque Joven K-means") + 
  xlab('Number of Centroids k') + 
  ylab('WCSS')

#In this case, is not entirely clear the amount of clusters
#we´d need to use. We can start trying out 5,6 and 7 centroids...

#It means that a certain amount of samples group together, but mostly remain unique
#which could relate to the time of fermentation some samples had, for example.

set.seed(8)
km.out=kmeans(clyou, 5, iter.max=1000, nstart=50)
#Here, we specify to work with only 5 clusters. It is suggested to use nstar=20 or 50, and 
##iter.max refers tot the maximum iterations to apply to the algorithm and 
#nstart to the amount of centroid groups that are being employed internally or
#multiple initial cluster assignments (20 or 50).
#The last consideration will allow us to find a global optimum, rather than a local one,
#measured in the reduction of within cluster sumed squares, as the nstart value increases.

km.out$cluster
#Here we check the assignments of the n observations. We see how the data is sepa-
#rated into distinctive groups (as expected).

km.out
#We can see the sumary of the clustering run...

km.out$tot.withinss

#Now, we can plot our results...

plot(clyou, col=(km.out$cluster +1), main="K.Means Clustering with K=5", xlab="Principal Component 1 (34.5%)", ylab="Principal Component 2 (25.4%)", pch=20, cex=2)

#Let´s try to incorporate labels to the clustering plot:

text(clyou, row.names(you), cex=0.6, pos=4, col="red")
#Maybe 5 clusters would be alright...it also has sense with reality and the WCSS value
#calculated here is also similar to that of the k-means clustering for the beverage's 
#volatile profile. Also, in Álvarez-Ríos et al. (2020) their results show that
#the pulques they sampled, even with different producing methods and fermentation times,
#show little dissimilarity among them (both in physical, chemical and microbiology diversity).

#Let´s try 6 clusters...

set.seed(9)
km.out=kmeans(clyou, 6, iter.max=1000, nstart=50)
#Here, we specify to work with only 6 clusters. It is suggested to use nstar=20 or 50, and 
##iter.max refers tot the maximum iterations to apply to the algorithm and 
#nstart to the amount of centroid groups that are being employed internally or
#multiple initial cluster assignments (20 or 50).
#The last consideration will allow us to find a global optimum, rather than a local one,
#measured in the reduction of within cluster sumed squares as the nstart value increases.

km.out$cluster
#Here we check the assignments of the n observations. We see how the data is sepa-
#rated into distinctive groups (as expected).

km.out
#We can see the sumary of the clustering run...

km.out$tot.withinss

#Now, we can plot our results...

plot(clyou, col=(km.out$cluster +1), main="K.Means Clustering with K=6", xlab="Principal Component 1 (34.5%)", ylab="Principal Component 2 (25.4%)", pch=20, cex=2)

text(clyou, row.names(you), cex=0.6, pos=4, col="red")

#Let´s try 7 clusters...

set.seed(10)
km.out=kmeans(clyou, 7, iter.max=1000, nstart=50)
#Here, we specify to work with only 7 clusters. It is suggested to use nstar=20 or 50, and 
##iter.max refers tot the maximum iterations to apply to the algorithm and 
#nstart to the amount of centroid groups that are being employed internally or
#multiple initial cluster assignments (20 or 50).
#The last consideration will allow us to find a global optimum, rather than a local one,
#measured in the reduction of within cluster sumed squares as the nstart value increases.

km.out$cluster
#Here we check the assignments of the n observations. We see how the data is sepa-
#rated into distinctive groups (as expected).

km.out
#We can see the sumary of the clustering run...

km.out$tot.withinss

#Now, we can plot our results...

plot(clyou, col=(km.out$cluster +1), main="K.Means Clustering with K=7", xlab="Principal Component 1", ylab="Principal Component 2", pch=20, cex=2)

text(clyou, row.names(you), cex=0.6, pos=4, col="red")

#Based on the evidence the Elbow method shows and on the WCSS values
#calculated on each clustering, the difference on the wcss
#between 5 or 6 clusters is 1.865, while between 6 and 7 is 0.784. Therefore, 7 clusters
#would be the logical answer, but as James et al. (2023) point out, the analyst
#criteria also should take part on the interpretation, and therefore 6 or even 5 clusters
#could explain properly the data given their particularities on the sampling. So,
#5-6 clusters could describe/group the data properly. 5 clusters are chosen. 

#WATCH OUT: As stated by James et al. (2023), the difference on the WCSS
#values we see between the Elbow method and the k-means estimation per se
#is due to the nstart value of 50 we use on the clustering calculation. In 
#other words and as can be seen on each of the matrice's determinations,
#the actual WCSS is even lower than that calculated on the Elbow method,
#which is precisely what the method looks for: minimize the WCSS. More
#iterations translate into the better detection of a global rather than a local
#optimum and it can be appreciated on how the WCSS gets lower on its value :).

#_#_#_#_#_____________________#######_____________________________#_#_#_#_#



