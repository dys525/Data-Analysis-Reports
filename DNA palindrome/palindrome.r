#our data
data <- read.table("hcmv.txt", header=TRUE)
head(data)


# create a stripplot of the uniform distribution of samples.
# indicates the total number of bases the sample could get from.
N <- 229354
# the number of sites that needed to be sampled.
n <- 296
gene <- seq(1, N)
set.seed(100)
# uniformply sample the distributions.
palindrome.distribution <- sample.int(N, size=n, replace=FALSE)
# create strip plot of the uniform distribution of samples
library(lattice)
stripplot(palindrome.distribution , psc=16, cex=0.25)



# create histogram to display random uniform scatter.
# number of samples needed will be 296.
n <- 296
# the bases can go from 1 to 229,354.
sample <- runif(n, min=1, max=229354)
# create a histogram of the uniform distribution.
hist(sample, breaks=50, probability=TRUE, col="grey", main="Uniform␣
,→Distribution of Palindromes")
# create a theoretical density line
lines(density(sample), col="blue")


# create histogram of HCMV datafile given.
data <- read.table("hcmv.txt", header=TRUE)
hist(data$location, breaks=30)

#initial data of palindromes
locations <- data$location
locations
length(locations)

#histogram of the first type of spacing
first_type_spacing = diff(locations)
hist(first_type_spacing)

# Sum of consecutive pairs
one_pair = head(locations,-1) + locations[-1]
one_pair

#Histogram of second type of spacing
second_type_spacing = diff(one_pair)
hist(second_type_spacing)


#Sum of consecutive triplets
one_back = locations[-1]
triplet = head(locations, -2) + locations[-1] + one_back[-1]
triplet = head(triplet, -1)
triplet

#Histogram of third type of spacing
hist(diff(triplet))


n <- 296
# the bases can go from 1 to 229,354.
random <- runif(n, min=1, max=229354)
# create a dataframe from the randomly generated dataset.
random.matrix <- c(random)
random.table <- as.table(random.matrix)
random.df <- as.data.frame(random.table)
random.df$Var1 <- NULL
random.df
#- spacing_between_palindrome_RANDOM
# create histogram of random data.
freq <- random.df$Freq
# spacing_between_palindrome of original histogram.
spacing_between_palindrome_RANDOM <- diff(freq)
hist(spacing_between_palindrome_RANDOM, breaks=40)
#- spacing_between_consecutive_RANDOM
# create histogram of random data.
freq <- random.df$Freq
one_pair = head(freq,-1) + freq[-1]
# spacing_between_sum_of_consecutive_spacings
spacing_between_consecutive_RANDOM <- diff(one_pair)
hist(spacing_between_consecutive_RANDOM, breaks=30)
#- spacing_between_triplets_RANDOM
# create histogram of random data.
freq <- random.df$Freq
one_back = freq[-1]
triplet_spacing = head(freq, -2) + freq[-1] + one_back[-1]
triplet = head(triplet_spacing, -1)
# spacing_between_triplets_RANDOM
spacing_between_triplets_RANDOM <- diff(triplet)
hist(spacing_between_triplets_RANDOM, breaks=30)



N <- 229354
n <- 296
gene <- seq(1,N)
set.seed(100)
gene.ind <- 100000:103000
gene.weight <- rep(1, N)
gene.weight[gene.ind] <- 50
set.seed(100)

regionsplit <- function(n.region, gene, site){
count.int <- table(cut(site, breaks = seq(1, length(gene), length.out=n.
,→region+1), include.lowest=TRUE))
count.vector <- as.vector(count.int)
count.tab <- table(factor(count.vector,levels=0:max(count.vector)))
return (count.tab)
}

n.region1 <- 20
O1.tab = regionsplit(n.region1, gene, data[['location']])
O1.tab

n.region2 <- 50
O2.tab = regionsplit(n.region2, gene, data[['location']])
O2.tab

n.region3 <- 300
O3.tab = regionsplit(n.region3, gene, data[['location']])
O3.tab

trunc=9
lvls=factor(c(0:(trunc-1),paste(">=",trunc,sep="")),levels=c(0:
,→(trunc-1),paste(">=",trunc,sep="")))
O1=as.vector(O1.tab)
O2=as.vector(O2.tab)
O3=as.vector(O3.tab)
O1.trunc=c(O1[1:trunc],sum(O1[-(1:trunc)]))
O2.trunc=c(O2[1:trunc],sum(O2[-(1:trunc)]))
O3.trunc=c(O3[1:trunc],sum(O3[-(1:trunc)]))
lambda1=n/n.region1
lambda2=n/n.region2
lambda3=n/n.region3
p1=c(dpois(0:(trunc-1),lambda1),1-sum(dpois(0:(trunc-1),lambda1)))
p2=c(dpois(0:(trunc-1),lambda2),1-sum(dpois(0:(trunc-1),lambda2)))
p3=c(dpois(0:(trunc-1),lambda3),1-sum(dpois(0:(trunc-1),lambda3)))
E1=p1*n.region1
E2=p2*n.region2
E3=p3*n.region3
tab1=data.frame(levels=lvls,Observed=O1.trunc,Expected=E1)
tab2=data.frame(levels=lvls,Observed=O2.trunc,Expected=E2)
tab3=data.frame(levels=lvls,Observed=O3.trunc,Expected=E3)
tab1
tab2
tab3

chisq.test(O1.trunc,p=p1,simulate.p.value=TRUE)
chisq.test(O2.trunc,p=p2,simulate.p.value=TRUE)
chisq.test(O3.trunc,p=p3,simulate.p.value=TRUE)


trunc=7
lvls=factor(c(0:(trunc-1),paste(">=",trunc,sep="")),levels=c(0:
,→(trunc-1),paste(">=",trunc,sep="")))
O1=as.vector(O1.tab)
O2=as.vector(O2.tab)
O3=as.vector(O3.tab)
O1.trunc=c(O1[1:trunc],sum(O1[-(1:trunc)]))
O2.trunc=c(O2[1:trunc],sum(O2[-(1:trunc)]))
O3.trunc=c(O3[1:trunc],sum(O3[-(1:trunc)]))
lambda1=n/n.region1
lambda2=n/n.region2
lambda3=n/n.region3
p1=c(dpois(0:(trunc-1),lambda1),1-sum(dpois(0:(trunc-1),lambda1)))
p2=c(dpois(0:(trunc-1),lambda2),1-sum(dpois(0:(trunc-1),lambda2)))
p3=c(dpois(0:(trunc-1),lambda3),1-sum(dpois(0:(trunc-1),lambda3)))
E1=p1*n.region1
E2=p2*n.region2
E3=p3*n.region3
tab1=data.frame(levels=lvls,Observed=O1.trunc,Expected=E1)
tab2=data.frame(levels=lvls,Observed=O2.trunc,Expected=E2)
tab3=data.frame(levels=lvls,Observed=O3.trunc,Expected=E3)
tab1
tab2
tab3

chisq.test(O1.trunc,p=p1,simulate.p.value=TRUE)
chisq.test(O2.trunc,p=p2,simulate.p.value=TRUE)
chisq.test(O3.trunc,p=p3,simulate.p.value=TRUE)



n.region1 <- 20
O1.tab = regionsplit(n.region1, gene, data[['location']])
O1.tab
n.region2 <- 50
O2.tab = regionsplit(n.region2, gene, data[['location']])
O2.tab
n.region3 <- 70
O3.tab = regionsplit(n.region3, gene, data[['location']])
O3.tab

trunc=9
lvls=factor(c(0:(trunc-1),paste(">=",trunc,sep="")),levels=c(0:
,→(trunc-1),paste(">=",trunc,sep="")))
O1=as.vector(O1.tab)
O2=as.vector(O2.tab)
O3=as.vector(O3.tab)
O1.trunc=c(O1[1:trunc],sum(O1[-(1:trunc)]))
O2.trunc=c(O2[1:trunc],sum(O2[-(1:trunc)]))
O3.trunc=c(O3[1:trunc],sum(O3[-(1:trunc)]))
lambda1=n/n.region1
lambda2=n/n.region2
lambda3=n/n.region3
p1=c(dpois(0:(trunc-1),lambda1),1-sum(dpois(0:(trunc-1),lambda1)))
p2=c(dpois(0:(trunc-1),lambda2),1-sum(dpois(0:(trunc-1),lambda2)))
p3=c(dpois(0:(trunc-1),lambda3),1-sum(dpois(0:(trunc-1),lambda3)))
E1=p1*n.region1
E2=p2*n.region2
E3=p3*n.region3
tab1=data.frame(levels=lvls,Observed=O1.trunc,Expected=E1)
tab2=data.frame(levels=lvls,Observed=O2.trunc,Expected=E2)
tab3=data.frame(levels=lvls,Observed=O3.trunc,Expected=E3)
tab1
tab2
tab3


chisq.test(O1.trunc,p=p1,simulate.p.value=TRUE)
chisq.test(O2.trunc,p=p2,simulate.p.value=TRUE)
chisq.test(O3.trunc,p=p3,simulate.p.value=TRUE)


var.test(tab1[['Observed']], tab2[['Observed']])
var.test(tab2[['Observed']], tab3[['Observed']])



t.test(tab1[['Observed']], tab2[['Observed']], paired = FALSE, var.equal = TRUE)
t.test(tab2[['Observed']], tab3[['Observed']], paired = FALSE, var.equal =␣
,→FALSE)





