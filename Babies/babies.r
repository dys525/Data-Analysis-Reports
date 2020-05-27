# call the babies data
df <- read.table("babies.txt", header=TRUE)
head(df)

# call summary of the data
summary(df)

# filter the smokers and call the summary
smoker.ind <- which(df['smoke'] == 1)
df.smoker <- df[smoker.ind,]

# filter nonsmoker and unknown
nonsmoker.ind <- setdiff(rownames(df), smoker.ind)
df.nonsmoker <- df[nonsmoker.ind,]
# filter only 
nonsmoker_nonirr.ind <- which(df['smoke'] == 0)
df.smoker <- df[smoker.ind,]
df.nonsmoker_nonirr <- df[nonsmoker_nonirr.ind,]
print('with irregular data size')
nrow(df.nonsmoker)
print('without irregular data size')
nrow(df.nonsmoker_nonirr)

print("smoker")
summary(df.smoker)
print("non smoker with irregular")
summary(df.nonsmoker)
print("non smoker without irregular")
summary(df.nonsmoker_nonirr)

#drow boxplot
boxplot(bwt~smoke, df)

#draw each histogram
hist(df.smoker$bwt)
hist(df.nonsmoker$bwt)
hist(df.nonsmoker_nonirr$bwt)

smoker.sd = sd(df.smoker[,'bwt'])
smoker.skewness = 0
for (i in 1:nrow(df.smoker)){
    smoker.skewness = smoker.skewness+((df.smoker[i,'bwt']-114.1)/smoker.sd)**3
}
smoker.skew = smoker.skewness/nrow(df.smoker)
smoker.skew

nonsmoker.sd = sd(df.nonsmoker_nonirr[,'bwt'])
nonsmoker.skewness = 0
for (i in 1:nrow(df.smoker)){
    nonsmoker.skewness = nonsmoker.skewness+((df.nonsmoker_nonirr[i,'bwt']-123)/nonsmoker.sd)**3
}
nonsmoker.skew = nonsmoker.skewness/nrow(df.nonsmoker_nonirr)
nonsmoker.skew

low.bwt = 5.5*16
smoker.low.ind <- which(df.smoker['bwt'] < low.bwt)
smoker.lbwt = length(smoker.low.ind)
smoker.Rbwt = nrow(df.smoker)-smoker.lbwt

nonsmoker.low.ind <- which(df.nonsmoker_nonirr['bwt'] < low.bwt)
nonsmoker.lbwt = length(nonsmoker.low.ind)
nonsmoker.Rbwt = nrow(df.smoker)-nonsmoker.lbwt

freq_table <- matrix(c(smoker.lbwt, smoker.Rbwt, nrow(df.smoker),
         nonsmoker.lbwt, nonsmoker.Rbwt, nrow(df.nonsmoker_nonirr),
         smoker.lbwt+nonsmoker.lbwt, smoker.Rbwt+nonsmoker.Rbwt, nrow(df.smoker)+nrow(df.nonsmoker_nonirr)),
       ncol=3,byrow=TRUE)
colnames(freq_table) <- c('low weight', 'Regular weight', 'population total')
rownames(freq_table) <- c('smoker','non.smoker','weight total')
freq_table

print('low birth weight percentage of smoker')
smoker.lbwt/nrow(df.smoker)
print('low birth weight percentage of non smoker')
nonsmoker.lbwt/nrow(df.nonsmoker)
