library(dplyr)
countries <- read.csv("countries.csv", sep = ",", stringsAsFactors = FALSE)
names(countries) <- c("X", "Year", "CountryCode", "Country", "Population", "gdp", "FoodImports", "FuelImports")
#countries$gdpPC <- countries$gdp/countries$Population
#countries$FoodImportsPC <- countries$FoodImports/countries$Population
##EXPLORATORY ANANYSES
attach(countries)
plot(log(Population), log(FoodImports))
plot(Population, FoodImports, xlim = c(0,max(Population)*1.2))
text(countries$Population, countries$FoodImports, labels = countries$CountryCode, pos = 4)
summary(lm(FoodImports ~ Population, data = countries))

plot(log(Population), log(FuelImports))
plot(Population, FuelImports, xlim = c(0,max(Population)*1.2))
text(countries$Population, countries$FuelImports, labels = countries$CountryCode, pos = 4)
summary(lm(FuelImports ~ Population, data = countries))


plot(log(gdp), log(FoodImports))
plot(gdp, FoodImports, xlim = c(0,max(gdp)*1.2))
text(countries$gdp, countries$FoodImports, labels = countries$CountryCode, pos = 4)
summary(lm(FoodImports ~ gdp, data = countries))
summary(lm(FoodImports ~ gdp + I(gdp^2), data = countries))
#per Capita
plot(log(gdp/Population), log(FoodImports/Population))
plot(gdp/Population, FoodImports/Population)
text(gdp/Population, FoodImports/Population, labels = countries$CountryCode, pos = 4)
summary(lm(I(FoodImports/Population) ~ I(gdp/Population), data = countries))
summary(lm(I(FoodImports/Population) ~ I(gdp/Population) + I((gdp/Population)^2), data = countries))




plot(log(gdp), log(FuelImports))
plot(gdp, FuelImports, xlim = c(0,max(gdp)*1.2))
text(countries$gdp, countries$FuelImports, labels = countries$CountryCode, pos = 4)
summary(lm(FuelImports ~ gdp, data = countries))
summary(lm(FuelImports ~ gdp + I(gdp^2), data = countries))

plot(log(FoodImports), log(FuelImports))
plot(FoodImports, FuelImports, xlim = c(0,max(FoodImports)*1.2))
text(countries$FoodImports, countries$FuelImports, labels = countries$CountryCode, pos = 4)
summary(lm(FuelImports ~ FoodImports, data = countries))
detach(countries)


#plot(countries$gdpPC[-1], countries$FoodImportsPC[-1])
#text(countries$gdpPC[-1], countries$FoodImportsPC[-1], labels = countries$CountryCode[-1], pos = 4)



#plot(gdp, log(FoodImports))
###CLUSTERING
#mydata <- data.frame(gdp_norm = scale(countries$gdp), FoodImp_norm = scale(countries$FoodImports))
mydata <- data.frame(gdp = countries$gdp, FoodImp = countries$FoodImports)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 
# K-Means Cluster Analysis
nk <- 5
fit <- kmeans(mydata, nk) # 5 cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster) 

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
groups <- cutree(fit, k=nk) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=nk, border="red") 

df_table <- mydata %>% group_by(fit.cluster) %>% summarize(mean_gdp = mean(gdp), mean_FoodImp = mean(FoodImp))
df_table
summary(lm(df_table$mean_FoodImp ~ df_table$mean_gdp))

test <- function(ii) {
    mydata <- data.frame(gdp = countries$gdp, FoodImp = countries$FoodImports)
    wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
    for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                         centers=i)$withinss)
    #plot(1:15, wss, type="b", xlab="Number of Clusters",
         #ylab="Within groups sum of squares") 
    # K-Means Cluster Analysis
    nk <- ii
    fit <- kmeans(mydata, nk) # 5 cluster solution
    # get cluster means
    aggregate(mydata,by=list(fit$cluster),FUN=mean)
    # append cluster assignment
    mydata <- data.frame(mydata, fit$cluster) 
    
    # Ward Hierarchical Clustering
    d <- dist(mydata, method = "euclidean") # distance matrix
    fit <- hclust(d, method="ward.D")
    #plot(fit) # display dendogram
    groups <- cutree(fit, k=nk) # cut tree into 5 clusters
    # draw dendogram with red borders around the 5 clusters
    #rect.hclust(fit, k=nk, border="red") 
    
    df_table <- mydata %>% group_by(fit.cluster) %>% summarize(mean_gdp = mean(gdp), mean_FoodImp = mean(FoodImp))
    #df_table
    print(summary(lm(df_table$mean_FoodImp ~ df_table$mean_gdp))$r.squared)
}

for(i in 1:30) {test(i)}
