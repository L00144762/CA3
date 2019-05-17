library(ggpubr)
library(pwr)
library(dplyr)
library(tibble)

#importing data
rainfall <- read.csv('rainfall.csv', 
                     header = TRUE, 
                     sep = ",",
                     strip.white = TRUE,
                     stringsAsFactors = FALSE)
temp <- read.csv('temp.csv',
                 header = TRUE, 
                 sep = ",",
                 strip.white = TRUE,
                 stringsAsFactors = FALSE)
str(rainfall)


head(rainfall, 10)

# removing unwanted rows; any row containging
# a certain string is removed
# only want the mean (given in dataset)
temp <- temp[grepl("Mean", temp$X.1),]

rainfall <- rainfall[grepl("Total", rainfall$X.1),]

#merging two columns and removing old column
temp$X.1 <- paste(temp$X, temp$X.1)
temp <- temp[,-1]

rainfall$X.1 <- paste(rainfall$X, rainfall$X.1)
rainfall <- rainfall[,-1]

# dealing with non-interger values
temp[temp == ".."] <- NA

rainfall[rainfall== ".."] <- NA

temp<- na.omit(temp) 
rainfall <- na.omit((rainfall))

# making rainfall and temp the variables in each dataframe
# transposing the column and rows

temp <-temp %>%
  t() %>%
  as.data.frame(stringsAsFactors = F) %>%
  rownames_to_column("value") %>%
  `colnames<-`(.[1,]) %>%
  .[-1,] %>%
  `rownames<-`(NULL)


rainfall <-rainfall %>%
  t() %>%
  as.data.frame(stringsAsFactors = F) %>%
  rownames_to_column("value") %>%
  `colnames<-`(.[1,]) %>%
  .[-1,] %>%
  `rownames<-`(NULL)

# renaming variables
colnames(temp) <- c("Month & Year", "Belmullet Temp","Valentia Temp", "Casement Temp", "Cork Temp",
                           "Dublin Temp","Malin Head Temp", "Mullingar Temp", "Shannon Temp")

colnames(rainfall) <-c("Month & Year","Belmullet Rain","Valentia Rain", "Casement Rain", "Cork Rain",
                          "Dublin Rain","Malin Head Rain", "Mullingar Rain", "Shannon Rain")



# merging the two datasets by the X.1 colum (yearly and monthly data)
total <- merge(temp, rainfall, by = "Month & Year")
total_data <-  merge(temp, rainfall, by = "Month & Year")

total <-total_data[,-1]
rownames(total) <- total_data[,1]

total[, 1:ncol(total)] <- sapply(total[, 1:ncol(total)],
                                 as.numeric)

#####ignore##### this is for yearly data
#sorting out the years
#only want to know averagae yearly values  for temp and rain
# so edit each value in Month & Yeart varible to only contain the year
#find.list <- list("X") #"M01", "M02", "M03",
#                   "M04", "M05", "M06", "M07", 
#                   "M08", "M09", "M10", "M11", "M12")
# find.string <- paste(unlist(find.list), collapse = "|")
# 
# total_data$`Month & Year` <- gsub(find.string, replacement = "", x = total_data$`Month & Year`)
# total_data$`Month & Year` <- as.factor(total_data$`Month & Year`)
# levels(total_data$`Month & Year`)
# str(total)

#making the first colum the index (years and months)
# total <-total_data[,-1]
# rownames(total) <- total_data[,1]
# 
# 
# #new dataframe containg the average temp for each station 
# # convert all columns excpet first to numeric
# # calculate the mean values for each factor variable
# # making the first colum  the index (years)
# total[, 1:ncol(total)] <- sapply(total[, 1:ncol(total)],
#                                  as.numeric)
# 
# temp_rain1 <- aggregate(total[,2:ncol(total)], 
#           by = list(total$`Month & Year`), 
#           FUN = mean)



# temp_rain <- temp_rain1[,-1]
# rownames(temp_rain) <- temp_rain1[,1]



#rainfall is the dependant variable and temperature is the independant
# want to see if rainfall is affected by temp change
#make dataframe numeric
str(total)


# splitting dataset into data pertaining to each province
Ulster <- total %>% select(`Malin Head Temp`,`Malin Head Rain`)
Munster <- total %>% select(`Valentia Temp`,`Valentia Rain`)
Leinster <- total %>% select(`Casement Temp`, `Casement Rain`,
                             `Dublin Temp`, `Dublin Rain`)
Connaucht <- total %>% select(`Belmullet Temp`, `Belmullet Rain`,
                              `Mullingar Temp`, `Mullingar Rain`,
                              `Shannon Temp`, `Shannon Rain`)
#mean rain and temp for connaucht and leinster for each year
# as they contain < 1 weather station
# this will unfortunaetly effect granularity 
Connaucht$Mean_Temp <- apply(Connaucht[,c("Belmullet Temp","Mullingar Temp",
                                          "Shannon Temp")], 1, mean)
Connaucht$Mean_Rain <- apply(Connaucht[,c("Belmullet Rain", "Mullingar Rain",
                                          "Shannon Rain")], 1, mean)
Leinster$Mean_Temp <- apply(Leinster[,c("Casement Temp", "Dublin Temp")], 1, mean)
Leinster$Mean_Rain <- apply(Leinster[,c("Casement Rain", "Dublin Rain")], 1, mean)



# mean, stdev skew and kurtosis for each province
my_stats <- function(x, na.omit = FALSE){
  if(na.omit)
    x <- x[!is.na(x)] #omits missing values
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m) ^ 3 / s ^ 3)/n
  kurt <- sum((x-m) ^ 4 / s ^ 4)/n - 3
  
  return(c(n = n, mean = m, stdev = s, skew = skew,
           kurtosis = kurt))
}

sapply(Connaucht[,7:ncol(Connaucht)], my_stats)
sapply(Munster, my_stats)
sapply(Ulster, my_stats)
sapply(Leinster[,5:ncol(Leinster)], my_stats)



#effect size for the analysis is 0.3
#  correlation is the analysis type
cohen.ES(test = "r", size = "medium")


# type 2 error tolerance of 0.2 so Power = 0.80
# type 1 error tolerance is usually 0.05. anything
# greater than this and H0 must be accepted or risk
# makign type 1 error
# two sided test (default); using two tail test because
# regardless of the direction of the relationship, I am
# testing for the possibility of a relationship in both directions
r_test <- pwr.r.test(r = 0.3, sig.level = 0.05, power = 0.8, alternative = "two.sided")
plot(r_test)

#show number of samples needed to get an effect size of 0.3
# power of 0.9 and p value of 0.05
pwr.t.test(n = NULL, d = 0.3, sig.level = 0.05, 
           power = 0.8, alternative = "two.sided")



#density graphs for each province
# shows releveant skewness of the dependat variable
d_u <- density(Ulster$`Malin Head Rain`) 
d_c <- density(Connaucht$Mean_Rain)
d_m <- density(Munster$`Valentia Rain`)
d_l <- density(Leinster$Mean_Rain)


par(mfrow=c(2,2))
plot(d_u, main = "Ulster Rain dist.")
abline(v = 94.84,#mean
       lty = 2, col = "blue")

plot(d_c, main = "Connaucht Rain dist.")
abline(v = 90.739,#mean
       lty = 2, col = "blue")

plot(d_m, main = "Munster Rain dist.")
abline(v = 134.028,#mean
       lty = 2, col = "blue")

plot(d_l, main = "Leinster Rain dist.")
abline(v = 63.705,#mean
       lty = 2, col = "blue")





#normality plot
# to see if the data follows a normal distribution.
# central limit theorem states that a large enough sample size
# sample > 30 will lead to a normal dist.


#taking the recommended sample
# this will not be used however as the dataframes are
#  relatively small and manageable
set.seed(1234)
sample <- dplyr::sample_n(Ulster, 85)

ggqqplot(Ulster$`Malin Head Rain`, main ="Ulster Normality")

ggqqplot(Connaucht$Mean_Rain, main = "Connaucht Normality")

ggqqplot(Munster$`Valentia Rain`, main = "Munster Normality")

ggqqplot(Leinster$Mean_Rain, main = "Leinster Normality")

#shapiro to show normality as visually determening normality is unreliable with 
# large dataframes
# p value is << 0.05 meaning a non-parametric test must be used
# also the plots shows the data does not follow a normal distribution - high kurtosis

shapiro.test(Ulster$`Malin Head Rain`)
shapiro.test(Connaucht$Mean_Rain)
shapiro.test(Munster$`Valentia Rain`)
shapiro.test(Leinster$Mean_Rain)


# correlation test: spearman test
cor.test(Ulster$`Malin Head Temp`, Ulster$`Malin Head Rain`,
         method = "spearman", exact = FALSE)

cor.test(Munster$`Valentia Temp`, Munster$`Valentia Rain`,
         method = "spearman", exact = FALSE)

cor.test(Connaucht$Mean_Temp, Connaucht$Mean_Rain,
         method = "spearman", exact =FALSE)

cor.test(Leinster$`Dublin Temp`, Leinster$`Dublin Rain`,
         method = "spearman", exact = FALSE)
cor.test(Leinster$`Casement Temp`, Leinster$`Casement Rain`,
         method = "spearman", exact = FALSE)
#Leinster accepts the null hypothesis as p-value > 0.05; avoiding a type 1 error
# each of the other provinces have p valu << 0.05 and so accept the alternate hypothesis; 
# avoiding a type 2 error (accepting H0)