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
library(dplyr)

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
library(tibble)
temp1 <-temp %>%
  t() %>%
  as.data.frame(stringsAsFactors = F) %>%
  rownames_to_column("value") %>%
  `colnames<-`(.[1,]) %>%
  .[-1,] %>%
  `rownames<-`(NULL)
temp <- temp1[,-1]
rownames(temp) <- temp1[,1]

rainfall1 <-rainfall %>%
  t() %>%
  as.data.frame(stringsAsFactors = F) %>%
  rownames_to_column("value") %>%
  `colnames<-`(.[1,]) %>%
  .[-1,] %>%
  `rownames<-`(NULL)
rainfall <- rainfall1[,-1]
rownames(rainfall) <- rainfall1[,1]



# renaming columns
colnames(temp) <- c("Belmullet Temp","Valentia Temp", "Casement Temp", "Cork Temp",
                           "Dublin Temp","Malin Head Temp", "Mullingar Temp", "Shannon Temp")

colnames(rainfall) <-c("Belmullet Rain","Valentia Rain", "Casement Rain", "Cork Rain",
                          "Dublin Rain","Malin Head Rain", "Mullingar Rain", "Shannon Rain")

# changing to numeric

temp[, 1:ncol(temp)] <- sapply(temp[, 1:ncol(temp)],
                                    as.numeric)


rainfall[, 1:ncol(rainfall)] <- sapply(rainfall[, 1:ncol(rainfall)],
                               as.numeric)

str(temp)
str(rainfall)
