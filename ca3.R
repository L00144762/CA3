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
#temp <- temp1[,-1]
#rownames(temp) <- temp1[,1]

rainfall1 <-rainfall %>%
  t() %>%
  as.data.frame(stringsAsFactors = F) %>%
  rownames_to_column("value") %>%
  `colnames<-`(.[1,]) %>%
  .[-1,] %>%
  `rownames<-`(NULL)
#rainfall <- rainfall1[,-1]
#rownames(rainfall) <- rainfall1[,1]



# renaming columns
colnames(temp1) <- c("Belmullet Temp","Valentia Temp", "Casement Temp", "Cork Temp",
                           "Dublin Temp","Malin Head Temp", "Mullingar Temp", "Shannon Temp")

colnames(rainfall1) <-c("Belmullet Rain","Valentia Rain", "Casement Rain", "Cork Rain",
                          "Dublin Rain","Malin Head Rain", "Mullingar Rain", "Shannon Rain")

# changing to numeric

#temp1[, 1:ncol(temp1)] <- sapply(temp1[2:ncol(temp1)],
                                    #as.numeric)



#rainfall1[, 1:ncol(rainfall1)] <- sapply(rainfall1[, 2:ncol(rainfall1)],
                               #as.numeric)




# merging the two datasets by the X.1 colum (yearly and monthly data)
total1 <- merge(temp1, rainfall1, by = "X.1")
total <- total1[,-1]
rownames(total) <- temp1[,1]

#changing structure to numeric
str(total)
total[, 1:ncol(total)] <- sapply(total[, 1:ncol(total)],
                                         as.numeric)
