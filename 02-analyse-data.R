## working directory------------------------------------------------------------------------------------------
setwd("G:/NeuAll/Project/ROU-MB")

## read library------------------------------------------------------------------------------------------
library(plyr)

## year 2014------------------------------------------------------------------------------------------
data <- read.table("output/data_all_years_2.csv")

sort(unique(data$host))

# delete hosts
data_2 <- subset(data, !(host %in% c("confusing results",       
                                     "missing",
                                     "mosquito DNA",             
                                     "negative", 
                                     "FCS contamination",
                                     NA)))

# delete mosquito taxa
data_3 <- subset(data_2, !(mosquito_species %in% c("Culicoides",
                                     NA,
                                     "Simulidae")))

# delete sites with gravid traps
data_4 <- subset(data_3, !(data_3$site %in% c("GS", "CMZ")))

write.table(sort(unique(data_4$host)), "output/species.csv")

nrow(data_4)/nrow(data)
sort(unique(data_4$host))
colnames(data_4)
et <- ddply(data_4, .(mosquito_species, site),
            summarize,
            n_species = length(unique(host)),
            n_specimens = length(mosquito_species),
            length(unique(host))/length(mosquito_species))

ddply(data_4, .(host),
      summarize,
      n_specimens = length(host))

## host species per mosquito species------------------------------------------------------------------------------------------
anatidae <- c("Anas platyrhynchos", "Anatidae")
bovidae <- c("Bos taurus", "Bovidae")
chiroptera <- c("Rhinolophus hipposideros", "Pipistrellus kuhlii")

ddply(data_4, .(mosquito_species), function(d.sub) {length(unique(d.sub$host))-
        ifelse(((sum(match(unique(d.sub$host), "Chiroptera"), na.rm = T) > 0) + 
                  (sum(match(unique(d.sub$host), chiroptera), na.rm = T) > 0)) > 1, 1, 0) -
        ifelse(((sum(match(unique(d.sub$host), "Bovidae"), na.rm = T) > 0) + 
                  (sum(match(unique(d.sub$host), bovidae), na.rm = T) > 0)) > 1, 1, 0) -
        ifelse(((sum(match(unique(d.sub$host), "Anatidae"), na.rm = T) > 0) + 
                  (sum(match(unique(d.sub$host), anatidae), na.rm = T) > 0)) > 1, 1, 0)})


library(vegan)
data(BCI)
H <- diversity(BCI)
data_4$numbers <- 1

# diversity
et <- ddply(data_4, .(mosquito_species, host),
            summarize, numbers = sum(numbers))
spread(data_4, c("mosquito_species"), numbers)

AS2<-acast(et, mosquito_species~host, value.var="numbers", fill = 0)
diversity(AS2)
