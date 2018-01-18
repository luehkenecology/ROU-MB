## functions------------------------------------------------------------
## NA's in a vector or factor are replaced with last non-NA values
## If firstBack is TRUE, it will fill in leading NA's with the first
## non-NA value. If FALSE, it will not change leading NA's.
# from here: http://www.cookbook-r.com/Manipulating_data/Filling_in_NAs_with_last_non-NA_value/
fillNAgaps <- function(x, firstBack=FALSE) {

  
  # If it's a factor, store the level labels and convert to integer
  lvls <- NULL
  if (is.factor(x)) {
    lvls <- levels(x)
    x    <- as.integer(x)
  }
  
  goodIdx <- !is.na(x)
  
  # These are the non-NA values from x only
  # Add a leading NA or take the first good value, depending on firstBack   
  if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
  else             goodVals <- c(NA,            x[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  x <- goodVals[fillIdx]
  
  # If it was originally a factor, convert it back
  if (!is.null(lvls)) {
    x <- factor(x, levels=seq_along(lvls), labels=lvls)
  }
  
  x
}

## working directory------------------------------------------------------------------------------------------
setwd("G:/NeuAll/Project/ROU-MB")

## read library------------------------------------------------------------------------------------------
library(xlsx)
library(plyr)
library(stringr)
library(tidyr)

## year 2014------------------------------------------------------------------------------------------
# read data 2014
data2014 <- read.xlsx("data/bloodmeals-2014.xlsx", 1)

# remove NAs
data2014_2 <- data2014[1:2891,]

# built new data.frame only with relevant information
data2014_3 <- data.frame(sample_id = as.character(data2014_2$Zusatz.bezeichnung),
                        site = as.character(fillNAgaps(data2014_2$Trapping.site)),
                        date = as.character(fillNAgaps(data2014_2$Date.of.collection)),
                        mosquito_species = as.character(data2014_2$Mosquitoe.species),
                        host = as.character(data2014_2$bewertet.als),stringsAsFactors=FALSE)

# read excel file to adapt some of the host species names 
adapt_2014 <- read.xlsx("data/adapt_2015.xlsx", 2)[1:57,]

# identify samples by sample id and replace host species names 
data2014_3$host[c(match(adapt_2014$Zusatz.bezeichnung,data2014_3$sample_id))] <- as.character(adapt_2014$Answer.Alex)

# homogenize names of host species
data2014_3$host <- revalue(data2014_3$host, c("Anas sp." = "Anas spp.",      
                                              "Anopheles sp mit Kocher" = "mosquito DNA",              
                                              "Anopheles sp." = "mosquito DNA",                         
                                              "Anopheles sp. Mit Kocher" = "mosquito DNA",    
                                              "Bos taurus " = "Bos taurus",                          
                                              "Canis lupus familiaris" = "Canis lupus",            
                                              "Canis lupus familiaris famiaris" = "Canis lupus", 
                                              "Equus caballus " = "Equus caballus",                        
                                              "felis catus" = "Felis catus",                          
                                              "Hirundo rustica transitiva" = "Hirundo rustica",            
                                              "Homo sapiens " = "Homo sapiens",                        
                                              "Homo sapiens m. Kocher" = "Homo sapiens",
                                              "Homo sapiens sapiens"= "Homo sapiens",
                                              "kein ergebnis" = "negative",                   
                                              "Lepus europaeus " = "Lepus europaeus",                       
                                              "negativ" = "negative",
                                              "Rhinolophus h. hipposideros" = "Rhinolophus hipposideros",           
                                              "schlechte ident." = "negative",                     
                                              "sus scrofa" = "Sus scrofa",                         
                                              "unauswertbar" = "negative"))          

## year 2015------------------------------------------------------------
# read data 2015
data2015 <- read.xlsx("data/bloodmeals-2015.xlsx", 1,stringsAsFactors=FALSE)

# collapse mosquito species information from columns to 1 column
data2015_2 <- data2015 %>% gather(species, specimens, Cx..spec:Mansonia.richiardii, na.rm = T)

# identify samples with additional host information (e.g. multiple species)
add_info_2015_1 <- c("Kocker: Nycticorax An 3+4 05.09.17 negativ",
          "Bos taurus",
          "Felis catus",
          "Bovine with Burkett",
          "Kocher =Ardea purpurea ",
          "Gallus gallus m. Kocher An 3+4 05.09.17 negativ",
          "Homo sapiens m. Kocher")
add_info_2015_2 <- c("Gallus gallus m. Kocher",
          "Ardea purpurea am 22.11.17 (10157AA-411)",
          "Homo sapiens m. Kocher",
          "Homo sapiens am 22.11.17 (10157AA-411)") 
add_info_2015 <- subset(data2015_2, Bemerkungen.1 %in% add_info_2015_1 | Bemerkungen.2 %in% add_info_2015_2)

# information to replace for the host of the samples with additional host information (e.g. multiple species)
add_info_2015$Host. <- as.character(c("Bos taurus/Nycticorax nycticorax",
                                      "Bos taurus",
                                      "Felis catus",
                                      "Sus scrofa/Bovidae",
                                      "Homo sapiens/Ardea purpurea",
                                      "Bos taurus/Gallus gallus",
                                      "Bos taurus/Ardea purpurea",
                                      "Homo sapiens",
                                      "Bos taurus/Homo sapiens",
                                      "Chiroptera/Homo sapiens",
                                      "Homo sapiens", "Homo sapiens", "Homo sapiens"))

# built new data.frame with relevant information
data2015_3 <- data.frame(sample_id = as.character(data2015_2$Sample.ID), 
                        site = as.character(data2015_2$Trapping.site),
                        date = as.character(data2015_2$Date.of.collection),
                        mosquito_species = as.character(data2015_2$species),
                        host = as.character(data2015_2$Host.),stringsAsFactors=FALSE)

# identify samples by sample id and replace host species names 
data2015_3$host[c(match(add_info_2015$Sample.ID,data2015_3$sample_id))] <- add_info_2015$Host.

# homogenize names of host species
data2015_3$host <- revalue(data2015_3$host, c(" Bos taurus" = "Bos taurus",           
                                              " Sus scrofa" = "Sus scrofa",              
                                              "Anas clypeata/Cygnus olor" = "Anatidae",
                                              "Anopheles sp." = "mosquito DNA",             
                                              "Bovine" = "Bovidae",                    
                                              "Canis lupus familiaris" = "Canis lupus",     
                                              "FCS contam. Bos taurus" = "FCS contanimation",   
                                              "fehlt" = "missing",                   
                                              "Hipposideros (Bat)" = "Chiroptera",       
                                              "kein ergebnis" = "negative",   
                                              "negativ" = "negative"))          

## year 2016------------------------------------------------------------
# read data 2016
data2016 <- read.xlsx("data/bloodmeals-2016.xlsx", 1)

# remove NAs
data2016_2 <- data2016[1:497,]

# extract date information of sampling
#date_info_2016_1 <- str_split_fixed((data2016_2$Date.of.collection), "/", 2)[,2]
#date_info_2016_2 <- ifelse(date_info_2016_1 == "", NA, date_info_2016_1)

# built new data.frame with relevant information
data2016_3 <- data.frame(sample_id = as.character(data2016_2$Zusatz.bezeichnung), 
                        site = as.character(fillNAgaps(data2016_2$Trapping.site)),
                        date = as.character(fillNAgaps(data2016_2$date_new)),
                        mosquito_species = as.character(fillNAgaps(data2016_2$Mosquitoe.species)),
                        host = as.character(data2016_2$bewertet.als))

# homogenize names of host species
data2016_3$host <- revalue(data2016_3$host, c("Anopheles sp." = "mosquito DNA",             
                                            "Canis lupus familiaris" = "Canis lupus",     
                                            "FCS contam. Bos taurus" = "FCS contanimation",   
                                            "Pelophylax (Rana) ridibundus" = "Pelophylax ridibundus",                   
                                            "Mustela nivalis " = "Mustela nivalis"))

# rbind data from the 3 years
data_all_years <- rbind(data2014_3, data2015_3, data2016_3)

# finally homogenize names of host species
data_all_years$host <- revalue(data_all_years$host, c("Anas phathyrhyncos" = "Anas platyrhynchos",
                                                      "FCS contanimation" = "FCS contamination"))

# homogenize names of mosquitoes
data_all_years$mosquito_species <- revalue(data_all_years$mosquito_species, c("Ae. vexans" = "Aedes vexans",
                                                            "Ae./Oc. spec" = "Aedes spp.",                
                                                            "Ae./Oc. spec " = "Aedes spp.",
                                                            "Ae./Oc. spec." = "Aedes spp.",                 
                                                            "Aedes cinereus" = "Aedes cinereus",
                                                            "Aedes vexans" = "Aedes vexans",                  
                                                            "Aedes.vexans" = "Aedes vexans",
                                                            "An. algeriensis" = "Anopheles algeriensis",               
                                                            "An. hyrcanus" = "Anopheles hyrcanus",   
                                                            "An. maculipennis" = "Anopheles maculipennis s.l.",               
                                                            "An. maculipennis s.l." = "Anopheles maculipennis s.l.",  
                                                            "An. maculipennis s.l. ???" = "Anopheles maculipennis s.l.",      
                                                            "An. plumbeus" = "Anopheles algeriensis", 
                                                            "Anopheles claviger" = "Anopheles algeriensis",               
                                                            "Anopheles hyrcanus" = "Anopheles hyrcanus",   
                                                            "Anopheles maculipennis" = "Anopheles maculipennis s.l.",        
                                                            "Anopheles.algeriensis" = "Anopheles algeriensis", 
                                                            "Anopheles.hyrcanus" = "Anopheles hyrcanus",            
                                                            "Anopheles.maculipennis" = "Anopheles maculipennis s.l.",    
                                                            "Coq. richiardii" = "Coquillettidia richiardii",               
                                                            "Coq. Richiardii" = "Coquillettidia richiardii",  
                                                            "Culex martinii" = "Culex martinii",               
                                                            "Culex modestus" = "Culex modestus",
                                                            "Culex pipiens/ torrentium" ="Culex pipiens s.l./torrentium",     
                                                            "Culex pipiens/torrentium" ="Culex pipiens s.l./torrentium",     
                                                            "Culex.pipiens" ="Culex pipiens s.l./torrentium",                      
                                                            "culicoides (8)" = "Culicoides",
                                                            "Culex.modestus" = "Culex modestus",
                                                            "Cx. modestus" = "Culex modestus",                  
                                                            "Cx. pip/tor" ="Culex pipiens s.l./torrentium",     
                                                            "Cx. pipiens/modestus/torrentiu" ="Culex pipiens s.l./torrentium",     
                                                            "Cx. spec" = "Culex spp.",
                                                            "Cx. Spec" = "Culex spp.",                      
                                                            "Mansonia richiardii" = "Coquillettidia richiardii",
                                                            "Mansonia.richiardii"  = "Coquillettidia richiardii",          
                                                            "Oc. caspius"  = "Aedes caspius",                  
                                                            "Oc. flavescens" = "Aedes flavescens",
                                                            "Ochlerotatus caspius" = "Aedes caspius",          
                                                            "Ochlerotatus.caspius" = "Aedes caspius",            
                                                            "Ochlerotatus.detritus" = "Aedes detritus",         
                                                            "Ochlerotatus.flavescens" = "Aedes flavescens",
                                                            "simulidae" = "Simulidae",                  
                                                            "unbestimmbar" = "unidentified",                 
                                                            "unidentified" = "unidentified",                  
                                                            "Ur. unguiculata" = "Uranotaenia unguiculata"))

# homogenize names of sites
data_all_years$site <- revalue(data_all_years$site, c("Dunarea veche" = "Dunarea Veche",
                                "Letea" = "Letea",
                                "Sulina" = "Sulina",
                                "L. Rosulet" = "Lake Rosulet",
                                "LAKE ROSULET" = "Lake Rosulet",
                                "SULINA" = "Sulina",
                                "LETEA"  = "Letea",
                                "DUNAREA-VECHE" = "Dunarea Veche",
                                "DUNAREA VECHE" = "Dunarea Veche",
                                "DV1" = "Dunarea Veche",
                                "LR" = "Lake Rosulet",
                                "LT" = "Lake Rosulet",
                                "DV2" = "Dunarea Veche",
                                "DV3" = "Dunarea Veche",
                                "SL" = "Sulina"))

# date
dates_new <- do.call(rbind, strsplit(data_all_years$date, '[.]'))
dates_new_2 <- paste(dates_new[,1], dates_new[,2], dates_new[,3])
dates_new_3 <- as.POSIXct(dates_new_2, format = "%d%m%Y")

data_all_years$date <- dates_new_3

write.table(data_all_years, "output/data_all_years.csv")

# resolve the multiple-host samples and add as additional samples on the end of the table
splitted_host_info <- str_split_fixed(data_all_years$host, "/", 2)
data_all_years$host[which(splitted_host_info[,2] > 0)] <- splitted_host_info[which(splitted_host_info[,2] > 0),1]
data_all_years_sub <- data_all_years[which(splitted_host_info[,2] > 0),]
data_all_years_sub$host <- splitted_host_info[which(splitted_host_info[,2] > 0),2]
data_all_years_2 <-rbind(data_all_years, data_all_years_sub)

write.table(data_all_years_2, "output/data_all_years_2.csv")



# delete hosts
ee2 <- subset(ee, !(species_revalue %in% c("confusing results"       
                                           "missing"
                                           "mosquito DNA"             
                                           "negative" 
                                           "FCS contanimation")))

# delete taxa
"Culicoides",
"NA",
"Simulidae",
"unidentified" 


# delete sites with gravid traps
ee4 <- subset(ee3, !(ee3$site %in% c("GS", "CMZ")))
        





et <- ddply(na.omit(ee4), .(mosquito_species_new, site),
      summarize,
      n_species = length(unique(species_revalue)),
      n_specimens = length(species_revalue),
      length(unique(species_revalue))/length(species_revalue))
sum(et[,4])
