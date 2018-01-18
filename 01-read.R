## functions------------------------------------------------------------
# 
# from here: http://www.cookbook-r.com/Manipulating_data/Filling_in_NAs_with_last_non-NA_value/
fillNAgaps <- function(x, firstBack=FALSE) {
  ## NA's in a vector or factor are replaced with last non-NA values
  ## If firstBack is TRUE, it will fill in leading NA's with the first
  ## non-NA value. If FALSE, it will not change leading NA's.
  
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

## working directory------------------------------------------------------------
setwd("G:/NeuAll/Project/ROU-MB")

## read library------------------------------------------------------------
library(xlsx)
library(plyr)
library(stringr)
library(tidyr)

## year 2014------------------------------------------------------------
# read data 2014
data2014_2 <- read.xlsx("data/bloodmeals-2014.xlsx", 1)

# delete samples without information
#data2014_2 <- subset(data2014, !(data2014$Zusatz.bezeichnung == "k.A."))

# built new data.frame
data2014b <- data.frame(sample_id = as.character(data2014_2$Zusatz.bezeichnung),
                        site = as.character(fillNAgaps(data2014_2$Trapping.site)),
                        date = as.character(fillNAgaps(data2014_2$Date.of.collection)),
                        mosquito_species = as.character(data2014_2$Mosquitoe.species),
                        host = as.character(data2014_2$bewertet.als),stringsAsFactors=FALSE)

adapt_2014 <- read.xlsx("data/adapt_2015.xlsx", 2)[c(1:30, 32:57),]

data2014b$host[c(match(adapt_2014$Zusatz.bezeichnung,data2014b$sample_id))] <- as.character(adapt_2014$Answer.Alex)


data2014b$species_revalue <- revalue(data2014b$host, c("Anas sp." = "Anas spp.",      
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

#dfdf <- data2014_full %>% gather(species, specimens, Culex.modestus:Cx..spec)
#dfdf2 <- subset(dfdf, specimens > 0)
#splitted_sample_id2 <- str_split_fixed(dfdf2$Sample.ID, "C", 2)
#dfdf2$Sample.ID = paste(splitted_sample_id2[,1], splitted_sample_id2[,2], sep = "-")
#data2014b$sample_id2[!(data2014b$sample_id2 %in% dfdf2$Sample.ID)]

#uuu <- merge(data2014b, dfdf2, by.x = "sample_id2",
#             by.y = "Sample.ID")

## year 2015------------------------------------------------------------
data2015 <- read.xlsx("data/bloodmeals-2015.xlsx", 1,stringsAsFactors=FALSE)

dfdf <- data2015 %>% gather(species, specimens, Cx..spec:Mansonia.richiardii, na.rm = T)

BBB1 <- c("Kocker: Nycticorax An 3+4 05.09.17 negativ",
          "Bos taurus",
          "Felis catus",
          "Bovine with Burkett",
          "Kocher =Ardea purpurea ",
          "Gallus gallus m. Kocher An 3+4 05.09.17 negativ",
          "Homo sapiens m. Kocher")
BBB2 <- c("Gallus gallus m. Kocher",
          "Ardea purpurea am 22.11.17 (10157AA-411)",
          "Homo sapiens m. Kocher",
          "Homo sapiens am 22.11.17 (10157AA-411)") 
CCC <- subset(dfdf, Bemerkungen.1 %in% BBB1 | Bemerkungen.2 %in% BBB2)

CCC$Host. <- as.character(c("Bos taurus/Nycticorax nycticorax",
                            "Bos taurus",
                            "Felis catus",
                            "Sus scrofa/Bovine",
                            "Homo sapiens/Ardea purpurea",
                            "Bos taurus/Gallus gallus",
                            "Bos taurus/Ardea purpurea",
                            "Homo sapiens",
                            "Bos taurus/Homo sapiens",
                            "Bat/Homo sapiens",
                            "Homo sapiens", "Homo sapiens", "Homo sapiens"))

data2015b <- data.frame(sample_id = as.character(dfdf$Sample.ID), 
                        site = as.character(dfdf$Trapping.site),
                        date = as.character(dfdf$Date.of.collection),
                        mosquito_species = as.character(dfdf$species),
                        host = as.character(dfdf$Host.),stringsAsFactors=FALSE)

data2015b$host[c(match(CCC$Sample.ID,data2015b$sample_id))] <- CCC$Host.

# homogenize names of host species
data2015b$species_revalue <- revalue(data2015b$host, c(" Bos taurus" = "Bos taurus",           
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
data2016 <- read.xlsx("data/bloodmeals-2016.xlsx", 1)
data2016 <- data2016[1:497,]

eee <- str_split_fixed((data2016$Date.of.collection), "/", 2)[,2]
eee2 <- ifelse(eee == "", NA, eee)


data2016b <- data.frame(sample_id = as.character(data2016$Zusatz.bezeichnung), 
                        site = as.character(fillNAgaps(data2016$Trapping.site)),
                        date = as.character(fillNAgaps(eee2)),
                        mosquito_species = as.character(fillNAgaps(data2016$Mosquitoe.species)),
                        host = as.character(data2016$bewertet.als))

data2016b$species_revalue <- revalue(data2016b$host, c("Anopheles sp." = "mosquito DNA",             
                                                       "Canis lupus familiaris" = "Canis lupus",     
                                                       "FCS contam. Bos taurus" = "FCS contanimation",   
                                                       "Pelophylax (Rana) ridibundus" = "Pelophylax ridibundus",                   
                                                       "Mustela nivalis " = "Mustela nivalis"))

ee <- rbind(data2014b, data2015b, data2016b)

# remove 
ee2 <- subset(ee, !(species_revalue %in% c("mosquito DNA",
                                          "negative",
                                          "missing",
                                          "FCS contanimation",
                                          "FCS contamination",
                                          "confusing results")))

# homogenize names of mosquitoes
ee2$mosquito_species_new <- revalue(ee2$mosquito_species, c("Ae. vexans" = "Aedes vexans",
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
                                                            "An. plumbeus" = "Anopheles plumbeus", 
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

# resolve the 
sort(unique(ee2$species_revalue))
gdgdg <- str_split_fixed(ee2$species_revalue, "/", 2)
ee2$species_revalue[which(gdgdg[,2] > 0)] <- gdgdg[which(gdgdg[,2] > 0),1]
LLL <- ee2[which(gdgdg[,2] > 0),]
LLL$species_revalue <- gdgdg[which(gdgdg[,2] > 0),2]
ee3 <-rbind(ee2, LLL)

# homogenize names of sites
unique(ee3$site)
ee3$site <- revalue(ee3$site, c("Dunarea veche" = "Dunarea Veche",
                                "Dunarea Veche" = "Dunarea Veche",
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

ee4 <- subset(ee3, !(ee3$site %in% c("GS", "CMZ")))
        

et <- ddply(na.omit(ee4), .(mosquito_species_new, site),
      summarize,
      n_species = length(unique(species_revalue)),
      n_specimens = length(species_revalue),
      length(unique(species_revalue))/length(species_revalue))
sum(et[,4])



table((ee2$species_revalue))
