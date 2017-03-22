# Making the Ref files for Norway
# Date: 3 March 2017
# Author: Lars Dalby

library(data.table)
library(ralmass)

GetNR = function(x){
  knr = stringr::str_split(x, pattern = '-')[[1]][1]
  KommuneNr = as.numeric(knr)*1000000
  gnr = stringr::str_split(x, pattern = '-')[[1]][2]
  GaardNr = as.numeric(gnr)*1000
  bnr = stringr::str_split(x, pattern = '-')[[1]][3]
  BrugerNr = as.numeric(bnr)
  return(KommuneNr+GaardNr+BrugerNr)
}

staticpath = 'o:/ST_LandskabsGenerering/Norway/NTrondelag/Landscape/outputs'
attr = fread(file.path(staticpath, paste0('Attr_NTrondelag', '.csv')))
setkey(attr, 'LINK')
recl = fread(file.path(staticpath, paste0('Reclass_Completemap_NTrondelag','.txt')), sep = ':')
setnames(recl, c('PolyType', 'LINK'))
setkey(recl, 'LINK')

tmp = merge(attr, recl)  # Okay fine, now we can get rid of LINK.
tmp[, LINK:=NULL]
farmlink = fread(file.path(staticpath, paste0('FarmLinkTable_NTrondelag', '.txt')))
farmlink = farmlink[PolyType >= 2100000,]
farmlink[,FarmNumber:=sapply(FarmID, FUN = GetNR)]
farmlink[is.na(FarmNumber),FarmNumber:=-9999L]  # Any fields without owner gets this NA indicator.
setkey(farmlink, PolyType)
setkey(tmp, PolyType)

full = merge(tmp, farmlink, all.x = TRUE)
full[is.na(FarmNumber), FarmNumber:=-1L]
full[, FarmID:=NULL]

full[PolyType %between% c(2100000, 2299999), PolyType:=20]  # Field
full[PolyType %between% c(2300000, 2399999), PolyType:=35]  # Permnanent pasture

setnames(full, c('PolyType','PolyRefNum','Area','Farmref'))
setkey(full, PolyRefNum)
WriteAlmassInput(full, pathtofile = file.path(staticpath, 'ALMaSSTrondelag','PolyrefNTrondelag.txt'))
# fwrite(full, file = file.path(staticpath, 'ALMaSSTrondelag','PolyrefNTrondelag.txt'), sep = '\t')

tables()

# Weather file:
weather = fread('o:/ST_GooseProject/Norway/NorwayLandscape/Weather/WeatherFromMæreStation2010-2016.txt')

# Investigating issue with too many farm IDs in the final map:
fullattr = fread(file.path('o:/ST_LandskabsGenerering/Norway/NTrondelag/Landscape/', 'CombiFinalAttributeTable.txt'))
str(fullattr)
length(fullattr[,unique(FARMID),])
fullattr[MATRIKKELK != 0, FarmNumber:=MATRIKKELK*1000000+GNR*1000+BNR]
length(fullattr[,unique(FARMID),])
fullattr[ARTYPE %between% c(21,23),.(NFarmID = length(unique(FARMID)), NFarmNumber = length(unique(FarmNumber)))]
# subset of 1719 (levanger, where Nesset sits)
levangerattr = fread(file.path('o:/ST_LandskabsGenerering/Norway/NTrondelag/Landscape/', '1719AttributeTable.txt'))
levangerattr[MATRIKKELK != 0, FarmNumber:=MATRIKKELK*1000000+GNR*1000+BNR]
levangerattr[ARTYPE %between% c(21,23),.(NFarmID = length(unique(FARMID)), NFarmNumber = length(unique(FarmNumber)))]
# Read in the production data and see which ID are in the map, but not in the production data.
library(readxl)
farmclassification = as.data.table(read_excel('o:/ST_GooseProject/Norway/NorwayLandscape/FarmClassification/FarmClassification_Norway_20160215.xlsx',
           sheet = 'Ark1', skip = 1))
farmnumbers_cjt = unique(farmclassification$FarmNumber)
fullattr[!FarmNumber %in% farmnumbers_cjt & ARTYPE %between% c(21,23) & MATRIKKELK == 1719,]

setkey(fullattr, 'FarmNumber')
levanger = fullattr[MATRIKKELK == 1719,]
levanger[, FarmNumber:=sapply(FARMID, FUN = GetNR)]
levanger[ARTYPE %between% c(21,23), Area:=sum(Shape_Area), by = FarmNumber]
levanger = unique(levanger[!is.na(Area), .(AreaDekar = Area/1000), by = FarmNumber])
setkey(levanger, 'FarmNumber')
farmclassification = farmclassification[Kommune == 1719,c("FarmNumber", "Total Area dekar")]
setkey(farmclassification, 'FarmNumber')
setnames(farmclassification, old = 'Total Area dekar', new = 'AreaDekar')
farms = merge(levanger, farmclassification, suffixes = c(".Map", ".Production"))
library(ggplot2)
ggplot(farms, aes(AreaDekar.Map, AreaDekar.Production)) + 
  geom_point() +
  xlab('Area in map') +
  ylab('Area in production data') +
  labs(title = 'Comparison of areas', subtitle = 'Levanger municipality. Area in dekar') +
  theme_bw()
polyrefarea = full[Farmref != -1,.(FieldArea = sum(Area)/1000), by = Farmref ]
setnames(polyrefarea, old = 'Farmref', new = 'FarmNumber')
setkey(polyrefarea, FarmNumber)
merge(farms, polyrefarea) %>%
ggplot(aes(AreaDekar.Map, FieldArea)) + 
  geom_point() +
  xlab('Area in map') +
  ylab('Area in polyref file') +
  labs(title = 'Comparison of areas', subtitle = 'Levanger municipality.\nComparing the area based on the attribute table in the shape files with the area in the polyref file. Area in dekar') +
  theme_bw()


