# Making the Ref files for Norway
# Date: 3 March 2017
# Author: Lars Dalby

library(data.table)

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
attr = fread(file.path(staticpath, paste0('Attr_NTrondelagSubset', '.csv')))
setkey(attr, 'LINK')
recl = fread(file.path(staticpath, paste0('Reclass_Completemap_Subset','.txt')), sep = ':')
setnames(recl, c('PolyType', 'LINK'))
setkey(recl, 'LINK')

tmp = merge(attr, recl)  # Okay fine, now we can get rid of LINK.
tmp[, LINK:=NULL]
farmlink = fread(file.path(staticpath, paste0('FarmLinkTable_NTrondelagSubset', '.txt')))
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
setkey(full, PolyRef)
fwrite(full, file = file.path(staticpath, 'ALMaSSTrondelagSubset','PolyrefNTrondelagSubset.txt'), sep = '\t')

tables()


