# Making the Ref files for Norway
# Date: 3 March 2017
# Author: Lars Dalby

library(data.table)

staticpath = 'o:/ST_LandskabsGenerering/Norway/NTrondelag/Landscape/outputs'
attr = fread(file.path(staticpath, 'Attr_NTrondelag.csv'))
setkey(attr, 'LINK')
recl = fread(file.path(staticpath, 'Reclass_Completemap_NTrondelag.txt'), sep = ':')
setnames(recl, c('PolyType', 'LINK'))
setkey(recl, 'LINK')

tmp = merge(attr, recl)  # Okay fine, now we can get rid of LINK.
tmp[, LINK:=NULL]
farmlink = fread(file.path(staticpath, 'FarmLinkTable_NTrondelag.txt'))
farmlink = farmlink[PolyType >= 2100000,]
setkey(farmlink, PolyType)
setkey(tmp, PolyType)

full = merge(tmp, farmlink, all.x = TRUE)
full[is.na(FarmID), FarmID:="-1"]

full[PolyType %between% c(2100000, 2299999), PolyType:=20]  # Field
full[PolyType %between% c(2300000, 2399999), PolyType:=35]  # Permnanent pasture
setkey(full, PolyRef)
fwrite(full, file = file.path(staticpath, 'ALMaSSTestTrondelag', 'PolyrefTestNTrondelag.txt'), sep = '\t')

tables()

