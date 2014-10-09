# -*- coding: cp1252 -*-
# Name: twoInOne vs 1
# Form�l: This script combines conversion and mosaic
# Flemming Skov - Oct2014
# Sidst opdateret: October 9, 2014

# IMPORT SYSTEM MODULES
import arcpy, traceback, sys, time
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")
nowTime = time.strftime('%X %x')
print "Landscape conversion started: " + nowTime
print "... system modules found and read"

# DATA - HER S�TTES LINKS TIL DIVERSE GEODATABASER OG FOLDERS
outPatch = "E:/Landskabsgenerering/landskaber/Vejlerne/vejlerne.gdb/"                     # Her gemmes de f�rdige rasterkort
localSettings = "E:/Landskabsgenerering/landskaber/Vejlerne/project.gdb/vejlerne_mask"    # Projektfolder med mask
gisDB = "E:/LandskabsGenerering/gis/dkgis.gdb"                                            # Her findes feature gis lagene
gis2DB = "E:/Landskabsgenerering/landskaber/Vejlerne/vejlerne.gdb"                        # Her l�ses filerne fra i mosaikprocessen
asciisti = "E:/Landskabsgenerering/landskaber/Vejlerne/ASCII_Input.txt"                   
scratchDB = "E:/LandskabsGenerering/landskaber/Vejlerne/scratch"                          # Folder til tempor�re raster

# VIGTIGT-VIGTIGT-VIGTIGT:  Husk at �ndre reference til det konkrete marklag i lagene 505 og 1000!!!

arcpy.env.overwriteOutput = True
arcpy.env.workspace = gisDB
arcpy.env.scratchWorkspace = scratchDB
arcpy.env.extent = localSettings
arcpy.env.mask = localSettings
arcpy.env.cellSize = localSettings
print "... geoprocessing settings read"


# IMPORTANT - controls which processes are executed
default = 1

#MOSAIC
vejnet_c = default
bebyggelser_c = default
natur_c = default
vaadnatur_c = default
ferskvand_c = default
kultur_c = default
mosaik_c = default


#CONVERSION
landhav_c = default   #land_hav  - det faste land - og havet 

skrt105_c = default  #skraent - skr�nter langs veje
vejk110_c = default  #vejkant
stie112_c = default  #stier - stier (inkl. cykelstier og grusstier af enhver art)
park114_c = default  #parkering - parkeringspladser
spor115_c = default  #vejmidt_brudt - spor (grusveje og spor)
jern120_c = default  #jernbane_brudt - jernbaner
vu30122_c = default  #vejmidt_brudt - sm� veje (under 3 meter)
vu60125_c = default  #vejmidt_brudt - mellemstore veje (3-6 meter)
vu90130_c = default  #vejmidt_brudt - store veje (over 6 meter)
# OBS: de f�lgende to lag tilknyttes forel�big vejlaget for at sikre, at de ender �verst i mosaikken
hjsp150_c = default  #h�jsp�ndingsmaster
vind155_c = default  #vind�ller

lavb205_c = default   #lavbebyg - lav bebyggelse
hojb210_c = default   #hojbebyg - h�j bebyggelse
byke215_c = default   #bykerne
indu220_c = default   #industri - industriomr�der
kirk225_c = default   #kirkegrd - kirkegaarde
sprt230_c = default   #sportanlaeg - sportsanl�g
bygn250_c = default   #bygning

skov310_c = default   #top10dk skov (alle typer)
krat315_c = default   #top10dk krat_bevoksning
sand320_c = default   #top10dk sand - is�r langs kyster
hede325_c = default   #top10dk hede - 'hede' d�kker ogs� klitter og overdrev
vaad330_c = default   #top10dk vaadomr�de - moser, enge, mv.

eng_355_c = default   #paragraf 3 eng
hede360_c = default   #paragraf 3 hede
mose365_c = default   #paragraf 3 mose
over370_c = default   #paragraf 3 overdrev
seng375_c = default   #paragraf 3 strandeng
soe_380_c = default   #paragraf 3 s�

soer440_c = default   #soer - s�er
aaer435_c = default   #vandloeb_brudt - sm� vandl�b (< 2.5 meter)
aaer436_c = default   #vandloeb_brudt - mellemstore vandl�b (2.5 - 12 meter)
aaer437_c = default   #vandloeb_brudt - store vandl�b (> 12 meter)
sorn420_c = default   #soer - randzone om s�er

#NB: disse tre randzoner beregnes automatisk - de er n�vnt her for at holde styr p� koderne
#aarn425        #vandloeb_brudt - randzone om sm� vandl�b
#aarn426        #vandloeb_brudt - randzone om mellemstore vandl�b
#aarn427        #vandloeb_brudt - randzone om store vandl�b

mark505_c = default   #markrandzoner
mark1000_c = default  #marker

dige620_c = default   #diger
fred625_c = default   #fred_fortid  - fredede fortidsminder
rekr630_c = default   #rekromr - rekreative omr�der - vel i reglen gr�s
hegn635_c = default   #levende hegn
trae640_c = default   #tr�grupper
trae641_c = default   #enkeltst�ende tr�
raas650_c = default   #raastof - raastofudvinding

ais1100_c = default   #ais arealanvendelse 100 kort

print " "

#####################################################################################################

try:

# 1 - land og hav (landhav)
  if landhav_c == 1:
    print "Processerer basiskort med land og hav ..."
    if arcpy.Exists(outPatch + "landhav"):
      arcpy.Delete_management(outPatch + "landhav")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("land_hav", "Land", outPatch + "landhav", "CELL_CENTER", "NONE", "1")

# 105 - Skr�nter til raster (skrt105)
  if skrt105_c == 1:
    print "Processerer kunstige skr�nter ved vejanl�g ..."
    if arcpy.Exists(outPatch + "skrt105"):
      arcpy.Delete_management(outPatch + "skrt105")
      print "... sletter eksisterende raster"
    eucDistVejkant = EucDistance("skraent","","1","")
    skrt105 = Con(eucDistVejkant < 2.5, 105, 1)
    skrt105.save(outPatch + "skrt105")

# 110 - Vejkanter til raster (vejk110)
  if vejk110_c == 1:
    print "Processerer vejkanter ..."
    if arcpy.Exists(outPatch + "vejk110"):
      arcpy.Delete_management(outPatch + "vejk110")
      print "... sletter eksisterende raster"
    eucDistVejkant = EucDistance("vejkant","","1","")
    vejk110 = Con(eucDistVejkant < 1.75, 110, 1)
    vejk110.save(outPatch + "vejk110")

# 112 - Stier til raster (stie112)
  if stie112_c == 1:
    print "Processerer stier  ..."
    if arcpy.Exists(outPatch + "stie120"):
      arcpy.Delete_management(outPatch + "stie112")
      print "... sletter eksisterende raster"
    eucDistTmp = EucDistance("veje_stier", "", "1", "")
    stie112 = Con(eucDistTmp < 1.51, 112, 1)
    stie112.save(outPatch + "stie112")
    
# 114 - Parkeringsarealer til raster (park114)
  if park114_c == 1:
    print "Processerer parkeringsomr�der ..."
    if arcpy.Exists(outPatch + "park114"):
      arcpy.Delete_management(outPatch + "park114")
      print "... sletter eksisterende raster"
    eucDistTmp = EucDistance("parkering", "", "1", "")
    park114 = Con(eucDistTmp < 3.0, 114, 1)
    park114.save(outPatch + "park114")

# 115 - Spor til raster (spor115)
  if spor115_c == 1:
    print "Processerer spor  ..."
    if arcpy.Exists(outPatch + "spor115"):
      arcpy.Delete_management(outPatch + "spor115")
      print "... sletter eksisterende raster"
    eucDistTmp = EucDistance("veje_spor", "", "1", "")
    spor115 = Con(eucDistTmp < 2.25, 115, 1)
    spor115.save(outPatch + "spor115")    

# 120 - Jernbaner til raster (jern120)
  if jern120_c == 1:
    print "Processerer jernbanespor ..."
    if arcpy.Exists(outPatch + "jern120"):
      arcpy.Delete_management(outPatch + "jern120")
      print "... sletter eksisterende raster"
    eucDistTmp = EucDistance("jernbane_brudt", "", "1", "")
    jern120 = Con(eucDistTmp < 4.5, 120, 1)
    jern120.save(outPatch + "jern120")

 # 122 - Sm� veje til raster (vu30122)
  if vu30122_c == 1:
    print "Processerer sm� veje  ..."
    if arcpy.Exists(outPatch + "vu30122"):
      arcpy.Delete_management(outPatch + "vu30122")
      print "... sletter eksisterende raster"
    eucDistTmp = EucDistance("veje_vu30", "", "1", "")
    vu30122 = Con(eucDistTmp < 1.75, 122, 1)
    vu30122.save(outPatch + "vu30122")

 # 125 - Mellemstore veje til raster (vu60125)
  if vu60125_c == 1:
    print "Processerer mellemstore veje ..."
    if arcpy.Exists(outPatch + "vu60125"):
      arcpy.Delete_management(outPatch + "vu60125")
      print "... sletter eksisterende raster"
    eucDistTmp = EucDistance("veje_vu60", "", "1", "")
    vu60125 = Con(eucDistTmp < 3.0, 125, 1)
    vu60125.save(outPatch + "vu60125")

 # 130 - Store veje til raster (vu90130)
  if vu90130_c == 1:
    print "Processerer store veje ..."
    if arcpy.Exists(outPatch + "vu90130"):
      arcpy.Delete_management(outPatch + "vu90130")
      print "... sletter eksisterende raster"
    eucDistTmp = EucDistance("veje_vu90", "", "1", "")
    vu90130 = Con(eucDistTmp < 5.0, 130, 1)
    vu90130.save(outPatch + "vu90130")

##### OBS: H�jsp�ndingsmaster og vindm�ller sat ind i vejlaget; da de skal �verst i mosaikken

# 150 - H�jsp�ndingsmaster
  if hjsp150_c == 1:
    print "Processerer h�jsp�ndingsmaster ..."
    if arcpy.Exists(outPatch + "hjsp150"):
      arcpy.Delete_management(outPatch + "hjsp150")
      print "... sletter eksisterende raster"
    eucDistTmp = EucDistance("hjspmast", "", "1", "")
    hjsp150 = Con(eucDistTmp < 1.5, 150, 1)
    hjsp150.save(outPatch + "hjsp150")

# 155 - H�jsp�ndingsmaster
  if vind155_c == 1:
    print "Processerer vindm�ller ..."
    if arcpy.Exists(outPatch + "vind155"):
      arcpy.Delete_management(outPatch + "vind155")
      print "... sletter eksisterende raster"
    eucDistTmp = EucDistance("vindmoel", "", "1", "")
    vind155 = Con(eucDistTmp < 1.5, 155, 1)
    vind155.save(outPatch + "vind155")
     

# 205 - Lav bebyggelse
  if lavb205_c == 1:
    print "Processerer lav bebyggelse ..."
    if arcpy.Exists(outPatch + "lavb205"):
      arcpy.Delete_management(outPatch + "lavb205")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("lavbebyg", "FEAT_KODE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    lavb205 = Con(rasIsNull == 1, 1, 205)
    lavb205.save(outPatch + "lavb205")
    arcpy.Delete_management(outPatch + "tmpRaster")

# 210 - H�j bebyggelse
  if hojb210_c == 1:
    print "Processerer h�j bebyggelse ..."
    if arcpy.Exists(outPatch + "hojb210"):
      arcpy.Delete_management(outPatch + "hojb210")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("hojbebyg", "FEAT_KODE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    hojb210 = Con(rasIsNull == 1, 1, 210)
    hojb210.save(outPatch + "hojb210")
    arcpy.Delete_management(outPatch + "tmpRaster")

# 215 - Bykerne
  if byke215_c == 1:
    print "Processerer bykerner ..."
    if arcpy.Exists(outPatch + "byke215"):
      arcpy.Delete_management(outPatch + "byke215")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("bykerne", "FEAT_KODE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    byke215 = Con(rasIsNull == 1, 1, 215)
    byke215.save(outPatch + "byke215")
    arcpy.Delete_management(outPatch + "tmpRaster")

# 220 - Industri
  if indu220_c == 1:
    print "Processerer industriomr�der ..."
    if arcpy.Exists(outPatch + "indu220"):
      arcpy.Delete_management(outPatch + "indu220")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("industri", "FEAT_KODE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    indu220 = Con(rasIsNull == 1, 1, 220)
    indu220.save(outPatch + "indu220")
    arcpy.Delete_management(outPatch + "tmpRaster")

# 225 - Kirkeg�rde
  if kirk225_c == 1:
    print "Processerer kirkeg�rde ..."
    if arcpy.Exists(outPatch + "kirk225"):
      arcpy.Delete_management(outPatch + "kirk225")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("kirkegrd", "FEAT_KODE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    kirk225 = Con(rasIsNull == 1, 1, 225)
    kirk225.save(outPatch + "kirk225")
    arcpy.Delete_management(outPatch + "tmpRaster")

# 230 - Sportsanl�g
  if sprt230_c == 1:
    print "Processerer sportsanl�g ..."
    if arcpy.Exists(outPatch + "sprt230"):
      arcpy.Delete_management(outPatch + "sprt230")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("sportanlaeg", "FEAT_KODE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    sprt230 = Con(rasIsNull == 1, 1, 230)
    sprt230.save(outPatch + "sprt230")
    arcpy.Delete_management(outPatch + "tmpRaster")

# 250 - Bygninger
  if bygn250_c == 1:
    print "Processerer bygninger ..."
    if arcpy.Exists(outPatch + "bygn250"):
      arcpy.Delete_management(outPatch + "bygn250")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("bygning", "FEAT_KODE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    bygn250 = Con(rasIsNull == 1, 1, 250)
    bygn250.save(outPatch + "bygn250")
    arcpy.Delete_management(outPatch + "tmpRaster")

# 310 - skove (OBS: har kun et tema for skov kan ikke underopdeles) (skov310)
  if skov310_c == 1:
    print "Processerer skove ..."
    if arcpy.Exists(outPatch + "skov310"):
      arcpy.Delete_management(outPatch + "skov310")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("skov", "FEAT_KODE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    skov310 = Con(rasIsNull == 1, 1, 310)
    skov310.save(outPatch + "skov310")
    arcpy.Delete_management(outPatch + "tmpRaster")

# 315 - krat (OBS: har kun et tema for skov kan ikke underopdeles) (krat315)
  if krat315_c == 1:
    print "Processerer kratvegetation ..."
    if arcpy.Exists(outPatch + "krat315"):
      arcpy.Delete_management(outPatch + "krat315")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("krat_bevoksning", "FEAT_KODE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    krat315 = Con(rasIsNull == 1, 1, 315)
    krat315.save(outPatch + "krat315")
    arcpy.Delete_management(outPatch + "tmpRaster")

# 320 - sand (svarer mest til sand ved kysten) (sand315)
  if sand320_c == 1:
    print "Processerer sandflader ..."
    if arcpy.Exists(outPatch + "sand320"):
      arcpy.Delete_management(outPatch + "sand320")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("sand", "FEAT_KODE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    sand320 = Con(rasIsNull == 1, 1, 320)
    sand320.save(outPatch + "sand320")
    arcpy.Delete_management(outPatch + "tmpRaster")

# 325 - hede (OBS: d�kker b�de hede og overdrev og til dels klit) (hede325)
  if hede325_c == 1:
    print "Processerer hedevegetation ..."
    if arcpy.Exists(outPatch + "hede325"):
      arcpy.Delete_management(outPatch + "hede325")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("hede", "FEAT_KODE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    hede325 = Con(rasIsNull == 1, 1, 325)
    hede325.save(outPatch + "hede325")
    arcpy.Delete_management(outPatch + "tmpRaster")

# 330 - vaadomraade (vaad330)
  if vaad330_c == 1:
    print "Processerer vaadomraader ..."
    if arcpy.Exists(outPatch + "vaad330"):
      arcpy.Delete_management(outPatch + "vaad330")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("vaadomraade", "FEAT_KODE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    vaad330 = Con(rasIsNull == 1, 1, 330)
    vaad330.save(outPatch + "vaad330")
    arcpy.Delete_management(outPatch + "tmpRaster")

# 355 - p3 eng 3055 (eng_355)
  if eng_355_c == 1:
    print "Processerer paragraf3-enge ..."
    if arcpy.Exists(outPatch + "eng_355"):
      arcpy.Delete_management(outPatch + "eng_355")
      print "... sletter eksisterende raster"
    feat_class = "paragraf3"
    feat_layer = "eng_355"
    arcpy.MakeFeatureLayer_management(feat_class,feat_layer)
    arcpy.SelectLayerByAttribute_management(feat_layer, "NEW_SELECTION", '"OBJEKTKODE" = 3055')
    eucDisttmp = EucDistance("eng_355","","1","")
    eng_355 = Con(eucDisttmp < 3, 355, 1)
    eng_355.save(outPatch + "eng_355")

# 360 - p3 hede 3060 (hede360)
  if hede360_c == 1:
    print "Processerer paragraf3-heder ..."
    if arcpy.Exists(outPatch + "hede360"):
      arcpy.Delete_management(outPatch + "hede360")
      print "... sletter eksisterende raster"
    feat_class = "paragraf3"
    feat_layer = "hede360"
    arcpy.MakeFeatureLayer_management(feat_class,feat_layer)
    arcpy.SelectLayerByAttribute_management(feat_layer, "NEW_SELECTION", '"OBJEKTKODE" = 3060')
    eucDisttmp = EucDistance("hede360","","1","")
    hede360 = Con(eucDisttmp < 3, 360, 1)
    hede360.save(outPatch + "hede360")

# 365 - p3 mose 3065 (mose365)
  if mose365_c == 1:
    print "Processerer paragraf3-moser ..."
    if arcpy.Exists(outPatch + "mose365"):
      arcpy.Delete_management(outPatch + "mose365")
      print "... sletter eksisterende raster"
    feat_class = "paragraf3"
    feat_layer = "mose365"
    arcpy.MakeFeatureLayer_management(feat_class,feat_layer)
    arcpy.SelectLayerByAttribute_management(feat_layer, "NEW_SELECTION", '"OBJEKTKODE" = 3065')
    eucDisttmp = EucDistance("mose365","","1","")
    mose365 = Con(eucDisttmp < 3, 365, 1)
    mose365.save(outPatch + "mose365")

# 370 - p3 overdrev 3070 (over370)
  if over370_c == 1:
    print "Processerer paragraf3-overdrev ..."
    if arcpy.Exists(outPatch + "over370"):
      arcpy.Delete_management(outPatch + "over370")
      print "... sletter eksisterende raster"
    feat_class = "paragraf3"
    feat_layer = "over370"
    arcpy.MakeFeatureLayer_management(feat_class,feat_layer)
    arcpy.SelectLayerByAttribute_management(feat_layer, "NEW_SELECTION", '"OBJEKTKODE" = 3070')
    eucDisttmp = EucDistance("over370","","1","")
    over370 = Con(eucDisttmp < 3, 370, 1)
    over370.save(outPatch + "over370")

# 375 - p3 strandeng 3075 (seng375)
  if seng375_c == 1:
    print "Processerer paragraf3-strandeng ..."
    if arcpy.Exists(outPatch + "seng375"):
      arcpy.Delete_management(outPatch + "seng375")
      print "... sletter eksisterende raster"
    feat_class = "paragraf3"
    feat_layer = "seng375"
    arcpy.MakeFeatureLayer_management(feat_class,feat_layer)
    arcpy.SelectLayerByAttribute_management(feat_layer, "NEW_SELECTION", '"OBJEKTKODE" = 3075')
    eucDisttmp = EucDistance("seng375","","1","")
    seng375 = Con(eucDisttmp < 3, 375, 1)
    seng375.save(outPatch + "seng375")

# 380 - p3 s� 3080 (soe_380)
  if soe_380_c == 1:
    print "Processerer paragraf3-s� ..."
    if arcpy.Exists(outPatch + "soe_380"):
      arcpy.Delete_management(outPatch + "soe_380")
      print "... sletter eksisterende raster"
    feat_class = "paragraf3"
    feat_layer = "soe_380"
    arcpy.MakeFeatureLayer_management(feat_class,feat_layer)
    arcpy.SelectLayerByAttribute_management(feat_layer, "NEW_SELECTION", '"OBJEKTKODE" = 3080')
    eucDisttmp = EucDistance("soe_380","","1","")
    soe_380 = Con(eucDisttmp < 1, 380, 1)
    soe_380.save(outPatch + "soe_380")

# 440 - Ferskvandss�er
  if soer440_c == 1:
    print "Processerer s�er ..."
    if arcpy.Exists(outPatch + "soer440"):
      arcpy.Delete_management(outPatch + "soer440")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("soer", "FEAT_KODE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    soer440 = Con(rasIsNull == 1, 1, 440)
    soer440.save(outPatch + "soer440")
    # arcpy.Delete_management(outPatch + "tmpRaster")

# 425/435 - Sm� vandl�b (2.5-12) (vandloeb_brudt)+ buffer  OBS:  HUske at tilf�je de 'ukendte'
  if aaer435_c == 1:
    print "Processerer sm� vandl�b (0 - 2.5 meter)"
    if arcpy.Exists(outPatch + "aaer435"):
      arcpy.Delete_management(outPatch + "aaer435")
      print "... sletter eksisterende raster"
    if arcpy.Exists(outPatch + "aaer425"):
      arcpy.Delete_management(outPatch + "aaer425")
      print "... sletter eksisterende raster"
    feat_class = "vandloeb_brudt"
    feat_layer = "aaer415"
    arcpy.MakeFeatureLayer_management(feat_class,feat_layer)
    arcpy.SelectLayerByAttribute_management(feat_layer, "NEW_SELECTION", '"MIT_BREDDE" = \'0 - 2,5 m\'')
    arcpy.SelectLayerByAttribute_management(feat_layer, "ADD_TO_SELECTION", '"MIT_BREDDE" = \'Ukendt\'')
    eucDistTmp = EucDistance("aaer415","","1","")
    aaer435 = Con(eucDistTmp < 0.95, 435, 1)
    aaer435.save(outPatch + "aaer435")
    aaer425 = Con(eucDistTmp < 2.01, 425, 1)
    aaer425.save(outPatch + "aaer425")

# 426/436 - Mellemstore vandl�b (2.5-12) (vandloeb_brudt)+ buffer
  if aaer436_c == 1:
    print "Processerer mellemstore vandl�b (2.5 - 12 meter)"
    if arcpy.Exists(outPatch + "aaer436"):
      arcpy.Delete_management(outPatch + "aaer436")
      print "... sletter eksisterende raster"
    if arcpy.Exists(outPatch + "aaer426"):
      arcpy.Delete_management(outPatch + "aaer426")
      print "... sletter eksisterende raster"
    feat_class = "vandloeb_brudt"
    feat_layer = "aaer416"
    arcpy.MakeFeatureLayer_management(feat_class,feat_layer)
    arcpy.SelectLayerByAttribute_management(feat_layer, "NEW_SELECTION", '"MIT_BREDDE" = \'2,5 - 12 m\'')
    eucDistTmp = EucDistance("aaer416","","1","")
    aaer436 = Con(eucDistTmp < 5, 436, 1)
    aaer436.save(outPatch + "aaer436")
    aaer426 = Con(eucDistTmp < 7, 426, 1)
    aaer426.save(outPatch + "aaer426")

# 427/437 - Store vandl�b (> 12 meter) (vandloeb_brudt)+ buffer
  if aaer437_c == 1:
    print "Processerer store vandl�b (> 12 meter)"
    if arcpy.Exists(outPatch + "aaer437"):
      arcpy.Delete_management(outPatch + "aaer437")
      print "... sletter eksisterende raster"
    if arcpy.Exists(outPatch + "aaer427"):
      arcpy.Delete_management(outPatch + "aaer427")
      print "... sletter eksisterende raster"
    feat_class = "vandloeb_brudt"
    feat_layer = "aaer417"
    arcpy.MakeFeatureLayer_management(feat_class,feat_layer)
    arcpy.SelectLayerByAttribute_management(feat_layer, "NEW_SELECTION", '"MIT_BREDDE" = \'over 12 m\'')
    eucDistTmp = EucDistance("aaer417","","1","")
    aaer437 = Con(eucDistTmp < 5, 437, 1)
    aaer437.save(outPatch + "aaer437")
    aaer427 = Con(eucDistTmp < 7, 427, 1)
    aaer427.save(outPatch + "aaer427")

# 420 - randzoner om s�er (sat til 2 meter p� hver side (soer410)
  if sorn420_c == 1:
    print "Processerer randzoner om s�er ..."
    if arcpy.Exists(outPatch + "sorn420"):
      arcpy.Delete_management(outPatch + "sorn420")
      print "... sletter eksisterende raster"
    eucDistVejkant = EucDistance("soer","","1","")
    sorn420 = Con(eucDistVejkant < 2.05, 420, 1)
    sorn420.save(outPatch + "sorn420") 

# 505 - randzoner ved marker (sat til 1 meter p� hver side (mark505)
  if mark505_c == 1:
    print "Processerer markers randzoner ..."
    if arcpy.Exists(outPatch + "mark505"):
      arcpy.Delete_management(outPatch + "mark505")
      print "... sletter eksisterende raster"
    eucDistVejkant = EucDistance("MarkVJID12","","1","")
    mark505 = Con(eucDistVejkant < 1.05, 505, 1)
    mark505.save(outPatch + "mark505")

# mark1000 plus - konverterer markblokke s�ledes at hver markpolygon f�r sin unikke id
  if mark1000_c == 1:
    print "Processerer markpolygoner ..."
    if arcpy.Exists(outPatch + "mark1000"):
      arcpy.Delete_management(outPatch + "mark1000")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("MarkVJID12", "NYID", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    tmpIsNull = IsNull(outPatch + "tmpRaster")
    markNummerFirst = (Plus(outPatch + "tmpRaster", 0))
    markNummer = Int(markNummerFirst)
    mark10000 = Con(tmpIsNull == 1, 1, markNummer)
    mark10000.save(outPatch + "mark1000") 
   # arcpy.Delete_management(outPatch + "tmpRaster")

# 620 - konververing af diger (dige620)
  if dige620_c == 1:
    print "Processerer diger ..."
    if arcpy.Exists(outPatch + "dige620"):
      arcpy.Delete_management(outPatch + "dige620")
      print "... sletter eksisterende raster"
    eucDistTmp = EucDistance("dige", "", "1", "")
    dige620 = Con(eucDistTmp < 1.2, 620, 1)
    dige620.save(outPatch + "dige620")

# 625 - Fredede fortidsminder
  if fred625_c == 1:
    print "Processerer fredede fortidsminder ..."
    if arcpy.Exists(outPatch + "fred625"):
      arcpy.Delete_management(outPatch + "trae640")
      print "... sletter eksisterende raster"
    eucDistTmp = EucDistance("fred_fortid", "", "1", "")
    fred625 = Con(eucDistTmp < 6, 625, 1)
    fred625.save(outPatch + "fred625")

# 630 - Rekreative omr�der
  if rekr630_c == 1:
    print "Processerer rekreative omr�der ..."
    if arcpy.Exists(outPatch + "rekr630"):
      arcpy.Delete_management(outPatch + "rekr630")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("rekromr", "FEAT_KODE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    rekr630 = Con(rasIsNull == 1, 1, 630)
    rekr630.save(outPatch + "rekr630")
    arcpy.Delete_management(outPatch + "tmpRaster")

# 635 - konververing af levende hegn (hegn635)
  if hegn635_c == 1:
    print "Processerer levende hegn ..."
    if arcpy.Exists(outPatch + "hegn635"):
      arcpy.Delete_management(outPatch + "hegn635")
      print "... sletter eksisterende raster"
    eucDistTmp = EucDistance("hegn", "", "1", "")
    hegn635 = Con(eucDistTmp < 2, 635, 1)
    hegn635.save(outPatch + "hegn635")


# 640 - konvertering af enkeltst�ende tr�grupper (trae640)
  if trae640_c == 1:
    print "Processerer grupper af enkeltst�ende tr�er ..."
    if arcpy.Exists(outPatch + "trae640"):
      arcpy.Delete_management(outPatch + "trae640")
      print "... sletter eksisterende raster"
    eucDistTmp = EucDistance("traegruppe", "", "1", "")
    trae640 = Con(eucDistTmp < 8, 640, 1)
    trae640.save(outPatch + "trae640")

# 641 - konvertering af enkeltst�ende tr�er (trae641)
  if trae641_c == 1:
    print "Processerer enkeltst�ende tr�er ..."
    if arcpy.Exists(outPatch + "trae641"):
      arcpy.Delete_management(outPatch + "trae641")
      print "... sletter eksisterende raster"
    eucDistTmp = EucDistance("trae", "", "1", "")
    trae641 = Con(eucDistTmp < 4, 641, 1)
    trae641.save(outPatch + "trae641")

# 650- Raastofudvinding
  if raas650_c == 1:
    print "Processerer omr�der med r�stofudvinding ..."
    if arcpy.Exists(outPatch + "raas650"):
      arcpy.Delete_management(outPatch + "raas650")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("raastof", "FEAT_KODE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    raas650 = Con(rasIsNull == 1, 1, 650)
    raas650.save(outPatch + "raas650")
    arcpy.Delete_management(outPatch + "tmpRaster")

# 1100- AIS kortl�gning til at udfylde baggrund
  if ais1100_c == 1:
    print "Processerer AIS arealanvendelseskort ..."
    if arcpy.Exists(outPatch + "ais1100"):
      arcpy.Delete_management(outPatch + "ais1100")
      print "... sletter eksisterende raster"
    arcpy.PolygonToRaster_conversion("ais100", "LUATYPE", outPatch + "tmpRaster", "CELL_CENTER", "NONE", "1")
    rasIsNull = IsNull(outPatch + "tmpRaster")
    ais1100 = Con(rasIsNull == 1, 1, outPatch + "tmpRaster")
    ais1100.save(outPatch + "ais1100")
    arcpy.Delete_management(outPatch + "tmpRaster")

# MOSAIC

  if vejnet_c == 1:   #Sammens�tter et vejnettema med alle vejtyper og kanter
    print "Processerer vejnet-tema ..."
    if arcpy.Exists(outPatch + "T1_vejnet"):
      arcpy.Delete_management(outPatch + "T1_vejnet")
      print "... sletter eksisterende raster"
    vejRastList = [Raster ("vejk110"), Raster ("stie112"), Raster ("spor115"), Raster ("hjsp150"), Raster ("vind155"),
                   Raster ("jern120"), Raster ("vu30122"), Raster ("vu60125"), Raster ("vu90130"), Raster ("park114"), Raster("landhav")]
    vejnet = CellStatistics(vejRastList, "MAXIMUM", "DATA")    
    
#  brug n�ste linje hvis vejnettet �nskes pre-shrinked - husk at �ndre navn til vejnet0 ovenfor f�rst 
#    vejnet = Shrink(vejnet0, prelShrinking, 1)
    vejnet.save (outPatch + "T1_vejnet")
    
  if bebyggelser_c == 1:   #Sammens�tter et bebyggelsestema 
    print "Processerer bebyggelser ..."
    if arcpy.Exists(outPatch + "T2_bebyggelser"):
      arcpy.Delete_management(outPatch + "T2_bebyggelser")
      print "... sletter eksisterende raster"
    bebyggelserRastList = [Raster ("lavb205"), Raster ("hojb210"), Raster ("byke215"), Raster ("kirk225"), Raster ("bygn250"), Raster ("sprt230"), Raster ("indu220"), Raster ("landhav")]
    bebyggelser = CellStatistics(bebyggelserRastList, "MAXIMUM", "DATA")
    bebyggelser.save (outPatch + "T2_bebyggelser")

  if natur_c == 1:   #Sammens�tter et naturtema 
    print "Processerer naturomr�der ..."
    if arcpy.Exists(outPatch + "T3_natur"):
      arcpy.Delete_management(outPatch + "T3_natur")
      print "... sletter eksisterende raster"
    naturRastList = [Raster ("skrt105"), Raster ("skov310"), Raster ("krat315"), Raster ("sand320"), Raster ("hede325"), Raster ("vaad330"),
                     Raster ("eng_355"), Raster ("hede360"), Raster ("mose365"), Raster ("over370"), Raster ("seng375"), Raster ("soe_380"), Raster ("landhav")]
    natur = CellStatistics(naturRastList, "MAXIMUM", "DATA")
    natur.save (outPatch + "T3_natur")  

  if vaadnatur_c == 1:   #Sammens�tter et v�dt naturtema 
    print "Processerer naturomr�der ..."
    if arcpy.Exists(outPatch + "T3_vaadnatur"):
      arcpy.Delete_management(outPatch + "T3_vaadnatur")
      print "... sletter eksisterende raster"
    vaadnaturRastList = [Raster ("mose365"), Raster ("soe_380"), Raster ("landhav")]
    vaadnatur = CellStatistics(vaadnaturRastList, "MAXIMUM", "DATA")
    vaadnatur.save (outPatch + "T3_vaadnatur")  

  if ferskvand_c == 1:   #Sammens�tter et vandtema 
    print "Processerer vandl�b og s�er ..."
    if arcpy.Exists(outPatch + "T4_vand"):
      arcpy.Delete_management(outPatch + "T4_vand")
      print "... sletter eksisterende raster"
    vandRastList = [Raster ("soer440"), Raster ("aaer435"), Raster ("aaer436"), Raster ("aaer437"),
                    Raster ("sorn420"), Raster ("aaer425"), Raster ("aaer426"), Raster ("aaer427"), Raster ("landhav")]
    vand = CellStatistics(vandRastList, "MAXIMUM", "DATA")
    vand.save (outPatch + "T4_vand")

  if kultur_c == 1:   # Sammens�tter et kulturtema
    print "Processerer hegn, tr�er og kulturspor ..."
    if arcpy.Exists(outPatch + "T5_kultur"):
      arcpy.Delete_management(outPatch + "T5_kultur")
      print "... sletter eksisterende raster"
      # "fred625" = fredede fortidsminder
    kulturRastList = [Raster ("dige620"), Raster ("fred625"), Raster ("rekr630"), Raster ("hegn635"), Raster ("trae640"), Raster ("trae641"),
                    Raster ("raas650"), Raster ("landhav")]
      # Denne r�kkef�lge burde give mere mening.               
    kultur = CellStatistics(kulturRastList, "MAXIMUM", "DATA")
    kultur.save (outPatch + "T5_kultur")

  gc.collect()  # Test af om dette kan afhj�lpe huskommelsesproblemer.

  if mosaik_c == 1:   # S�tter den endelige mosaik sammen af de fem temalag - her styres hvad der har prioritet over hvad
    print "Processerer mosaik for alle temaer ..."
    if arcpy.Exists(outPatch + "Mosaik_rekl"):
      arcpy.Delete_management(outPatch + "Mosaik_rekl")
    if arcpy.Exists(outPatch + "Mosaik_raa"):
      arcpy.Delete_management(outPatch + "Mosaik_raa")
    if arcpy.Exists(outPatch + "Mosaik_almass"):
      arcpy.Delete_management(outPatch + "Mosaik_almass")
      
    print "... sletter eksisterende raster"

 #  Dette script l�gger de fem temaer sammen til en samlet mosaik. Sciptet styrer r�kkef�lgen
 #  og hvad, der fortr�nger hvad
 
    T1ve = Raster("T1_vejnet")
    T2be = Raster("T2_bebyggelser")
    T3na = Raster("T3_natur")
    T3ana = Raster("T3_vaadnatur")
    T4va = Raster("T4_vand")
    T5ku = Raster("T5_kultur")
    ais1100 = Raster("ais1100")  
    mark1 = Raster("mark505")    # markrand
    mark2 = Raster("mark1000")   # egentlige marker (eller markblokke)
    landhav = Raster("landhav")

    step1 = Con(mark2 > 999, mark2, 1)          # marklag l�gges p� f�rst
    print "marklag ..."
    step2 = Con(T4va == 1, step1, T4va)            # l�gger vand oven p�
    print "vand ..."
    step3 = Con(step2 == 1, T3na, step2)            # l�gger natur p� det som ikke er vand eller marklag
    print "natur ..."
    step4 = Con(step3 == 1, T2be, step3)            # l�gger bebyggelser p� det som ikke er vand, marklag eller natur
    print "bebyggelse ..."
    step4a = Con(T3ana == 1, step4, T3ana)            # l�gger vaad natur oven p� alt andet
    print "vaad natur ..."
    step5 = Con(T5ku == 1, step4a, T5ku)             # l�gger kultur over alt andet
    print "kultur ..."
    step6 = Con(T1ve == 1, step5, T1ve)             # l�gger veje over alt andet
    print "veje ..."
    mosaik01 = Con(landhav == 1, step6, 0)          # l�gger hav p�
    print "hav ..."
    
    mosaik1 = Con(mosaik01 == 1, ais1100, mosaik01) # l�gger ais-laget, hvor der ikke er andet
    mosaik1.save (outPatch + "Mosaik_raa")
    nowTime = time.strftime('%X %x')
    print "R� mosaik f�rdig ..." + nowTime

# Reclassify to ALMaSS raster values
# Her overs�ttes raster v�rdierne s� de matcher med dem ALMaSS bruger

    mosaik2 = ReclassByASCIIFile(mosaik1, "E:/Landskabsgenerering/landskaber/Vejlerne/reclassification_31juli2014.txt", "DATA")
    mosaik2.save(outPatch + "Mosaik_rekl")
    nowTime = time.strftime('%X %x')
    print "Reklassificering f�rdig ..." + nowTime

    regionALM = RegionGroup(mosaik2,"EIGHT","WITHIN","ADD_LINK","")
    if arcpy.Exists(outPatch + "Mosaik_almass"):
        arcpy.Delete_management(outPatch + "Mosaik_almass")
        print " * Deleting existing Mosaik_almass  "
    regionALM.save(outPatch + "Mosaik_almass")
    nowTime = time.strftime('%X %x')
    print "Regionalisering f�rdig ..." + nowTime

# Write the ASCII file needed for ALMaSS:
# Make sure to keep the file name - the program to make the lsb file will be looking for it.
    arcpy.RasterToASCII_conversion(regionALM, asciisti)

  endTime = time.strftime('%X %x')
  print ""
  print "Done: " + endTime



except:
    tb = sys.exc_info()[2]
    tbinfo = traceback.format_tb(tb)[0]
    pymsg = "PYTHON ERRORS:\nTraceback Info:\n" + tbinfo + "\nError Info:\n     " +        str(sys.exc_type) + ": " + str(sys.exc_value) + "\n"
    msgs = "ARCPY ERRORS:\n" + arcpy.GetMessages(2) + "\n"

    arcpy.AddError(msgs)
    arcpy.AddError(pymsg)

    print msgs
    print pymsg

    arcpy.AddMessage(arcpy.GetMessages(1))
    print arcpy.GetMessages(1)



