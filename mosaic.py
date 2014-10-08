# -*- coding: cp1252 -*-
# Name: mosaik vs. 1
# Form�l: Dette script laver en fladed�kkende mosaik fra de enkelte raster temaer
# Flemming Skov - februar - 2013
# Sidst opdateret: 31. juli 2014 - Export af ASCII fil tilf�jet - LD.
# -"-                             VIGTIGT: simpel mosaik uden brug af den tidsforbrugende statistikprocedure - burde ikke v�re n�dvendig med AIS!
# -"-                             regionGROUP �ndret til EIGHT fra FOUR i sidste step. 
# -"-                             Klip kanterne af rasteren for at undg� hakker tilf�jet inden export af ASCII.
 
# IMPORT SYSTEM MODULES
import arcpy, traceback, sys, time, gc
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")
nowTime = time.strftime('%X %x')
print "Mosaikproces p�begyndt: " + nowTime
print "... systemmoduler indl�st"

# DATABASER - HER S�TTES LINKS TIL DIVERSE DATABASER  
outPatch = "E:/Landskabsgenerering/landskaber/Vejlerne/vejlerne.gdb/"                    # Her gemmes de f�rdige rasterkort
localSettings = "E:/Landskabsgenerering/landskaber/Vejlerne/project.gdb/vejlerne_mask"   # Projektfolder med mask og extent
gisDB = "E:/Landskabsgenerering/landskaber/Vejlerne/vejlerne.gdb"                        # Her findes feature gis lagene
scratchDB = "E:/LandskabsGenerering/landskaber/Vejlerne/scratch"  
asciisti = "E:/Landskabsgenerering/landskaber/Vejlerne/ASCII_Input.txt"                     # Folder til tempor�re raster

prelShrinking = 1  # benyttes hvis vejlaget T1 skal udvides

arcpy.env.overwriteOutput = True
arcpy.env.workspace = gisDB
arcpy.env.scratchWorkspace = scratchDB
arcpy.env.extent = localSettings
arcpy.env.mask = localSettings
arcpy.env.cellSize = localSettings
print "... geoprocessing settings indl�st"

# VIGTIGT - styrer hvilke processer, der bliver gennemf�rt: '1' k�res scriptet; '0' g�r det ikke

default = 1

vejnet_c = default
bebyggelser_c = default
natur_c = default
vaadnatur_c = default
ferskvand_c = default
kultur_c = default
mosaik_c = default

print " "

#####################################################################################################

try:

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
  print "Konvertering f�rdig: " + endTime

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


