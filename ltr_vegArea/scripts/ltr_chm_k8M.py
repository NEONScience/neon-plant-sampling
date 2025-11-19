# -*- coding: utf-8 -*-
"""
Created on Mon Nov 11 13:18:41 2024

@author: kmurphy
"""


import sys
import os
import matplotlib.pyplot as plt
import numpy as np
import h5py, os, copy
import rasterio as rio
from rasterio.transform import Affine
from rasterio.merge import merge
# from NIS.lib.Python.aop_processing_utils_K8M import *
# #from aop_processing_utils_K8M import *
# from Lidar.lib.Python.aop_utils_K8M import *
from multiprocessing import Pool
from functools import partial
import pandas as pd
import math
import time
import glob
import gdal2tiles
from simpledbf import Dbf5
import re
from collections import Counter
import shutil
#import rioxarray


#currentFile = os.path.abspath(os.path.join(__file__))
#nisLibDir = os.path.join(currentFile.lower().split('gold_pipeline')[0], 'Gold_Pipeline','ProcessingPipelines', 'lib', 'Python')

nisLibDir = r'C:\Gold_Pipeline\ProcessingPipelines\lib\Python'

if nisLibDir not in sys.path:
    sys.path.insert(0, nisLibDir)
    
from aop_processing_utils import *
from aop_gcs_download import *

def compileYSV(sites, years):
    ysvL = []
    for site in sites:
        for year in years:
            if (site == 'ALL'):
                if (year == 'ALL'):
                    YearSiteVisitNameList = dl.list_all_ysvs()
                    ysvL.append(YearSiteVisitNameList)
                else:
                    YearSiteVisitNameList = dl.list_sites_by_year(year)
                    ysvL.append(YearSiteVisitNameList)
            else:
                if (year == 'ALL'):
                    YearSiteVisitNameList = dl.list_all_ysvs(site)
                    ysvL.append(YearSiteVisitNameList)
                else:
                    ysvl = dl.list_all_ysvs(site)
                    YearSiteVisitNameList = [ysv for ysv in ysvl if year in ysv]
                    ysvL.append(YearSiteVisitNameList)
    flattened = [value for sublist in ysvL for value in sublist]
    return flattened

def list_tiles_to_download(site, csv):
    print("Reading in Plot Centroid Data")
    df = pd.read_csv(csv)
    #df_filtered = df[df['appMods'].str.contains("cfc")]
    
    print("Determining tiles to download")
    #centroids_site = df_filtered[df_filtered['siteID'].str.contains(site.site_name)]
    centroids_site = df[df['siteID'].str.contains(site.site)]
    #centroids_site = df[df['siteID'].str.contains("TREE")]
    if centroids_site.shape[0] == 0:
        print("Site not included in csv")
    #centroids_site_buffer = addBuffer(centroids_site, 20)
    easting = centroids_site['easting_calc']
    northing = centroids_site['northing_calc']   
    buffer = int(np.sqrt((centroids_site['plotArea']).iat[0])/2)
    
    # get the tile corners for the coordinates
    tileEasting = np.floor(easting/1000)*1000
    tileNorthing = np.floor(northing/1000)*1000
    
    # add & subtract buffer (buffer is a square)
    eastingPlus = np.floor((easting + buffer)/1000)*1000
    eastingMinus = np.floor((easting - (buffer-1))/1000)*1000
    northingPlus = np.floor((northing + buffer)/1000)*1000
    northingMinus = np.floor((northing - (buffer-1))/1000)*1000

    # get coordinates where buffer overlaps another tile
    eastingPlusMatch = tileEasting==eastingPlus
    eastingMinusMatch = tileEasting==eastingMinus
    northingPlusMatch = tileNorthing==northingPlus
    northingMinusMatch = tileNorthing==northingMinus
    matchMat = pd.concat([eastingMinusMatch, eastingPlusMatch, northingMinusMatch, northingPlusMatch], axis = 1)
    matchMat = 1*matchMat
    matchMatStr = matchMat.astype(str)
    matchMatJoined = matchMatStr.apply('.'.join, axis=1)

    # add coordinates for overlapping tiles
    for k in range(0,len(easting)):
        pos = matchMatJoined.iat[k]
        #print(pos)
        if(pos=="1.1.1.1"):
            continue
        else :
            if(pos=="0.1.1.1") :
                tileEasting = tileEasting.append(pd.Series(eastingMinus.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(tileNorthing.iat[k]), ignore_index = True)
            if(pos=="1.0.1.1") :
                tileEasting = tileEasting.append(pd.Series(eastingPlus.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(tileNorthing.iat[k]), ignore_index = True)
          
            if(pos=="0.1.0.1") :
                tileEasting = tileEasting.append(pd.Series(eastingMinus.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(tileNorthing.iat[k]), ignore_index = True)
                tileEasting = tileEasting.append(pd.Series(eastingMinus.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(northingMinus.iat[k]), ignore_index = True)
                tileEasting = tileEasting.append(pd.Series(tileEasting.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(northingMinus.iat[k]), ignore_index = True)
            if(pos=="0.1.1.0") :
                tileEasting = tileEasting.append(pd.Series(eastingMinus.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(tileNorthing.iat[k]), ignore_index = True)
                tileEasting = tileEasting.append(pd.Series(eastingMinus.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(northingPlus.iat[k]), ignore_index = True)
                tileEasting = tileEasting.append(pd.Series(tileEasting.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(northingPlus.iat[k]), ignore_index = True)
            if(pos=="1.0.0.1") :
                tileEasting = tileEasting.append(pd.Series(eastingPlus.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(tileNorthing.iat[k]), ignore_index = True)
                tileEasting = tileEasting.append(pd.Series(eastingPlus.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(northingMinus.iat[k]), ignore_index = True)
                tileEasting = tileEasting.append(pd.Series(tileEasting.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(northingMinus.iat[k]), ignore_index = True)
            if(pos=="1.0.1.0") :
                tileEasting = tileEasting.append(pd.Series(eastingPlus.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(tileNorthing.iat[k]), ignore_index = True)
                tileEasting = tileEasting.append(pd.Series(eastingPlus.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(northingPlus.iat[k]), ignore_index = True)
                tileEasting = tileEasting.append(pd.Series(tileEasting.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(northingPlus.iat[k]), ignore_index = True)
            if(pos=="1.1.0.1") :
                tileEasting = tileEasting.append(pd.Series(tileEasting.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(northingMinus.iat[k]), ignore_index = True)
            if(pos=="1.1.1.0") :
                tileEasting = tileEasting.append(pd.Series(tileEasting.iat[k]), ignore_index = True)
                tileNorthing = tileNorthing.append(pd.Series(northingPlus.iat[k]), ignore_index = True)
    tilesDict = {"tileEasting": tileEasting,
                 "tileNorthing": tileNorthing}
    tiles = (pd.concat(tilesDict, axis = 1)).astype(int)
    tiles.drop_duplicates(inplace=True)
    tileNames = "NEON_"+site.domain+"_" + site.site + "_DP3_" + tiles['tileEasting'].apply(str) + '_' + tiles['tileNorthing'].apply(str)
    #tileNames = "NEON_"+site.domain+"_TREE_DP3_" + tiles['tileEasting'].apply(str) + '_' + tiles['tileNorthing'].apply(str)
    return tileNames
    

def addBuffer(df, halfPlotSize):
    df = df.astype({"easting_calc": int, "northing_calc":int})
    df['east'] = df['easting_calc'].apply(lambda x : x-(halfPlotSize))
    df['west'] = df['easting_calc'].apply(lambda x : x+(halfPlotSize))
    df['north'] = df['northing_calc'].apply(lambda x : x+(halfPlotSize))
    df['south'] = df['northing_calc'].apply(lambda x : x-(halfPlotSize))
    #df = df.drop(['plotSize', 'easting', 'northing'], axis = 1)
    return df
    

def addBufferToSubplot(centroidsCSV, site):
    centroids = pd.read_csv(centroidsCSV)
    easting = centroids['easting_calc']
    northing = centroids['northing_calc']
    chmSubPlots = centroids[centroids['siteID'].str.contains(site)]
    bufferedCentroids = addBuffer(chmSubPlots, 10)
    return bufferedCentroids    


def calculateCHMpct(ChmMosaic, bufferPlots, YearSiteVisit, CHMpctOutDir):             
    newAbsMosaic = np.zeros_like(ChmMosaic)
    newAbsMosaic[newAbsMosaic == 0] = 'nan'
     
    print(str(len(bufferPlots)) + " plots")
    for b in range(len(bufferPlots)):
        #print(str(b+1) + " of " + str(len(bufferPlots)))
        plot = bufferPlots.iloc[b]

        try:
            upperIndex = int(YearSiteVisit.MosaicExtents[3]-plot['north'])
            leftIndex = int(plot['east']-YearSiteVisit.MosaicExtents[0])
            rightIndex = int(plot['west']-YearSiteVisit.MosaicExtents[0])
            lowerIndex = int(YearSiteVisit.MosaicExtents[3]-plot['south'])
            tempCHMarray = ChmMosaic[upperIndex:lowerIndex,leftIndex:rightIndex]
            newAbsMosaic[upperIndex:lowerIndex,leftIndex:rightIndex] = tempCHMarray
            bufferPlots['CHM_area'].iloc[b] = (tempCHMarray > 2).sum()
            del upperIndex, leftIndex, lowerIndex, rightIndex, tempCHMarray
        except:
            print("Failed on " + plot.namedLocation)
    
    if(os.path.isfile(CHMpctOutDir+'\\'+YearSiteVisit.YearSiteVisit+'_CHMMosaic_Absolute.tif')):
        print("CHM mosaic exists")
    else:
        YearSiteVisit.MosaicDataType = np.float32
        YearSiteVisit.writeMosaicFromProductClass(CHMpctOutDir+'\\'+YearSiteVisit.YearSiteVisit+'_CHMMosaic_Absolute.tif',newAbsMosaic)         
    return bufferPlots
     

def getMapExtentsForMosiac(chmFolder, ysvc):
    Files = os.listdir(chmFolder)
    FileName, FileExtension = os.path.splitext(Files[0])

    if FileExtension == '.tif':
        RasterIoFilesToMosaicInfo = []
        for File in Files:
            src = rio.open(chmFolder + '/'+ File)
            RasterIoFilesToMosaicInfo.append(src)
            src.close()
        xs = []
        ys = []
        for src in RasterIoFilesToMosaicInfo:
            left, bottom, right, top = src.bounds
            xs.extend([left, right])
            ys.extend([bottom, top])
            ExtentW, ExtentS, ExtentE, ExtentN = min(xs), min(ys), max(xs), max(ys)
            
        if RasterIoFilesToMosaicInfo[0].crs.is_epsg_code:
            ysvc.EPSGCode = int(RasterIoFilesToMosaicInfo[0].crs['init'].lstrip('epsg:'))
        AffineTransform = RasterIoFilesToMosaicInfo[0].transform
        
        ysvc.RasterCellHeight = np.abs(AffineTransform[4])
        ysvc.RasterCellWidth = np.abs(AffineTransform[0])
        ysvc.MosaicExtents = np.array((ExtentW, ExtentS, ExtentE, ExtentN),dtype=np.float) 
        ysvc.NoData = src.nodata
        
    if FileExtension == '.h5':
        NeonH5FilesToMosaicInfo = []
        for File in Files:
            reflArray, metadata, wavelengths = h5refl2array(File)
            NeonH5FilesToMosaicInfo.append(metadata)
        xs = []
        ys = []
        for Metadata in NeonH5FilesToMosaicInfo:
            left, bottom, right, top = Metadata['ext_dict']['xMin'], Metadata['ext_dict']['yMin'], Metadata['ext_dict']['xMax'], Metadata['ext_dict']['yMax']
            xs.extend([left, right])
            ys.extend([bottom, top])
            ExtentW, ExtentS, ExtentE, ExtentN = min(xs), min(ys), max(xs), max(ys)

        ysvc.EPSGCode = metadata['EPSG']     
        ysvc.MosaicExtents = np.array((ExtentW, ExtentS, ExtentE, ExtentN),dtype=np.float)
        ysvc.RasterCellHeight = int(float(metadata['res']['pixelHeight']))
        ysvc.RasterCellWidth = int(float(metadata['res']['pixelWidth']))
        ysvc.NoData = metadata['noDataVal'] 
    return ysvc
            
def canopyHeightPercentage(YearSiteVisitName, csvOfSubplotCentroids, dl, DFcalcCHM):  
    ysvc = YearSiteVisitClass(YearSiteVisitName, skipMissions=True)
    site = ysvc.site
    #print("Determining tiles to download")
    tileNames = list_tiles_to_download(site, csvOfSubplotCentroids)
    print(tileNames)
    # tileNames = ['NEON_D05_STEI_DP3_297000_5042000', 'NEON_D05_STEI_DP3_297000_5041000', 'NEON_D05_STEI_DP3_297000_5040000', 'NEON_D05_STEI_DP3_298000_5042000',
    #              'NEON_D05_STEI_DP3_298000_5041000', 'NEON_D05_STEI_DP3_298000_5040000']
    
    print("Downloading canopy height data")
    if os.path.isdir("D:/" + ysvc.year + "/FullSite/" + site.domain + "/" + YearSiteVisitName + "/L3/DiscreteLidar/CanopyHeightModelGtif"):
        print("Already downloaded")
    else:
        if site.site == 'TREE':
            tempYSV = YearSiteVisitName.replace("TREE", "STEI")
            for t in list(tileNames):
                #ysvc.downloadL3LidarChm(keyword = t)
                ysvc.dl.download_aop_object(ysvc.LidarL3ChmDir,Ext='tif', keyword = t)
            ysvc.downloadL3LidarChm()
            #dl.download_specific_tiles(tempYSV,'L3/DiscreteLidar/CanopyHeightModelGtif/',tileNames,'.tif')
        else:
            for t in list(tileNames):
                #ysvc.downloadL3LidarChm(keyword = t)
                ysvc.dl.download_aop_object(ysvc.LidarL3ChmDir,Ext='tif', keyword = t)
            #dl.download_specific_tiles(YearSiteVisitName,'L3/DiscreteLidar/CanopyHeightModelGtif/',tileNames,'.tif')
    
    #Establish folders
    #chmFolder = site.root_dir + "/L3/DiscreteLidar/CanopyHeightModelGtif/"
    chmFolder = ysvc.LidarL3ChmDir
    CHMpctOutDir = os.path.split(chmFolder)[0] +"\\CanopyHeightPct"
    os.makedirs(CHMpctOutDir, exist_ok=True)
    
    # GET EXTENT FOR EACH PLOT AND EXTRACT
    bufferPlots = addBufferToSubplot(csvOfSubplotCentroids, site.site)
    #bufferPlots = addBufferToSubplot(csvOfSubplotCentroids, "TREE")
    print("Plots read in")
    
    print("Getting map extents")
    ysvc = getMapExtentsForMosiac(chmFolder, ysvc)
    
    print("Getting mosaics")
    fullFiles = [os.path.join(chmFolder, f) for f in os.listdir(chmFolder)]
#    ChmMosaic = ysvc.getMosaicAndExtents([chmFolder + s for s in os.listdir(chmFolder)], 'tiles')
#    ysvc.generateMosaic([chmFolder + s for s in os.listdir(chmFolder)],CHMpctOutDir+'\\'+YearSiteVisitName+'_CHM.tif')
    #ysvc.generateLidarChmMosaic([chmFolder + s for s in os.listdir(chmFolder)],CHMpctOutDir+'\\'+YearSiteVisitName+'_CHM.tif')
    #generateMosaic(fullFiles,CHMpctOutDir)
    ysvc.getProductFiles()
    ysvc.generateLidarChmMosaic()
    QaFile = ysvc.LidarQaDir + '\\ProcessingReport\\'+YearSiteVisitName + '_CHM_mosaic.tif'
    shutil.copy(QaFile, CHMpctOutDir)         
    #ChmMosaic = rioxarray.open_rasterio(QaFile)
    with rio.open(QaFile) as src: 
        arr = src.read()
    ChmMosaic = arr.reshape(-1, arr.shape[-1])

    print("calculate CHM percentage")
    bufferCalc = calculateCHMpct(ChmMosaic, bufferPlots, ysvc, CHMpctOutDir)
    bufferCalc['CHM_percent'] = (bufferCalc['CHM_area']/bufferCalc['plotArea']*100)
    bufferCalc['Year'] = ysvc.year
    try:      
        missions = dl.list_missions_from_product(YearSiteVisitName)
        missions.sort()
        print("Add Missions: ", missions)
        # = ['2022051712', '2022053113']
        bufferCalc['startDate'] = missions[0][0:4]+"-"+missions[0][4:6]+"-"+missions[0][6:8]+"T"+missions[0][8:10]+":00:00.000Z"
        bufferCalc['endDate'] = missions[-1][0:4]+"-"+missions[-1][4:6]+"-"+missions[-1][6:8]+"T"+missions[-1][8:10]+":00:00.000Z"
    except:
        bufferCalc['startDate'] = 'NA'
        bufferCalc['endDate'] = 'NA'

    #YYYY-MM-DDT00:00:00.000Z
    bufferCalc = bufferCalc.drop(columns = ['east', 'west', 'north', 'south'])
    DFcalcCHM = DFcalcCHM.append(bufferCalc)
    #bufferCalc.to_csv("C:/Users/kmurphy/Documents/neon-plant-sampling/ltr_vegArea/TREE2016_CalculatedCHMpct.csv")
        
    return YearSiteVisitName, DFcalcCHM

def Filter(string, substr):
    return [str for str in string if
            any(sub in str for sub in substr)]

if __name__ == '__main__':

    dl = DownloadClass()
       
    sites = ['ALL']
    years = ['2025']
    
    csvOfSubplotCentroids = "C:/Users/kmurphy/Documents/neon-plant-sampling/ltr_vegArea/allLitterPlots.csv"
    #csvOfSubplotCentroids = "C:/Users/kmurphy/Documents/Git/neon-plant-sampling/ltr_vegArea/testDat.csv"

    
    YearSiteVisitNameList = compileYSV(sites, years)
    print(YearSiteVisitNameList)
    #YearSiteVisitNameList = ['2016_STEI_1', '2017_STEI_2', '2019_STEI_3', '2020_STEI_4', '2022_STEI_5', '2024_STEI_6']
    #YearSiteVisitNameList = ['2024_SRER_6', '2024_NIWO_6', '2024_CLBJ_8', '2024_JERC_8', '2024_DELA_8', '2024_CPER_9', '2024_STER_5', '2024_UNDE_6', '2024_SJER_7', '2024_RMNP_6', '2024_SOAP_8', '2024_KONZ_10', '2024_LENO_8', '2024_PRIN_7', '2024_STEI_6', '2024_UKFS_8', '2024_KONZ_9', '2024_CHEQ_8', '2024_OAES_8', '2024_TALL_8', '2024_TEAK_7', '2024_WLOU_5', '2024_DEJU_6', '2024_MCDI_4']
    
    # Remove aquatic and plains sites
    ignoreSite = ['ARIK', 'BARR', 'BLUE', 'CHEQ', 'CPER', 'CUPE', 'DSNY', 'GUIL',
                  'HOPB', 'JORN', 'KTHF', 'LAJA', 'LIRO', 'MCDI', 'MCRA', 'MOAB',
                  'MRMF', 'MOAB', 'NOGP', 'OAES', 'ONAQ', 'PRIN', 'REDB', 'SAWB',
                  'STER', 'SYCA', 'TEPF', 'TOOL', 'WLOU', 'WOOD', 'WTSF']
    
    for word in list(YearSiteVisitNameList):
        if word in ignoreSite:
            del YearSiteVisitNameList[word]
    
    removeYSVNL = Filter(YearSiteVisitNameList, ignoreSite)
    subYSVNL = [i for i in YearSiteVisitNameList if i not in removeYSVNL]
        
    ysv_completed = []
    ysv_fail = []
    DFcalcCHM = pd.DataFrame()
    i = 1
    
    for i in range(len(subYSVNL)):
        YearSiteVisitName = subYSVNL[i]
        start_time = time.time()
        try:
            ysv_comp, DFcalcCHM = canopyHeightPercentage(YearSiteVisitName, csvOfSubplotCentroids, dl, DFcalcCHM)
            ysv_completed.append(ysv_comp)
            print("Completed: " + YearSiteVisitName)
            print("--- %s seconds ---" % (time.time() - start_time))
        except:
            ysv_fail.append(YearSiteVisitName)
            print("Failed: " + YearSiteVisitName)
            print("--- %s seconds ---" % (time.time() - start_time))
    #Redo
    ysv_completed_redo = []
    ysv_fail_redo = []
    for i in range(len(ysv_fail)):
        YearSiteVisitName = ysv_fail[i]
        start_time = time.time()
        try:
            ysv_comp, DFcalcCHM = canopyHeightPercentage(YearSiteVisitName, csvOfSubplotCentroids, dl, DFcalcCHM)
            ysv_completed_redo.append(ysv_comp)
            print("Completed: " + YearSiteVisitName)
            print("--- %s seconds ---" % (time.time() - start_time))
        except:
            ysv_fail_redo.append(YearSiteVisitName)
            print("Failed: " + YearSiteVisitName)
            print("--- %s seconds ---" % (time.time() - start_time))
    # DFcalcCHM.to_csv("C:/Users/kmurphy/Documents/Git/neon-plant-sampling/ltr_vegArea/allLitterPlots_CalculatedCHMpct_wDatesUpdated_v2.csv")
    DFcalcCHM.to_csv("C:/Users/kmurphy/Documents/neon-plant-sampling/ltr_vegArea/allLitterPlots_2025_CalculatedCHMpct.csv")


# Calculate the number of times each site has been an issue and record the YSV number
    siteSplit = []
    for x in range(len(ysv_fail)):
        siteSplit.append(ysv_fail[x].split("_", 1)[1][0:4])
    siteErrorCounts = Counter(siteSplit)
    
    ignoreSite = ['ARIK', 'BARR', 'BLUE', 'CHEQ', 'CPER', 'CUPE', 'DSNY', 'GUIL',
                  'HOPB', 'JORN', 'KTHF', 'LAJA', 'LIRO', 'MCDI', 'MCRA', 'MOAB',
                  'MRMF', 'MOAB', 'NOGP', 'OAES', 'ONAQ', 'PRIN', 'REDB', 'SAWB',
                  'STER', 'SYCA', 'TEPF', 'TOOL', 'WLOU', 'WOOD', 'WTSF']
    
    for word in list(siteErrorCounts):
        if word in ignoreSite:
            del siteErrorCounts[word]
            
    substr = ['ORNL', 'SOAP', 'SERC']
    print(Filter(ysv_fail, substr))    