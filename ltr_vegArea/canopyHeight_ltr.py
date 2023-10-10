# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import matplotlib.pyplot as plt
import numpy as np
import h5py, os, copy
os.chdir('D:/Gold_Pipeline/ProcessingPipelines')
import rasterio as rio
from rasterio.transform import Affine
from rasterio.merge import merge
from NIS.lib.Python.aop_processing_utils_K8M import *
#from aop_processing_utils_K8M import *
from Lidar.lib.Python.aop_utils_K8M import *
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

import sys

nisLibDir = r'D:\Gold_Pipeline\ProcessingPipelines\NIS\lib\Python'

if nisLibDir not in sys.path:
    sys.path.insert(0, nisLibDir)

from aop_processing_utils import *

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
    centroids_site = df[df['siteID'].str.contains(site.site_name)]
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
    tileNames = "NEON_"+site.domain+"_" + site.site_name + "_DP3_" + tiles['tileEasting'].apply(str) + '_' + tiles['tileNorthing'].apply(str)
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
        YearSiteVisit.writeMosaicFromProductClass(CHMpctOutDir+'\\'+YearSiteVisit.YearSiteVisit+'_CHMMosaic_Absolute.tif',newAbsMosaic)         
    return bufferPlots
     

def getMapExtentsForMosiac(chmFolder, ysvc):
    Files = os.listdir(chmFolder)
    FileName, FileExtension = os.path.splitext(Files[0])

    if FileExtension == '.tif':
        RasterIoFilesToMosaicInfo = []
        for File in Files:
            src = rio.open(chmFolder + File)
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
    site = siteClass(YearSiteVisitName)
    site.get_dirs()
    ysvc = YearSiteVisitClass(YearSiteVisitName)
    
    #print("Determining tiles to download")
    tileNames = list_tiles_to_download(site, csvOfSubplotCentroids)
    
    print("Downloading canopy height data")
    if os.path.isdir("D:/" + site.year + "/FullSite/" + site.domain + "/" + YearSiteVisitName + "/L3/DiscreteLidar/CanopyHeightModelGtif"):
        print("Already downloaded")
    else:
        if site.site_name == 'TREE':
            tempYSV = YearSiteVisitName.replace("TREE", "STEI")
            dl.download_specific_tiles(tempYSV,'L3/DiscreteLidar/CanopyHeightModelGtif/',tileNames,'.tif')
        else:
            dl.download_specific_tiles(YearSiteVisitName,'L3/DiscreteLidar/CanopyHeightModelGtif/',tileNames,'.tif')
    
    #Establish folders
    CHMpctOutDir = site.root_dir+"/CanopyHeightPct"
    make_dir(CHMpctOutDir)
    chmFolder = site.root_dir + "/L3/DiscreteLidar/CanopyHeightModelGtif/"

    # GET EXTENT FOR EACH PLOT AND EXTRACT
    bufferPlots = addBufferToSubplot(csvOfSubplotCentroids, site.site_name)
    print("Plots read in")
    
    print("Getting map extents")
    ysvc = getMapExtentsForMosiac(chmFolder, ysvc)
    
    print("Getting mosaics")
    ChmMosaic = ysvc.getMosaic([chmFolder + s for s in os.listdir(chmFolder)], 'tiles')
    ysvc.generateMosaic([chmFolder + s for s in os.listdir(chmFolder)],CHMpctOutDir+'\\'+YearSiteVisitName+'_CHM.tif')

    print("calculate CHM percentage")
    bufferCalc = calculateCHMpct(ChmMosaic, bufferPlots, ysvc, CHMpctOutDir)
    bufferCalc['CHM_percent'] = (bufferCalc['CHM_area']/bufferCalc['plotArea']*100)
    bufferCalc['Year'] = ysvc.year
    bufferCalc = bufferCalc.drop(columns = ['east', 'west', 'north', 'south'])
    DFcalcCHM = DFcalcCHM.append(bufferCalc)
    #bufferCalc.to_csv("C:/Users/kmurphy/Documents/Git/neon-plant-sampling/ltr_vegArea/testDat_CalculatedCHMpct.csv")
        
    return YearSiteVisitName, DFcalcCHM

def Filter(string, substr):
    return [str for str in string if
            any(sub in str for sub in substr)]

if __name__ == '__main__':
    
    # Modify site and year to filter results. Leaving site as 'ALL', but modifying the year returns all year_site_visits
    # flown in that calendar year. Leaving year as 'ALL', but modifying site returns all year_site_visits for that one site
    # Leaving both as all returns all year_site_visits ever flown
    dl=downloadClass() 
    
    sites = ['ALL']
    years = ['ALL']
    csvOfSubplotCentroids = "C:/Users/kmurphy/Documents/Git/neon-plant-sampling/ltr_vegArea/allLitterPlots.csv"
    
    YearSiteVisitNameList = compileYSV(sites, years)
    print(YearSiteVisitNameList)
    #YearSiteVisitNameList = ['2016_TREE_1', '2017_TREE_2', '2019_TREE_3', '2020_TREE_4', '2022_TREE_5']
    
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
    #i = 1
    
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
    DFcalcCHM.to_csv("C:/Users/kmurphy/Documents/Git/neon-plant-sampling/ltr_vegArea/allLitterPlots_CalculatedCHMpct_updated.csv")

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