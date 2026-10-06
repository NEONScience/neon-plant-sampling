# -*- coding: utf-8 -*-
"""
Created on Fri Sep 25 09:11:06 2026

@author: lea
"""
# load packages
import pandas as pd 
import exifread
import os
import io
import rawpy
from google.cloud import storage
from PIL import Image

###############################################################################
# set input variables
domains = ['D01','D02','D03','D04','D05','D06','D07','D08','D09','D10','D11',
          'D12','D13','D14','D15','D16','D17','D18','D19','D20']
bucket_name = "neon-dhp-images"
#HEADER_BYTES = 2 * 1024 * 1024  # 2 MB
HEADER_BYTES=15000 
#5000 worked for old photos, newer ones needed 10000, used 15000 to be safe

# connect to google cloud storage
client = storage.Client()
bucket = client.bucket(bucket_name)

# create lists for images with issues
zoomed_images = []
exif_errors = []    
# loop through domains
for d in range(len(domains)):
    domain = domains[d]
    # get list of blobs
    blobs = list(bucket.list_blobs(prefix = domain))
    # loop through blobs
    for b in range(len(blobs)):
        blob = blobs[b]
        # blob download will error if file smaller than header byte number so use try-except
        try:
            # download header data
            data = blob.download_as_bytes(start=0, end=HEADER_BYTES - 1)
            # process download, extract tags
            tags = exifread.process_file(io.BytesIO(data), details=False)
            # can print tag names and values if you aren't sure what they'll be
            #for tag, value in tags.items():
            #    print(f"{tag}: {value}")
            # extract zoom property
            zoom = tags.get("EXIF DigitalZoomRatio")
            #D780 cameras do not have this so they will fail and need additional processing after
            if zoom == None:
                # this will get corrupted files and those from D780 cameras
                exif_errors = exif_errors + [blob.name]
            else:
                # extract value of zoom property
                zoom_value = zoom.values[0]
                # zoom = 1 is expected, so greater than 1 will be zoomed in
                if zoom_value > 1:
                    zoomed_images = zoomed_images + [blob.name]
                # I never actually saw a zoom of 0 so I don't know if it happens but I wrote this in just in case because it would definitely be worth flagging as weird
                elif zoom_value == 0:
                    exif_errors = exif_errors + [blob.name]
        except:
            # this will get 0B or other tiny weird files
            exif_errors = exif_errors + [blob.name]

                
# if you accidentally get some duplicates this will remove those
exif_errors=list(dict.fromkeys(exif_errors))
      
# make the lists dataframes so you can output as CSVs
exif_df = pd.DataFrame(data=exif_errors,columns=["photoPath"])
zoomed_df = pd.DataFrame(data=zoomed_images,columns=["photoPath"])

# set output directory
path = r'C:\Users\lea\Documents\LGM\dhp'
os.chdir(path)

# write out CSVs
out1 = 'exif_issues.csv'
exif_df.to_csv(out1, index=False)
out2 = 'zoomed_images.csv'
zoomed_df.to_csv(out2, index=False)

###############################################################################

# need additional processing to check D780 files
# because these cameras do not have "EXIF DigitalZoomRatio"
# will need to download the entire file and use rawpy to check the dimensions

# create lists for images with issues
file_corrupted = []
tiff_zoom = []
# loop through exif_errors list from first output
for p in range(len(exif_errors)):
    path = exif_errors[p]
    # you can use the path as a prefix to just get that one blob
    blob = list(bucket.list_blobs(prefix = path))
    # since this is getting the whole file not a specific number of bytes the small files should no longer fail
    # but I used try-except again just in case so the loop doesn't break
    try:
        # pull entire file
        data = blob[0].download_as_bytes()
        # get tags to rule out things that aren't images at all - photos should have some tag values no matter what
        tags = exifread.process_file(io.BytesIO(data), details=False)
        if len(tags) == 0:
            file_corrupted = file_corrupted + [path]
        else:
            # if there are tags it is a photo, so read it and process it with rawpy
            img = rawpy.imread(io.BytesIO(data)).postprocess()
            # height will be length of the object and width will be length of the height
            # ensure image is > 4000x6000 to be full resolution
            if len(img) < 4000 or len(img[0]) < 6000:
                tiff_zoom = tiff_zoom + [path]
    except:
        file_corrupted = file_corrupted + [path]

# make the lists dataframes so you can output as CSVs
corrupted_df = pd.DataFrame(data=file_corrupted,columns=["photoPath"])
tiff_df = pd.DataFrame(data=tiff_zoom,columns=["photoPath"])

# I'm suspicious of these zoom flags so I decide I want to pull the height and width
# to see if any really aren't > 4000x6000
tiff_df["height"] = None
tiff_df["width"] = None

for r in tiff_df.index.tolist():
    path = tiff_df.loc[r,"photoPath"]
    blob = list(bucket.list_blobs(prefix = path))
    data = blob[0].download_as_bytes()
    img = rawpy.imread(io.BytesIO(data)).postprocess()
    tiff_df.loc[r,"height"] = len(img)
    tiff_df.loc[r,"width"] = len(img[0])

# write out CSVs
out3 = 'file_corrupted.csv'
corrupted_df.to_csv(out3, index=False)
out4 = 'zoomed_images2.csv'
tiff_df.to_csv(out4, index=False)
# these all ended up being > 4000x6000 so false flags
  
###############################################################################

# this is additional code that can get blobs with specific file extensions 
# just fyi how to do that, also note it is case sensitive
'''
extension = "jpg"
extension = "JPG"
jpg_blobs = bucket.list_blobs(match_glob=f"**/*.{extension}")
jpg_list = list(jpg_blobs)
        
blobj = jpg_list[0]
    
dataj = blobj.download_as_bytes(start=0, end=HEADER_BYTES - 1)
tagsj = exifread.process_file(io.BytesIO(dataj), details=False)

for tag, value in tagsj.items():
    print(f"{tag}: {value}")
'''