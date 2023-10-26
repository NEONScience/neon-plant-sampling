
# -*- coding: utf-8 -*-
"""
Created on Mon May 10 18:29:53 2021

@author: tgoulden
"""
import os, sys, sqlite3, boto3
from datetime import datetime
from subprocess import Popen, PIPE
import csv



def downloadSpectrometerProduct(dl,YSV,ProductLevel,Product,Missions=None):

    if Missions:
        for Mission in Missions:
            dl.download_products(YSV,YSV.YearSiteVisit.split('_')[0]+'/FullSite/'+YSV.Domain+'/'+YSV.YearSiteVisit+'/'+ProductLevel+'/Spectrometer/'+Product+'/'+Mission) 
    else:
        dl.download_products(YSV,YSV.YearSiteVisit.split('_')[0]+'/FullSite/'+YSV.Domain+'/'+YSV.YearSiteVisit+'/'+ProductLevel+'/Spectrometer/'+Product)

def makeDir(directory):
    if not os.path.exists(directory):
        #print('Making directory:',directory)
        os.makedirs(directory)

class DownloadClass:
    def __init__(self,storage='gcs'):
        
        if storage == 'gcs':
            print('Downloading from GCS')
            endpoint              = 'https://storage.googleapis.com'
#             aws_access_key_id     = 'prod-sa-aop-reader-s3key'
            aws_access_key_id     = 'GOOG1EGEF5MF7745MC6FMPE726SWFAUFOPWGCUBJMUYJQSPLPUA7MMJFNPW7Q'
            aws_secret_access_key = 'TJ4ZIckFrMUuXQuTiZdyR9l2R6jOF/mVqPSfGffa'
            self.gps_bucket='neon-aop-l0-gps'
            self.l0_bucket='neon-aop-zero'
            self.daily_bucket='neon-aop-daily'
            self.products_bucket='neon-aop-products'
            self.ornl_bucket='neon-aop-ornl'
            self.process_bucket='neon-aop-processing'
        elif storage == 'ecs':
            endpoint              = 'https://s3.den1.neonscience.org'
            aws_access_key_id     = 'aop-read-only'
            aws_secret_access_key = 'XBfFj3t8AKp6XgNI7NP2rOBpKqmIRyx5HngFl/WA'
#             self.gps_bucket=
            self.l0_bucket='aop-zero'
            self.daily_bucket = 'aop-daily'
            self.products_bucket = 'neon-aop-products'
            self.ornl_bucket = 'neon-aop-ornl'
            self.process_bucket = 'aop-processing'
       
        self.resource = boto3.resource(
                service_name          =  's3',
                aws_access_key_id     =  aws_access_key_id,
                aws_secret_access_key =  aws_secret_access_key,
                endpoint_url          =  endpoint)
        self.client = boto3.client(
                service_name          =  's3',
                aws_access_key_id     =  aws_access_key_id,
                aws_secret_access_key =  aws_secret_access_key,
                endpoint_url          =  endpoint)
        self.l0_bucket_change_date = datetime.strptime('20200612','%Y%m%d')

    def list_missions_from_product(self,year_site_v):
        site=siteClass(year_site_v)
        DomainLookup = 'D:/Gold_Pipeline/ProcessingPipelines/NIS/res/Lookups/HDF5_lookup.csv'
        with open(DomainLookup) as CsvFile:
            DomainLookupContents = csv.reader(CsvFile,delimiter=',')
            for Row in DomainLookupContents:
                if Row[1] == site.site_name:
                    Domain=Row[2]
        try:
            #prefix = site.year+'/'+'FullSite/'+Domain+'/'+year_site_v+'/'+'L1/Camera/Images/'
            prefix = site.year+'/'+'FullSite/'+Domain+'/'+year_site_v+'/'+'L1/Spectrometer/RadianceH5/'

        except:
            print(year_site_v)
        MissionList = []
        if 'ORNL' in year_site_v:
            products_bucket=self.resource.Bucket(self.ornl_bucket)
        else:
            products_bucket=self.resource.Bucket(self.products_bucket)
        for s3_object in products_bucket.objects.filter(Prefix=prefix):
            ObjectListSplit = s3_object.key.split('/')
            MissionList.append(ObjectListSplit[-2])
        return list(set(MissionList))

    def list_year_site_visit_from_product(self,year):
        
        prefix = year+'/'+'FullSite/'
        YearSiteList = []
        products_bucket=self.resource.Bucket(self.products_bucket)
        for s3_object in products_bucket.objects.filter(Prefix=prefix):
            ObjectListSplit = s3_object.key.split('/')
            YearSiteList.append(ObjectListSplit[3])
        return list(set(YearSiteList))


    def list_missions(self,year=None):
        ecs_objects = self.client.list_objects(Bucket=self.l0_bucket, Delimiter='/')
        mission_list = []
        for obj in ecs_objects.get('CommonPrefixes'):
            mission_list.append(obj['Prefix'].replace('/', ''))
        
        # ecs_objects = self.ecs_client.list_objects(Bucket='aop-zero', Delimiter='/')
        # for obj in ecs_objects.get('CommonPrefixes'):
        #     mission_list.append(obj['Prefix'].replace('/', ''))
        
        if year is None:
            return mission_list
        else:
            return [m for m in mission_list if m.startswith(str(year))]
        
    def list_sbets(self,year=None):
        sbetList = []
        prefix=year+'/Daily/'
        daily_bucket=self.resource.Bucket(self.daily_bucket)
        for s3_object in daily_bucket.objects.filter(Prefix=prefix):
            path, filename = os.path.split(s3_object.key)
            if 'L1/GPSIMU' in path and filename.startswith('sbet'):
                sbetList.append(filename)
        return sbetList
    def download_sbet(self,mission_name=None):
        mission=missionClass(mission_name)
        prefix=mission.year+'/Daily/'+mission_name+'/L1/GPSIMU'
        mission.get_dirs()
        sbet_dir=mission.sbet_dir
#        print('sbet directory :',sbet_dir)
        del mission
        makeDir(sbet_dir)
        daily_bucket=self.resource.Bucket(self.daily_bucket)
        for s3_object in daily_bucket.objects.filter(Prefix=prefix):
            path, filename = os.path.split(s3_object.key)
            if filename.startswith('sbet') | filename.startswith('smrmsg'):
                #print('Downloading: ',filename)
                daily_bucket.download_file(s3_object.key,os.path.join(sbet_dir,filename))

    def download_products(self,year_site_v,prefix,ext=None,DoNotDownload=False):
        # year_site_v='YYYY_SITE_#' ; product='L3/Camera/Mosaic/'
        #site=siteClass(year_site_v)
        #site.get_dirs()
        download_dir = os.path.join('D:/',prefix)
        makeDir(download_dir)
        prefix_split = prefix.split('/')
        FileList = []
        FileListSize = []
        if prefix_split[4] == 'QA' or prefix_split[4] == 'Processing' or prefix_split[4] == 'Internal':
            bucket=self.resource.Bucket(self.process_bucket)
        else:
            bucket=self.resource.Bucket(self.products_bucket)
        if ext==None:
            #print('Downloading: ',site.year+'/FullSite/'+site.domain+'/'+year_site_v+'/'+product)
            for s3_object in bucket.objects.filter(Prefix=prefix):
                path, filename = os.path.split(s3_object.key)
                #print('Downloading: ',site.year+'/FullSite/'+site.domain+'/'+year_site_v+'/'+product)
                if not os.path.isfile(os.path.join(download_dir,filename)):
                    bucket.download_file(s3_object.key,os.path.join(download_dir,filename))
        else:
            for s3_object in bucket.objects.filter(Prefix=prefix):
                path, filename = os.path.split(s3_object.key)
                #print(path)
                if filename.endswith(ext):
                    #print('Downloading: ',filename)
                    FileList.append(filename)
                    FileListSize.append(s3_object.size)
                    if DoNotDownload:
                        continue
                    else:
                        if not os.path.isfile(os.path.join(download_dir,filename)):
                            bucket.download_file(s3_object.key,os.path.join(download_dir,filename))
        return FileList,FileListSize                
    
    def download_general(self,bucket,prefix,ext=None):
        # year_site_v='YYYY_SITE_#'
        #bucket = aop-processing,neon-aop-crestedbutte,neon-aop-ornl,neon-aop-product,neon-aop-products,neon-aop-walnutgulch,prod-aop-odrive
        #prefix='2019/FullSite/D01/2019_HOPB_3/L3/Camera/Mosaic'
        #site=siteClass(year_site_v)
        #site.get_dirs()
        bucket=self.resource.Bucket(bucket)
        for s3_object in bucket.objects.filter(Prefix=prefix):
            path, filename = os.path.split(s3_object.key)
            print('Downloading: ',filename)
            download_dir = os.path.join('D:\\','\\'.join(path.split('/')))
            makeDir(download_dir)
            
            if ((os.path.isfile(os.path.join(download_dir,filename)) and os.path.getsize(os.path.join(download_dir,filename))==s3_object.size)) or 'Thumbs.db' in filename:
                print(os.path.join(download_dir,filename))
                print('Skipping file',filename,' - already exists in folder',download_dir)
            else:
                if ext==None:
                    bucket.download_file(s3_object.key,os.path.join(download_dir,filename))
                else:
                    if filename.endswith(ext):
                        print('Downloading: ',filename)
                        bucket.download_file(s3_object.key,os.path.join(download_dir,filename))
    

    
    def list_l0_data(self,year=None):

        bucket=self.resource.Bucket(self.l0_bucket)
        fileList = []
        for s3_object in bucket.objects.filter(Prefix=year):
            fileList.append(s3_object.key)

        return fileList
    
    def download_l0(self,mission_name,l0_folder,check_folder='raw',ext=None):
        mission=missionClass(mission_name)
        mission_date = datetime.strptime(mission.flightday,'%Y%m%d%H')
        if mission_date < self.l0_bucket_change_date:
        #    l0_bucket = 'aop-l0'
            prefix = mission.mission_name+'/L0/'+l0_folder
        else:
        #    l0_bucket = 'aop-zero'
            prefix = mission.year+'/Zero/'+mission.mission_name+'/L0/'+l0_folder
        bucket=self.resource.Bucket(self.l0_bucket)
 
        for s3_object in bucket.objects.filter(Prefix=prefix):
            path, filename = os.path.split(s3_object.key)
            download_dir = os.path.join('D:\\Raw\\',mission.year,mission.payload,mission.campaign,'\\'.join(path.split('/')))
            download_dir = download_dir.replace(mission.year+'\\Zero\\','\\')
#             print(download_dir)
            if ext is None:
                makeDir(download_dir)
                if check_folder=='raw':
                    if os.path.isfile(os.path.join(download_dir,filename)) and os.path.getsize(os.path.join(download_dir,filename))==s3_object.size:
                        print('Skipping file',filename,' - already exists in raw folder',download_dir)
                    else:
                        print('Downloading',filename,'to',download_dir)
                        bucket.download_file(s3_object.key,os.path.join(download_dir,filename))
                else:
                    if (os.path.isfile(os.path.join(check_folder,filename)) and os.path.getsize(os.path.join(check_folder,filename))==s3_object.size) or (os.path.isfile(os.path.join(download_dir,filename)) and os.path.getsize(os.path.join(download_dir,filename))==s3_object.size):
                        print(os.path.join(check_folder,filename))
                        print('Skipping file',filename,' - already exists in folder',check_folder)
                    else:
                        print('Downloading',filename,'to',download_dir)
                        bucket.download_file(s3_object.key,os.path.join(download_dir,filename))
            else:
                if filename.endswith(ext):
                    print(filename)
                    makeDir(download_dir)
                    if check_folder=='raw' and os.path.isfile(os.path.join(download_dir,filename)) and os.path.getsize(os.path.join(download_dir,filename))==s3_object.size:
                            print('Skipping file',filename,' - already exists in raw folder',download_dir)
                            continue
                    else:
                        if (os.path.isfile(os.path.join(check_folder,filename)) and os.path.getsize(os.path.join(check_folder,filename))==s3_object.size) or (os.path.isfile(os.path.join(download_dir,filename)) and os.path.getsize(os.path.join(download_dir,filename))==s3_object.size):
                            print(os.path.join(check_folder,filename))
                            print('Skipping file',filename,' - already exists in folder',check_folder)
                        else:
                            print('Downloading',filename,'to',download_dir)
                            bucket.download_file(s3_object.key,os.path.join(download_dir,filename))

    def download_HKQA(self,mission_name,product=None,check_folder='raw',ext=None):
            mission=missionClass(mission_name)
            mission_date = datetime.strptime(mission.flightday,'%Y%m%d%H')

            if mission_date < self.l0_bucket_change_date:
                l0_bucket = 'aop-l0'
                prefix = mission.mission_name+'/QA_checks/'
            else:
                l0_bucket = self.l0_bucket
                prefix = mission.year+'/Zero/'+mission.mission_name+'/QA_checks/'
            bucket=self.resource.Bucket(l0_bucket)
            #for s3_object in bucket.objects.filter(Prefix=prefix):
            #    path, filename = os.path.split(s3_object.key)
            #    print(path)
            
            for s3_object in bucket.objects.filter(Prefix=prefix):
                
                if product is not None:
                    if product not in s3_object.key:
                        continue
                
                path, filename = os.path.split(s3_object.key)
                download_dir = os.path.join('D:\\Raw\\',mission.year,mission.payload,mission.campaign,'\\'.join(path.split('/')))
                download_dir = download_dir.replace(mission.year+'\\Zero\\','\\')
    #             print(download_dir)
                if ext is None:
                    makeDir(download_dir)
                    if check_folder=='raw':
                        if os.path.isfile(os.path.join(download_dir,filename)) and os.path.getsize(os.path.join(download_dir,filename))==s3_object.size:
                            print('Skipping file',filename,' - already exists in raw folder',download_dir)
                        else:
                            print('Downloading',filename,'to',download_dir)
                            bucket.download_file(s3_object.key,os.path.join(download_dir,filename))
                    else:
                        if (os.path.isfile(os.path.join(check_folder,filename)) and os.path.getsize(os.path.join(check_folder,filename))==s3_object.size) or (os.path.isfile(os.path.join(download_dir,filename)) and os.path.getsize(os.path.join(download_dir,filename))==s3_object.size):
                            print(os.path.join(check_folder,filename))
                            print('Skipping file',filename,' - already exists in folder',check_folder)
                        else:
                            print('Downloading',filename,'to',download_dir)
                            bucket.download_file(s3_object.key,os.path.join(download_dir,filename))
                else:
                    if filename.endswith(ext):
                        print(filename)
                        makeDir(download_dir)
                        if check_folder=='raw' and os.path.isfile(os.path.join(download_dir,filename)) and os.path.getsize(os.path.join(download_dir,filename))==s3_object.size:
                                print('Skipping file',filename,' - already exists in raw folder',download_dir)
                                continue
                        else:
                            if (os.path.isfile(os.path.join(check_folder,filename)) and os.path.getsize(os.path.join(check_folder,filename))==s3_object.size) or (os.path.isfile(os.path.join(download_dir,filename)) and os.path.getsize(os.path.join(download_dir,filename))==s3_object.size):
                                print(os.path.join(check_folder,filename))
                                print('Skipping file',filename,' - already exists in folder',check_folder)
                            else:
                                print('Downloading',filename,'to',download_dir)
                                bucket.download_file(s3_object.key,os.path.join(download_dir,filename))


class lookupClass:
    def __init__(self):
        self.site_lookup_dir="S:/Gold_Pipeline/ProcessingPipelines/Janus/Resources/Lookups/"
        self.epsg_lookup=os.path.join(self.site_lookup_dir,'EPSG code table.txt')
        self.site_lookup_db = os.path.join(self.site_lookup_dir,'neon_aop_lookup.db')
        self.epsg_dict = {}
        with open(self.epsg_lookup) as f:
            for line in f:
                utm=" ".join(line.split()[0:5]).replace('UTM Zone ','').replace('Northern Hemisphere','N').replace('Southern Hemisphere','S')
                epsg=line.split()[-1]
                self.epsg_dict[utm] = epsg

    def query_db(self,site,lookup_key):
        conn = sqlite3.connect(self.site_lookup_db)
        db_sites = [site[0] for site in conn.cursor().execute("SELECT site_code FROM site")]
        print(','.join(sys.argv))
#         if len(sys.argv) > 3:
#             print("ERROR: too many input arguments, use syntax query_lookup.py SITE LOOKUP_KEY")
#             sys.exit()
        if site not in db_sites:
            print("ERROR: ",site," is not in the database. Check that site is entered correctly or run add_new_site_to_lookup_db.py to add site to lookup database.")
        else:
            lookup_keys = [description[0] for description in conn.cursor().execute("SELECT * from site").description]
            if lookup_key in lookup_keys:
                return conn.cursor().execute("SELECT "+lookup_key+" FROM site WHERE site_code = ? ", (site,)).fetchone()[0]
            else:
                print("ERROR: ",lookup_key," is not valid lookup key")
                print("Valid lookup keys: ",", ".join(lookup_keys))

    def utm2epsg(self,utm_zone,hem='N'):
        try:
            epsg_code=self.epsg_dict[str(int(utm_zone))+' '+hem]
        except:
            epsg_code = None
#        print('EPSG: ',epsg_code)
        return epsg_code

    def get_flight_plans(self,site):
        conn = sqlite3.connect(self.site_lookup_db)
        db_sites = [site[0] for site in conn.cursor().execute("SELECT site_code FROM flight_plan_geo_data")]
#         if len(sys.argv) > 2:
#             print("ERROR: too many input arguments, use syntax get_flight_plans.py SITE")
#             sys.exit()
        if site not in db_sites:
            print("ERROR: ",site," is not in the database. Check that site is entered correctly or run add_new_site_to_lookup_db.py to add site to lookup database.")
            exit()
        else:
            try:
                fb_all = []
                fb_all.append(conn.cursor().execute("SELECT flightbox_p1 FROM flight_plan_geo_data WHERE end_date IS NULL AND site_code = ? ", (site,)).fetchone()[0])
                fb_all.append(conn.cursor().execute("SELECT flightbox_p2 FROM flight_plan_geo_data WHERE end_date IS NULL AND site_code = ? ", (site,)).fetchone()[0])
                fb_all.append(conn.cursor().execute("SELECT flightbox_p3 FROM flight_plan_geo_data WHERE end_date IS NULL AND site_code = ? ", (site,)).fetchone()[0])
                fb =  [x for x in fb_all if not x == 'N/A' and not x.strip() == '']
                if fb !=[]:
                    return fb
                else:
                    try:
                        fb_aquatic = []
                        fb_aquatic.append(conn.cursor().execute("SELECT aquatic_1 FROM flight_plan_geo_data WHERE end_date IS NULL AND site_code = ? ", (site,)).fetchone()[0])
                        fb_aquatic.append(conn.cursor().execute("SELECT aquatic_2 FROM flight_plan_geo_data WHERE end_date IS NULL AND site_code = ? ", (site,)).fetchone()[0])
                        fb_aquatic =  [x for x in fb_aquatic if not x == 'N/A' and not x.strip() == '']
                        if fb_aquatic ==[]:
                            print('WARNING could not find any terrestrial or aquatic flight plans for site',site)
                            return
                        else:
                            return fb_aquatic
                    except:
                        print('ERROR! check lookup_db flight_plan_geo_data table for site',site)
            except ValueError:
                print('ERROR!',sys.exc_info()[0])


class siteClass:
    def __init__(self,YearSiteVisit=None):
        self.YearSiteVisit=YearSiteVisit
        if YearSiteVisit is not None:
            self.year=YearSiteVisit.split('_')[0]
            self.site_name=YearSiteVisit.split('_')[1]
            self.visit=YearSiteVisit.split('_')[2]
        DomainLookup = 'D:/Gold_Pipeline/ProcessingPipelines/NIS/res/Lookups/HDF5_lookup.csv'
        with open(DomainLookup) as CsvFile:
            DomainLookupContents = csv.reader(CsvFile,delimiter=',')
            for Row in DomainLookupContents:
                if Row[1] == self.site_name:
                    self.Domain=Row[2] 
                 
class missionClass:
    def __init__(self,mission_name=None):
        self.mission_name=mission_name
        if mission_name is not None:
            self.year=mission_name[0:4]
            self.flightday=mission_name[0:10]
            self.payload=mission_name.split('_')[1][0:2]
            self.campaign=mission_name.split('_')[1][2:4]
    def get_dirs(self):
        self.sbet_dir=os.path.join('D:\\',self.year,'Daily',self.mission_name,'L1','GPSIMU')
        self.l0_dir=os.path.join('D:\\Raw\\',self.year,self.payload,self.campaign,self.mission_name,'L0')
        self.l0_ancillary_dir=os.path.join(self.l0_dir,'Ancillary')
        self.l0_waveform_dir=os.path.join(self.l0_dir,'WaveformLidar')
        if self.payload=='P3':
            self.sdf_dir=os.path.join(self.l0_waveform_dir,'03_RIEGL_RAW','01_SDF','Q780')
        else:
            self.range_dir=os.path.join(self.l0_dir,'DiscreteLidar')
            self.df3_dir=os.path.join(self.l0_waveform_dir,'DISK1')