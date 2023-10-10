# -*- coding: utf-8 -*-
"""
Created on Tue Jan  7 11:39:40 2020

@author: bhass

functions & classes used for L0-L1 trajectory and lidar processing

updated by kwoyna and bhass 6/2022 to use google cloud python API for downloading

required packages:
pip install --upgrade google-cloud-storage
pip install --upgrade google-auth

not sure if these are needed:
pip install google-cloud
pip install --upgrade google-api-python-client

"""
#%% import required packages

import os, sys, sqlite3, boto3, shutil, time, json
from datetime import datetime
from subprocess import Popen, PIPE
from fnmatch import fnmatch
from google.cloud import storage
from google.oauth2 import service_account
from google.api_core import page_iterator


#%% os/sys functions

def run_system_command(cmd):
    with Popen(cmd, stdout=PIPE, bufsize=1, universal_newlines=True) as p:
        for line in p.stdout:
            print(line, end='') # print standard output for each line
        if p.returncode != 0:
            print('returncode:',p.returncode)
    return cmd

def make_dir(directory):
    if not os.path.exists(directory):
        print('Making directory:',directory)
        os.makedirs(directory)

def move_files(sourcepath,destinationpath,ext=None):
    sourcefiles = os.listdir(sourcepath)
    if ext is None:
        for file in sourcefiles:
            shutil.move(os.path.join(sourcepath,file), os.path.join(destinationpath,file))
    else:
        for file in sourcefiles:
            if file.endswith(ext):
                shutil.move(os.path.join(sourcepath,file), os.path.join(destinationpath,file))

#%% classes

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

class siteClass:
    def __init__(self,year_site_v):
        self.year_site_v=year_site_v
        self.site_name=year_site_v.split('_')[1]
        self.year=year_site_v.split('_')[0]
        self.visit_no=year_site_v.split('_')[2]
        lookup=lookupClass()
#         print(self.site_name)
        self.domain=lookup.query_db(self.site_name,'domain')
        self.utm_zone=lookup.query_db(self.site_name,'utm_zone')
        self.epsg_code=lookup.utm2epsg(self.utm_zone)
        self.ellipsoid_offset=lookup.query_db(self.site_name,'wgs84_nad83_offset')
        del lookup
    def get_dirs(self,mission=None):
        if self.site_name[1:3]=='10':
            self.mission=mission
            self.flightday = mission.split('_')[0]
            self.pc = mission.split('_')[1]
            self.root_dir=os.path.join('D:\\',self.year,'Engineering',self.flightday+'_'+self.site_name+'_'+self.pc)
        else:
            self.root_dir=os.path.join('D:\\',self.year,'FullSite',self.domain,self.year_site_v)
        self.int_dir=os.path.join(self.root_dir,'Internal','DiscreteLidar')
        self.las_dir=os.path.join(self.root_dir,'L1','DiscreteLidar','Las')
        self.pls_dir=os.path.join(self.root_dir,'L1','WaveformLidar','Pulsewaves')        
        self.proc_dir=os.path.join(self.root_dir,'Processing','DiscreteLidar','LMS')
        self.lastools_proc_dir=os.path.join(self.root_dir,'Processing','DiscreteLidar','LASTOOLS')
        self.qa_dir=os.path.join(self.root_dir,'QA','DiscreteLidar')
    def make_dirs(self):
        make_dir(self.root_dir)
        make_dir(os.path.join(self.root_dir,'Internal','DiscreteLidar','LazStandard'))
        make_dir(os.path.join(self.root_dir,'L1','DiscreteLidar','Las'))
        make_dir(os.path.join(self.root_dir,'L1','WaveformLidar','Pulsewaves'))
        make_dir(os.path.join(self.root_dir,'Processing','DiscreteLidar','LMS'))
        make_dir(os.path.join(self.root_dir,'QA','DiscreteLidar','LMS'))

class downloadClass():
    def __init__(self,cloud_storage='gcs'):
        if cloud_storage == 'gcs':
            print('downloadClass pulling from GCS')
#             endpoint              = 'https://storage.googleapis.com'
# #             aws_access_key_id     = 'prod-sa-aop-reader-s3key'
#             aws_access_key_id     = 'GOOG1EGEF5MF7745MC6FMPE726SWFAUFOPWGCUBJMUYJQSPLPUA7MMJFNPW7Q'
#             aws_secret_access_key = 'TJ4ZIckFrMUuXQuTiZdyR9l2R6jOF/mVqPSfGffa'
            json_credentials_path = r'D:\Gold_Pipeline\ProcessingPipelines\res\gcs_aop_credentials.json'
            self.gcs_credentials = service_account.Credentials.from_service_account_file(json_credentials_path)
            self.gps_bucket='neon-aop-l0-gps'
            self.l0_bucket='neon-aop-zero'
            self.daily_bucket='neon-aop-daily'
            self.products_bucket='neon-aop-products'
            self.ornl_bucket='neon-aop-ornl-pending'
            self.process_bucket='neon-aop-processing'
#         elif cloud_storage == 'ecs':
#             endpoint              = 'https://s3.den1.neonscience.org'
#             aws_access_key_id     = 'aop-read-only'
#             aws_secret_access_key = 'XBfFj3t8AKp6XgNI7NP2rOBpKqmIRyx5HngFl/WA'
#             self.l0_bucket='aop-zero'
#             self.daily_bucket = 'aop-daily'
#             self.products_bucket = 'neon-aop-products'
#             self.ornl_bucket = 'neon-aop-ornl'
#             self.process_bucket = 'aop-processing'
    #connect to GCS/ECS with AWS credentials:
#         self.resource = boto3.resource(
#                 service_name          =  's3',
#                 aws_access_key_id     =  aws_access_key_id,
#                 aws_secret_access_key =  aws_secret_access_key,
#                 endpoint_url          =  endpoint)
#         self.client = boto3.client(
#                 service_name          =  's3',
#                 aws_access_key_id     =  aws_access_key_id,
#                 aws_secret_access_key =  aws_secret_access_key,
#                 endpoint_url          =  endpoint)
    #connect to GCS with google credentials:
        self.storage_client = storage.Client(credentials=self.gcs_credentials)
#         self.l0_bucket_change_date = datetime.strptime('20200612','%Y%m%d')

    # https://stackoverflow.com/questions/37074977/how-to-get-list-of-folders-in-a-given-bucket-using-google-cloud-api
    def _item_to_value(self,iterator,item):
        return item

    def list_directories(self,bucket_name,prefix):
        if prefix and not prefix.endswith('/'):
            prefix += '/'
    
        extra_params = {
            "projection": "noAcl",
            "prefix": prefix,
            "delimiter": '/'
            }
    
        gcs = self.storage_client
    
        path = "/b/" + bucket_name + "/o"
    
        iterator = page_iterator.HTTPIterator(
            client=gcs,
            api_request=gcs._connection.api_request,
            path=path,
            items_key='prefixes',
            item_to_value=self._item_to_value,
            extra_params=extra_params,
            )
    
        return [x for x in iterator]

    def get_all_years(self,bucket):
        year_list = []
        bucket = self.storage_client.bucket(bucket)
        iterator = bucket.list_blobs(delimiter='/')
        response = iterator._get_next_page_response()
        for prefix in response['prefixes']:
            year_list.append(prefix.replace('/',''))
        return year_list

    def get_latest_year(self,bucket):
        all_years = self.get_all_years(bucket)
        all_years.sort(reverse=True)
        latest_year = all_years[0]
        print('latest year:',latest_year)
        return latest_year

    def list_general(self,bucket):
        if bucket==self.l0_bucket:
            prefix='/Zero/'
            thing='missions'
        elif bucket==self.daily_bucket:
            prefix='/Daily/'
            thing='sbets'
        elif bucket==self.products_bucket:
            prefix='/FullSite/'
            thing='domains'
    
        start_time = time.time()
        year_list = self.get_all_years(bucket)
        print('getting all',thing,'for years',','.join(year_list))
        all_things = []
        for year in year_list:
            things_fullpath = self.list_directories(bucket,year+prefix)
            things = [x.split('/')[-2] for x in things_fullpath]
            all_things.extend(things)
        end_time = time.time()
        print('Elapsed time: ',str(round((end_time-start_time)/60,2)),'minutes')
        return all_things

    def list_missions_by_year(self,year):
        prefix = year+'/Zero/'
        year_l0_list = []
        bucket = self.storage_client.bucket(self.l0_bucket)
        for gcs_object in bucket.list_blobs(prefix=prefix):
            object_list_split = gcs_object.name.split('/')
            year_l0_list.append(object_list_split[2])
#             year_l0_list.sort()
        return list(set(year_l0_list))
    
        def list_missions_by_ysv(self,year_site_v):
            prefix = year+'/Zero/'
            year_l0_list = []
            bucket = self.storage_client.bucket(self.l0_bucket)
            for gcs_object in bucket.list_blobs(prefix=prefix):
                object_list_split = gcs_object.name.split('/')
                year_l0_list.append(object_list_split[2])
    #             year_l0_list.sort()
            return list(set(year_l0_list))
    
    #     def list_all_missions(self):
    #         start_time = time.time()
    #         year_list = self.get_all_years(self.l0_bucket)
    #         print('listing all missions for years',','.join(year_list))
#         missions_list = []
#         for year in year_list:
#             missions = self.list_missions_by_year(year)
#             missions_list.extend(missions)
#         end_time = time.time()
#         print('Elapsed time: ',str(round((end_time-start_time)/60,2)),'minutes')
#         return missions_list.sort()
    
    def list_all_missions(self,Hg=None):
        all_missions = self.list_general(self.l0_bucket)
        if not Hg:
            sci_missions = [x for x in all_missions if not 'Hg' in x]
            return sci_missions
        else:
            return all_missions

    # TO DO: create a function to get the latest year by payload, 
    # this doesn't work if the payload wasn't flown in the latest year or year specified
    def get_latest_mission(self,payload,year=None,Hg=None):
        if year is None:
            year = self.get_latest_year(self.l0_bucket)
        print('getting latest mission for',payload,'in',year)
        mission_list = self.list_missions_by_year(year)
        payload_missions = [x for x in mission_list if payload in x]
        if len(payload_missions)==0:
            print('No missions found for payload',payload,'in',year)
            return
        payload_missions.sort(reverse=True)
        if not Hg:
            payload_missions = [x for x in payload_missions if not 'Hg' in x]
        latest_mission = payload_missions[0]
        print('latest mission:',latest_mission)
        return latest_mission

    def list_sites_by_year(self,year):
        prefix = year+'/FullSite/'
        year_site_list = []
        bucket = self.storage_client.bucket(self.products_bucket)
        for gcs_object in bucket.list_blobs(prefix=prefix):
            object_list_split = gcs_object.name.split('/')
            year_site_list.append(object_list_split[3])
        return list(set(year_site_list))

    def list_gcs_fullsite_paths(self):
        start_time = time.time()
        year_list = self.get_all_years(self.products_bucket)
        all_year_domains = []
        for year in year_list:
            year_domains = self.list_directories(self.products_bucket,year+'/FullSite/')
            all_year_domains.extend(year_domains)

        fullsite_path_list = []
        for year_domain in all_year_domains:
            year_domain_site = self.list_directories(self.products_bucket,year_domain)
            fullsite_path_list.extend(year_domain_site)

        end_time = time.time()
        print('Elapsed time: ',str(round((end_time-start_time)/60,2)),'minutes')
        return fullsite_path_list
    
    def list_all_ysvs(self,site_name=None):
        # eg. site_name = 'ABBY'
        start_time = time.time()
        year_list = self.get_all_years(self.products_bucket)
        print('getting all domains for years',','.join(year_list))
        all_year_domains = []
        for year in year_list:
            year_domains = self.list_directories(self.products_bucket,year+'/FullSite/')
            all_year_domains.extend(year_domains)

        print('getting all ysvs for years',','.join(year_list))
        ysv_list = []
        for year_domain in all_year_domains:
            year_domain_site = self.list_directories(self.products_bucket,year_domain)
            ysvs = [site.split('/')[-2] for site in year_domain_site]  
            ysv_list.extend(ysvs)
        
        if site_name is None:
            end_time = time.time()
            print('Elapsed time: ',str(round((end_time-start_time)/60,2)),'minutes')
            return ysv_list
        else:
            print('getting all year_site_visits for site',site_name)
            site_ysv_list = [site for site in ysv_list if site_name in site]
            end_time = time.time()
            print('Elapsed time: ',str(round((end_time-start_time)/60,2)),'minutes')
#             print(site_ysv_list)
            return site_ysv_list

    def list_all_ornl_ysvs(self):
        # eg. site_name = 'ABBY'
        start_time = time.time()
        year_list = self.get_all_years(self.products_bucket)
        print('getting all domains for years',','.join(year_list))
        all_year_domains = []
        for year in year_list:
            year_domains = self.list_directories(self.products_bucket,year+'/FullSite/')
            all_year_domains.extend(year_domains)

        print('searching products bucket for ORNL ysvs for years',','.join(year_list))
        ysv_list = []
        for year_domain in all_year_domains:
            year_domain_site = self.list_directories(self.products_bucket,year_domain)
            ysvs = [site.split('/')[-2] for site in year_domain_site]  
            ysv_list.extend(ysvs)
        ornl_ysv_list = [site for site in ysv_list if 'ORNL' in site]
        
        print('searching ORNL bucket for ysvs')
        d07_years = [d for d in all_year_domains if 'D07' in d]
        for year_domain in d07_years:
            try:
                year_domain_site = self.list_directories(self.ornl_bucket,year_domain)
                ysvs = [site.split('/')[-2] for site in year_domain_site]  
                ornl_ysv_list.extend(ysvs)
            except:
                print('WARNING: Could not find anything in ORNL bucket for ',year_domain)
        
        end_time = time.time()
        print('Elapsed time: ',str(round((end_time-start_time)/60,2)),'minutes')
        ornl_ysvs = list(set(ornl_ysv_list))
        ornl_ysvs.sort()
        return ornl_ysvs

    def get_previous_visit(self,ysv):
        # eg. ysv = '2022_MOAB_6'
        site = siteClass(ysv)
        previous_visit_no = int(site.visit_no)-1
        all_ysvs = self.list_all_ysvs(site.site_name)
        print('All ',site.site_name,'year_site_visits:')
        print('\n'.join(site for site in all_ysvs))
        previous_visit = next(site for site in all_ysvs if int(site.split('_')[-1])==previous_visit_no)
        return previous_visit

    def list_all_sbets(self):
        all_sbets = self.list_general(self.daily_bucket)
        return all_sbets
    
    def download_specific_tiles(self,year_site_v,product,tileNames,ext=None):
        #eg. year_site_v='YYYY_SITE_#' ; product='L3/Camera/Mosaic/'
        site=siteClass(year_site_v)
        site.get_dirs()
        download_dir = os.path.join(site.root_dir,'\\'.join(product.split('/')))
        make_dir(download_dir)
        # if 'ORNL' in year_site_v:
        #     print('ORNL - bucket neon-aop-ornl-pending')
        #     bucket = self.storage_client.bucket(self.ornl_bucket)
        # else:
        #     bucket = self.storage_client.bucket(self.products_bucket)
        bucket = self.storage_client.bucket(self.products_bucket)
        if ext==None:
            for gcs_object in bucket.list_blobs(prefix=site.year+'/FullSite/'+site.domain+'/'+year_site_v+'/'+product):
                path, filename = os.path.split(gcs_object.name)
                if any(tileNames.str.contains(filename.rsplit('_', 1)[0], regex = True)):
                    print('Downloading: ',filename)
                    gcs_object.download_to_filename(os.path.join(download_dir,filename))
        else:
            for gcs_object in bucket.list_blobs(prefix=site.year+'/FullSite/'+site.domain+'/'+year_site_v+'/'+product):
                path, filename = os.path.split(gcs_object.name)
                if filename.endswith(ext):
                    if any(tileNames.str.contains(filename.rsplit('_', 1)[0], regex = True)):
                        print('Downloading: ',filename)
                        gcs_object.download_to_filename(os.path.join(download_dir,filename))


    def download_gps(self,gps,mission_name,all_dates=False):
        mission = missionClass(mission_name)
        prefix = mission.year+'/'+gps
#         del mission
        bucket = self.storage_client.bucket(self.gps_bucket)
        for gcs_object in bucket.list_blobs(prefix=prefix):
            path, filename = os.path.split(gcs_object.name)
            download_path = os.path.join('D:\\GPS',mission.year,gps)
            make_dir(download_path)
#             print(filename)
            if not all_dates:
                if mission.flightday[:-2] in filename and (filename.endswith('.T02') or filename.endswith('.T01')):
                    print('Downloading: ',filename,'to',download_path)
                    print(gcs_object.name)
                    gcs_object.download_to_filename(os.path.join(download_path,filename))
            else:
                print('Downloading: ',filename)
                gcs_object.download_to_filename(os.path.join(download_path,filename))

    def download_sbet(self,mission_name=None):
        mission = missionClass(mission_name)
        prefix = mission.year+'/Daily/'+mission_name+'/L1/GPSIMU'
        mission.get_dirs()
        sbet_dir = mission.sbet_dir
        print('sbet directory :',sbet_dir)
        del mission
        make_dir(sbet_dir)
        bucket = self.storage_client.bucket(self.daily_bucket)
        for gcs_object in bucket.list_blobs(prefix=prefix):
            path, filename = os.path.split(gcs_object.name)
            if filename.startswith('sbet') | filename.startswith('smrmsg'):
                print('Downloading: ',filename)
                print('Path: ', path)
                gcs_object.download_to_filename(os.path.join(sbet_dir,filename))

    def download_sbet_qa(self,mission_name=None):
        mission = missionClass(mission_name)
        prefix = mission.year+'/Daily/'+mission_name+'/QA/GPSIMU'
        mission.get_dirs()
        sbet_dir = mission.sbet_dir
        del mission
        make_dir(sbet_dir)
        bucket = self.storage_client.bucket(self.daily_bucket)
        for gcs_object in bucket.list_blobs(prefix=prefix):
            path, filename = os.path.split(gcs_object.name)
            if filename.endswith('.pdf'):
                print('Downloading: ',filename)
                print('Path: ', path)
                gcs_object.download_to_filename(os.path.join(sbet_dir,filename))

    def download_products(self,year_site_v,product,ext=None):
        # eg. year_site_v='YYYY_SITE_#' ; product='L3/Camera/Mosaic/'
        site=siteClass(year_site_v)
        site.get_dirs()
        download_dir = os.path.join(site.root_dir,'\\'.join(product.split('/')))
        make_dir(download_dir)
        if 'ORNL' in year_site_v:
            print('ORNL - bucket neon-aop-ornl-pending')
            bucket = self.storage_client.bucket(self.ornl_bucket)
        else:
            bucket = self.storage_client.bucket(self.products_bucket)
        if ext==None:
            for gcs_object in bucket.list_blobs(prefix=site.year+'/FullSite/'+site.domain+'/'+year_site_v+'/'+product):
                path, filename = os.path.split(gcs_object.name)
#                print('Downloading: ',filename)
                gcs_object.download_to_filename(os.path.join(download_dir,filename))
        else:
            for gcs_object in bucket.list_blobs(prefix=site.year+'/FullSite/'+site.domain+'/'+year_site_v+'/'+product):
                path, filename = os.path.split(gcs_object.name)
                if filename.endswith(ext):
                    print('Downloading: ',filename)
                    gcs_object.download_to_filename(os.path.join(download_dir,filename))

    def download_proc(self,year_site_v,download_dir=None,ext=None):
        site=siteClass(year_site_v)
        site.get_dirs()
        bucket = self.storage_client.bucket(self.process_bucket)
        if ext == None:
            for gcs_object in bucket.list_blobs(prefix=site.year+'/FullSite/'+site.domain+'/'+year_site_v+'/Processing/DiscreteLidar/LMS/'):
                path, filename = os.path.split(gcs_object.name)
                if download_dir == None: #use default download directory on D: drive
                    download_dir = os.path.join('D:\\','\\'.join(path.split('/')))
                make_dir(download_dir)
                print('Downloading: ',filename)
                gcs_object.download_to_filename(os.path.join(download_dir,filename))
        else:
            for gcs_object in bucket.list_blobs(prefix=site.year+'/FullSite/'+site.domain+'/'+year_site_v+'/Processing/DiscreteLidar/LMS/'):
                path, filename = os.path.split(gcs_object.name)
                if download_dir == None: #use default download directory on D: drive
                    download_dir = os.path.join('D:\\','\\'.join(path.split('/')))
                make_dir(download_dir)
                if filename.endswith(ext):
                    print('Downloading: ',filename)
                    gcs_object.download_to_filename(os.path.join(download_dir,filename))

    def download_qa(self,year_site_v,qa_sensor,ext,download_dir=None):
        # year_site_v='YYYY_SITE_#'
        # qa_sensor='DiscreteLidar' or 'Spectrometer'
        # ext = 'CHM.tif' or 'LAI_tiles.tif' (eg.)
        site = siteClass(year_site_v)
        site.get_dirs()
        bucket=self.storage_client.bucket(self.process_bucket)
#         if ext==None:
        if qa_sensor == 'DiscreteLidar':
            for gcs_object in bucket.list_blobs(prefix=site.year+'/FullSite/'+site.domain+'/'+year_site_v+'/QA/DiscreteLidar/ProcessingReport/'):
                path, filename = os.path.split(gcs_object.name)
                if download_dir == None: #use default download directory on D: drive
                    download_dir = os.path.join('D:\\','\\'.join(path.split('/')))
                make_dir(download_dir)
                if filename.endswith(ext):
                    print('Downloading: ',path,filename)
                    gcs_object.download_to_filename(os.path.join(download_dir,filename))
        if qa_sensor == 'Spectrometer':
            for gcs_object in bucket.list_blobs(prefix=site.year+'/FullSite/'+site.domain+'/'+year_site_v+'/QA/Spectrometer/L3Hists/'):
                path, filename = os.path.split(gcs_object.name)
                if download_dir == None: #use default download directory on D: drive
                    download_dir = os.path.join('D:\\','\\'.join(path.split('/')))
                make_dir(download_dir)
                if filename.endswith(ext):
                    print('Downloading: ',path,filename)
                    gcs_object.download_to_filename(os.path.join(download_dir,filename))

    def download_general(self,year_site_v,bucket,prefix,download_dir=None,ext=None):
        # eg. year_site_v='YYYY_SITE_#'
        # prefix='2019/FullSite/D01/2019_HOPB_3/L3/Camera/Mosaic/'
        site = siteClass(year_site_v)
        site.get_dirs()
        bucket = self.storage_client.bucket(bucket)
        for gcs_object in bucket.list_blobs(prefix=prefix):
            path, filename = os.path.split(gcs_object.name)
            if download_dir == None: #use default download directory on D: drive
                download_dir = os.path.join('D:\\','\\'.join(path.split('/')))
            make_dir(download_dir)
            if ext is None:
                print('Downloading: ',filename)
                gcs_object.download_to_filename(os.path.join(download_dir,filename))
            else:
                if filename.endswith(ext):
                    print('Downloading: ',filename,' to ',download_dir)
                    print(gcs_object.name)
                    gcs_object.download_to_filename(os.path.join(download_dir,filename))
    
    def download_l0(self,mission_name,l0_folder,check_folder='raw',ext=None):
        mission=missionClass(mission_name)
        prefix = mission.year+'/Zero/'+mission.mission_name+'/L0/'+l0_folder
        print(prefix)
        bucket = self.storage_client.bucket(self.l0_bucket)
        for gcs_object in bucket.list_blobs(prefix=prefix):
            path, filename = os.path.split(gcs_object.name)
#             print(filename)
            download_dir = os.path.join('D:\\Raw\\',mission.year,mission.payload,mission.campaign,'\\'.join(path.split('/')))
            download_dir = download_dir.replace(mission.year+'\\Zero\\','\\')
            gcs_object = bucket.get_blob(gcs_object.name)
#             print(download_dir)
            if ext is None:
                make_dir(download_dir)
                if check_folder == 'raw':
                    if os.path.isfile(os.path.join(download_dir,filename)) and os.path.getsize(os.path.join(download_dir,filename))==gcs_object.size:
                        print('Skipping file',filename,' - already exists in raw folder',download_dir)
                    else:
                        print('Downloading',filename,'to',download_dir)
                        gcs_object.download_to_filename(os.path.join(download_dir,filename))
                else:
                    if (os.path.isfile(os.path.join(check_folder,filename)) and os.path.getsize(os.path.join(check_folder,filename))==gcs_object.size) or (os.path.isfile(os.path.join(download_dir,filename)) and os.path.getsize(os.path.join(download_dir,filename))==gcs_object.size):
                        print(os.path.join(check_folder,filename))
                        print('Skipping file',filename,' - already exists in folder',check_folder)
                    else:
                        print('Downloading',filename,'to',download_dir)
                        gcs_object.download_to_filename(os.path.join(download_dir,filename))
            else:
                if filename.endswith(ext):
#                     print(filename)
                    make_dir(download_dir)
                    if check_folder == 'raw' and os.path.isfile(os.path.join(download_dir,filename)) and os.path.getsize(os.path.join(download_dir,filename)) == gcs_object.size:
                            print('Skipping file',filename,' - already exists in raw folder',download_dir)
                            continue
                    else:
                        if (os.path.isfile(os.path.join(check_folder,filename)) and os.path.getsize(os.path.join(check_folder,filename)) == gcs_object.size) or (os.path.isfile(os.path.join(download_dir,filename)) and os.path.getsize(os.path.join(download_dir,filename)) == gcs_object.size):
#                             print(os.path.join(check_folder,filename))
                            print('Skipping file',filename,' - already exists in folder',check_folder)
                        else:
                            print('Downloading',filename,'to',download_dir)
                            gcs_object.download_to_filename(os.path.join(download_dir,filename))

    def download_gpsimu(self,mission_name,check_folder='raw'):
        mission=missionClass(mission_name)
        prefix = mission.year+'/Zero/'+mission.mission_name+'/L0/GPSIMU/'
        print(prefix)
        bucket = self.storage_client.bucket(self.l0_bucket)
        for gcs_object in bucket.list_blobs(prefix=prefix):
            path, filename = os.path.split(gcs_object.name)
            download_dir = os.path.join('D:\\Raw\\',mission.year,mission.payload,mission.campaign,'\\'.join(path.split('/')))
            download_dir = download_dir.replace(mission.year+'\\Zero\\','\\')
            gcs_object = bucket.get_blob(gcs_object.name)
            # filter by files that end in a # (this only downloads POS files, eg. .001, .002, ...)
            if filename.split('.')[-1].isdigit():
                make_dir(download_dir)
                if check_folder == 'raw' and os.path.isfile(os.path.join(download_dir,filename)) and os.path.getsize(os.path.join(download_dir,filename)) == gcs_object.size:
                        print('Skipping file',filename,' - already exists in raw folder',download_dir)
                        continue
                else:
                    if (os.path.isfile(os.path.join(check_folder,filename)) and os.path.getsize(os.path.join(check_folder,filename)) == gcs_object.size) or (os.path.isfile(os.path.join(download_dir,filename)) and os.path.getsize(os.path.join(download_dir,filename)) == gcs_object.size):
                        print('Skipping file',filename,' - already exists in folder',check_folder)
                    else:
                        print('Downloading',filename,'to',download_dir)
                        gcs_object.download_to_filename(os.path.join(download_dir,filename))


class lookupClass:
    def __init__(self):
        self.site_lookup_dir="D:/Gold_Pipeline/ProcessingPipelines/res/Lookups/"
        self.epsg_lookup=os.path.join(self.site_lookup_dir,'EPSG_lookup.txt')
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
        epsg_code=self.epsg_dict[str(int(utm_zone))+' '+hem]
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
            sys.exit()
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