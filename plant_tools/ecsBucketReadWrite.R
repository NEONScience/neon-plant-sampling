### Use ECS Bucket Functions to get and put files from and to an ECS bucket

### Setup ####
#   Load required libraries
library(aws.s3)
library(dplyr)
library(pbapply)
library(stringr)

#   Load ECS Keys, Secrets, and Functions --> customize as necessary
if(file.exists("/Users/Pajaro")){
  source("~/Box/computing/ecsBucketKeys.R")
  funPath <- "~/Documents/workDocuments/gitRepositories/neon-plant-sampling/plant_tools"
  source(paste(funPath, "ecsBucketFunctions.R", sep = "/"))
}

#   DHP: Set System Environment to authenticate to AWS
Sys.setenv("AWS_ACCESS_KEY_ID" = cmDhpKey, "AWS_SECRET_ACCESS_KEY" = cmDhpSecret,
           "AWS_S3_ENDPOINT" = "neonscience.org", "AWS_DEFAULT_REGION" = "s3.data")

#   Microbe Seq Data: Set System Environment to authenticate to AWS
Sys.setenv("AWS_ACCESS_KEY_ID" = micKey, "AWS_SECRET_ACCESS_KEY" = micSecret,
           "AWS_S3_ENDPOINT" = "neonscience.org", "AWS_DEFAULT_REGION" = "s3.data")



### Read ECS bucket contents ####
#   Microbe bucket--> change 'max = 100' to 'max = Inf' to retrieve listing of complete bucket contents
temp <- aws.s3::get_bucket_df(bucket='neon-microbial-raw-seq-files', check_region = FALSE, max = 10)
temp <- temp %>% filter(Size != 0)

#   DHP bucket
temp <- aws.s3::get_bucket_df(bucket='neon-dhp-images', check_region = FALSE, max = 10)
temp <- temp %>% filter(Size != 0)



### Get files from ECS bucket ####
#   Define bucket and path to where files will be written
bucket <- 'neon-microbial-raw-seq-files'
writePath <- '~/Desktop'

#   Example with single file
get_file_ecs(objectKey = temp$Key[1], volumePath = writePath, ecsBucket = bucket)

#   Example with file list and pblapply
pbapply::pblapply(X = temp$Key, FUN = get_file_ecs,
                 ecsBucket = bucket, volumePath = writePath)



### Put files to ECS bucket ####
##  Example of reading source files and defining objectKey values
#   Define bucket to which source files are put and path for listing source files
bucket <- 'neon-microbial-raw-seq-files'
volumePath <- "~/Desktop"

#   Create list of source files
putFiles <- list.files(path = volumePath, full.names = TRUE, recursive = TRUE)
putFiles <- data_frame(putFiles) %>% rename(filePaths = putFiles)

#   Create objectKey values from source file path information --> this needs customizing
putFiles$keys <- putFiles$filePaths

#   Define list of paths, keys, and MoreArgs needed for function
mapply(FUN = put_file_ecs, putFiles$filePaths, putFiles$keys,
       MoreArgs = list(ecsBucket = bucket))