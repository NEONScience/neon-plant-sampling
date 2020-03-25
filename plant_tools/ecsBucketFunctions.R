### Functions to work with files stored in an ECS bucket


##  Function to get DHPs from ECS bucket, then save DHPs in a user-defined location. 
##  For each image, the function re-creates the directory structure defined by the value in the ECS Key field.
get_file_ecs <- function(objectKey, volumePath, ecsBucket, ecsUrl){
  # [MODIFY] Get original filePath from ECS Key -> string indices below are specific to 12 char DHP filename
  # [MODIFY] A function other than str_sub may be more appropriate if file name is variable
  filePath <- stringr::str_sub(objectKey, start = 1, end = -13)
  
  # Create directory from filePath if it does not exist
  if(!file.exists(paste(volumePath, filePath, sep = "/"))){
    dir.create(paste(volumePath, filePath, sep = "/"), recursive = TRUE)
  }
  
  # Retrieve object from ECS with Key
  if(file.exists(paste(volumePath, objectKey, sep = "/"))){
    print(paste("File already exists at specified volumePath:", objectKey, sep = " ", quote = FALSE))
  } else {
    # Save object to volumePath with original directory structure specified by Key
    aws.s3::save_object(object = objectKey, bucket = ecsBucket, file = paste(volumePath, objectKey, sep = "/"),
                        check_region = FALSE, base_url = ecsUrl)
    # Report progress -> use with 'pbapply' package to report progress in real-time for long input lists
    print(paste("File saved to specified volumePath:", objectKey, sep = " "), quote = FALSE)
  }
}



##  Function to put a file into specified ECS bucket and define the ECS object Key for the file
##  Function checks to determine whether file already exists with specified object Key
put_file_ecs <- function(filePath, objectKey, ecsBucket, ecsUrl){
  # Determine whether file at filePath with specified objectKey exists in the ECS bucket
  if(aws.s3::head_object(object = objectKey, bucket = ecsBucket, base_url = ecsUrl, check_region = FALSE)){
    print(paste0("Object already exists in ", ecsBucket, ": ", objectKey), quote = FALSE)
  } else {
    # Put object into bucket with specified objectKey, and report successful transfer
    aws.s3::put_object(file = filePath, object = objectKey, bucket = ecsBucket, 
                       acl = "public-read", base_url = ecsUrl, check_region = FALSE)
    print(paste0("Successful put to ", ecsBucket, ": ", objectKey), quote = FALSE)
  }
}



##  Function to copy an existing object in specified ECS bucket to new object with new specified object Key in same bucket
##  Function checks to determine original object exists, and whether new object Key exists before copying
copy_file_ecs <- function(objectKey, newObjectKey, ecsBucket, ecsUrl){
  # Determine whether objectKey is in bucket
  if(!aws.s3::head_object(object = objectKey, bucket = ecsBucket, base_url = ecsUrl, check_region = FALSE)){
    print(paste0("Object not in specified bucket: ", objectKey), quote = FALSE)
  
  # Determine whether newObjectKey already exists in bucket
  } else if(aws.s3::head_object(object = newObjectKey, bucket = ecsBucket, base_url = ecsUrl, check_region = FALSE)){
    print(paste0("New objectKey already exists in bucket: ", newObjectKey), quote = FALSE)
  } else {
    # Copy objectKey to newObjectKey in the bucket, and status result
    aws.s3::copy_object(from_object = objectKey, to_object = newObjectKey, from_bucket = ecsBucket, to_bucket = ecsBucket,
                        acl = "public-read", base_url = ecsUrl, check_region = FALSE)
    print(paste0("Successful copy: ", objectKey, " to ", newObjectKey), quote = FALSE)
  }
}
