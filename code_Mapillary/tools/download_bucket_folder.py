import os
import json
import argparse
import shutil
import boto3
import zipfile
import numpy as np
from PIL import Image

#import models

# ------------------------------------------------------------------------------
#  Command Line Tool Interface
# ------------------------------------------------------------------------------

parser = argparse.ArgumentParser(
        description='Downloads images given by keys in a local file.')

parser.add_argument(
    '-o', '--output_dir',
    help='output directory',
    required=True)

parser.add_argument(
    '-r', '--remote_dir',
    help='remote directory',
    required=False)

parser.add_argument(
    '-b', '--bucket',
    help='amazon bucket',
    required=True)

parser.add_argument(
    '-a', '--all',
    help='download all folders in bucket',
    required=False, action="store_true")


def download_bucket_folder(bucket_name, remote_dir, output_dir):
    """
    Downloads all data given by the amazon bucket in the given remote directory and save it to the given local directory.
    """

    output_dir = os.path.dirname(output_dir)

    if len(output_dir) > 0 and not os.path.exists(output_dir):
        os.makedirs(output_dir)

    s3 = boto3.resource('s3')
    
    bucket = s3.Bucket(bucket_name)
 
    for object in bucket.objects.filter(Prefix = remote_dir):
        
        object_key = str(object.key)
        dir_path = ""
        for dir in map(lambda x: str(x).replace(" ", ""), object_key.split('/')[:-1]):
            local_dir = os.path.join(output_dir, dir_path, dir)
            if not os.path.exists(local_dir):
                os.makedirs(local_dir)
            dir_path = os.path.join(dir_path, dir)
        bucket.download_file(object_key, os.path.join(output_dir, object_key.replace(" ", "")))
 
def download_bucket(bucket_name, output_dir):
    """
    Downloads all data given by the amazon bucket and save it to the given local directory.
    """

    output_dir = os.path.dirname(output_dir)

    if len(output_dir) > 0 and not os.path.exists(output_dir):
        os.makedirs(output_dir)

    s3 = boto3.resource('s3')
    
    bucket = s3.Bucket(bucket_name)
    
    for object in bucket.objects.all():

        object_key = str(object.key)
        if "." in object_key:
            dir_path = ""
            for dir in map(lambda x: str(x).replace(" ", ""), object_key.split('/')[:-1]):
                local_dir = os.path.join(output_dir, dir_path, dir)
                if not os.path.exists(local_dir):
                    os.makedirs(local_dir)
                dir_path = os.path.join(dir_path, dir)
            bucket.download_file(object_key, os.path.join(output_dir, object_key.replace(" ", "")))
   
if __name__ == "__main__":

    args = parser.parse_args()
            
    if args.all:
        download_bucket(args.bucket, args.output_dir)
    else:
        download_bucket_folder(args.bucket, args.remote_dir, args.output_dir)

