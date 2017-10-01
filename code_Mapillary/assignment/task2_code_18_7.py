#!/usr/bin/python

#libraries------------------------------
import math
import sys
from PIL import Image
import piexif
import time
from datetime import datetime 
#end of libraries---------------------------------------


#constants------------------------------
WGS84_a = 6378137.0
WGS84_b = 6356752.314245
#end of constants---------------------------------------


#functions------------------------------
def ecef_from_lla(lat, lon, alt):
	'''
	Compute ECEF XYZ from latitude, longitude and altitude.
	All using the WGS94 model.
	Altitude is the distance to the WGS94 ellipsoid.
	Check results here http://www.oc.nps.edu/oc2902w/coord/llhxyz.htm
	'''
	a2 = WGS84_a**2
	b2 = WGS84_b**2
	lat = math.radians(lat)
	lon = math.radians(lon)
	L = 1.0 / math.sqrt(a2 * math.cos(lat)**2 + b2 * math.sin(lat)**2)
	x = (a2 * L + alt) * math.cos(lat) * math.cos(lon)
	y = (a2 * L + alt) * math.cos(lat) * math.sin(lon)
	z = (b2 * L + alt) * math.sin(lat)
	return x, y, z


def gps_distance(latlon_1, latlon_2):
	'''
	Distance between two (lat,lon) pairs.
	>>> p1 = (42.1, -11.1)
	>>> p2 = (42.2, -11.3)
	>>> 19000 < gps_distance(p1, p2) < 20000
	True
	'''
	x1, y1, z1 = ecef_from_lla(latlon_1[0], latlon_1[1], 0.)
	x2, y2, z2 = ecef_from_lla(latlon_2[0], latlon_2[1], 0.)

	dis = math.sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2 + (z1 - z2) ** 2)

	return dis


def parse_json(inpFile):
	'''
	Parse a json format metadata file(inpFile) and return the parsed data in a dictionary.
	>>> dict=parse_json('metadata.json')
	'''

	meta_json_dictionary = {}

	with open(inpFile,'r') as metaFile: 
		
		for line in metaFile:

			if line.find("}")!= -1:#if this sign flush into dictionary, using the capture time as the key, to be able to extract in order
				meta_json_dictionary[capture_time] = {'filename' : filename, 'altitude' : altitude, 'longitude' : longitude, 
				'latitude' : latitude, 'heading' : heading, 'capture_time' : capture_time}
			if line.find("altitude")!= -1:
				altitude = float(line.split(":")[1].split(",")[0].strip())
			if line.find("longitude")!= -1:
				longitude = float(line.split(":")[1].split(",")[0].strip())
			if line.find("filename")!= -1:
				filename = line.split(":")[1].split(",")[0].strip().replace('"',"")
			if line.find("latitude")!= -1:
				latitude = float(line.split(":")[1].split(",")[0].strip())
			if line.find("heading")!= -1:
				heading = float(line.split(":")[1].split(",")[0].strip())
			if line.find("capture_time")!= -1:
				capture_time_string = line.split(":")[1].split(",")[0].strip().replace('"',"")
				#change the capture time in epoch miliseconds to be able to use the key for order and to have a unified representation
				capture_time=int(datetime.strptime(capture_time_string,"%Y%m%d-%H_%M_%S_%f").strftime("%s")) * 1000

	return meta_json_dictionary


def parse_txt(inpFile):
	'''
	Parse a txt format metadata file(inpFile) and return the parsed data in a dictionary.
	>>> dict=parse_txt('metadata.txt')
	'''

	meta_txt_dictionary={}

	with open(inpFile,'r') as metaFile:

		#read the header remove the ending newline and split on the space to get the column names
		metaHeader = metaFile.readline().strip().split(" ")

		#get indexes of the column names to make it robust to order
		filenameIndex = metaHeader.index("filename")
		latitudeIndex = metaHeader.index("latitude")
		longitudeIndex = metaHeader.index("longitude")
		headingIndex = metaHeader.index("heading")
		altitudeIndex = metaHeader.index("altitude")
		capture_timeIndex = metaHeader.index("capture_time(epoch_in_milliseconds)")

		for line in metaFile:
			
			#split on the space to get the column values
			splitLine = line.split(" ")
			
			#save each column values into the appropriate variable
			filename = splitLine[filenameIndex]
			latitude = float(splitLine[latitudeIndex])
			longitude = float(splitLine[longitudeIndex])
			heading = float(splitLine[headingIndex])
			altitude = float(splitLine[altitudeIndex])
			capture_time = float(splitLine[capture_timeIndex])

			#save into the dictionaty, using the capture time as the key, to be able to extract in order
			meta_txt_dictionary[capture_time] = {'filename' : filename, 'altitude' : altitude, 'longitude' : longitude, 
				'latitude' : latitude, 'heading' : heading, 'capture_time' : capture_time}

	return meta_txt_dictionary


def deg_to_deg_min_sec(floatyDeg):
	'''
	Convert the degrees in float to degrees, minutes and seconds as tuple of tuples with precision identifier.
	>>> latitude = 23.122886
	>>> deg_to_deg_min_sec(latitude)
	((23.0, 1), (7.0, 1), (223896.0, 10000))
	'''
	degies = math.floor(floatyDeg)
	minies = math.floor((floatyDeg-degies)*60)
        secies = math.floor((floatyDeg-degies-minies/60) * 36000000)

	return ((degies, 1), (minies, 1), (secies, 10000))


def encode_metadata_into_EXIF(imgDictCurrent, imgPath):
	'''
	Encode the metadata into an EXIF format, using the piexif module.
	Inputs are a dictionary of image metadata and a path to the image.
	>>> metadataDicty = parse_json('metadata.txt')
	>>> keylist = [key for key in sorted(metadataDicty.keys())]
	>>> path_to_images = '../exif_images/'
	>>> review_and_encode_metadata_into_EXIF(metadataDicty[keylist[0]], path_to_images)	
	'''

	#encode into EXIF
	img = Image.open(imgPath+imgDictCurrent["filename"])
	exif_dict = piexif.load(imgPath+imgDictCurrent["filename"])

	exif_dict["Exif"][piexif.ExifIFD.DateTimeOriginal] = time.strftime('%Y-%m-%d %H:%M:%S', time.gmtime(imgDictCurrent["capture_time"]/1000.0))#CaptureTime

	exif_dict["GPS"][piexif.GPSIFD.GPSAltitudeRef] = 0#AltitudeRef

	exif_dict["GPS"][piexif.GPSIFD.GPSAltitude] = (int(abs(imgDictCurrent["altitude"])*100),100)#Altitude

	exif_dict["GPS"][piexif.GPSIFD.GPSImgDirectionRef] = "T"#ImgDirectionRef

	exif_dict["GPS"][piexif.GPSIFD.GPSImgDirection] = (int(abs(imgDictCurrent["heading"])*100),100)#ImgDirection

	exif_dict["GPS"][piexif.GPSIFD.GPSLongitudeRef] = "E"#LongitudeRef

	exif_dict["GPS"][piexif.GPSIFD.GPSLongitude] = deg_to_deg_min_sec(imgDictCurrent["longitude"])#Longitude

	exif_dict["GPS"][piexif.GPSIFD.GPSLatitudeRef] = "N"#LatitudeRef

	exif_dict["GPS"][piexif.GPSIFD.GPSLatitude] = deg_to_deg_min_sec(imgDictCurrent["latitude"])#Latitude
	

	exif_bytes = piexif.dump(exif_dict)
	img.save(imgPath+imgDictCurrent["filename"], "jpeg", exif=exif_bytes)


def check_gps_dist(imgDictCurrent, imgDictPrevious):
	'''
	Check the gps distance between two images.
	Inputs are the images' metadata dictionaries.
	>>> metadataDicty = parse_json('metadata.txt')
	>>> keylist = [key for key in sorted(metadataDicty.keys())]
	>>> imageDictPrevious = metadataDicty[keylist[0]]
	>>> imageDictCurrent = metadataDicty[keylist[1]]
	>>> check_gps_dist(imageDictCurrent, imageDictPrevious)	
	'''

	gpsDist = gps_distance((imgDictPrevious["latitude"], imgDictPrevious["longitude"]), (imgDictCurrent["latitude"], imgDictCurrent["longitude"]))

	if gpsDist == 0:
		print "The longitude, latitude based gps distance between images ",imgDictPrevious["filename"],",",imgDictCurrent["filename"]," is equal to zero.\n"
	elif gpsDist > 1000:
		print "The longitude, latitude based gps distance between images ",imgDictPrevious["filename"],",",imgDictCurrent["filename"]," is larger than one kilometer.\n"			 				


#end of functions---------------------------------------


#main-----------------------------------

#check the number of arguments, in order to raise an error and quit if there are not sufficient
if len(sys.argv)<3:
	print "ERROR: Provide the file to be parsed and the path to the images in this order."
	quit()

#first argument is the input metadata file
inputFile = sys.argv[1]
#extract all strings between "." into a list
splits = inputFile.split(".")
#the last item should be the ending
ending = splits[len(splits)-1]

#second argument is the path to the images
imagePath = sys.argv[2]

#check the ending of the input file to determine the file format and call the appropriate function to parse the data in the dictionary
if ending == "json":
	metadataDicty = parse_json(inputFile)
elif ending == "txt":
	metadataDicty = parse_txt(inputFile)
else :
	#if the ending does not match neither of the two, raise an error and quit
	print "ERROR: File ending not recognizable, accepted file endings are .json and .txt"
	quit()

#extract the keys of the input dictionary, containing the image metadata, sort the keys as they are the capture time, 
#for each image encode the metadata into an EXIF format, while checking for gps abnormalities
keylist = [key for key in sorted(metadataDicty.keys())]

for key in range(0,len(keylist)):

	imageDictCurrent = metadataDicty[keylist[key]]

	encode_metadata_into_EXIF(imageDictCurrent, imagePath)

	if key == 0:#the first image does not have a preceding image, so gps distance is not calculated
		continue

	imageDictPrevious = metadataDicty[keylist[key-1]]

	check_gps_dist(imageDictCurrent, imageDictPrevious)

#end of main---------------------------------------
