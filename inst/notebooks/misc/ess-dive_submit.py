import requests
import os
import json

## Setup
token = "eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJodHRwOlwvXC9vcmNpZC5vcmdcLzAwMDAtMDAwMS03OTI0LTA0MFgiLCJmdWxsTmFtZSI6Ikh1Z2ggV29yc2hhbSIsImlzc3VlZEF0IjoiMjAyNC0wNi0yMlQyMDo1MTowNy4wNDgrMDA6MDAiLCJjb25zdW1lcktleSI6InRoZWNvbnN1bWVya2V5IiwiZXhwIjoxNzE5MTU0MjY3LCJ1c2VySWQiOiJodHRwOlwvXC9vcmNpZC5vcmdcLzAwMDAtMDAwMS03OTI0LTA0MFgiLCJ0dGwiOjY0ODAwLCJpYXQiOjE3MTkwODk0Njd9.xLFDWKb4uLduPAvQbj2Pq-LenFHzWaSBg_FkHdTTmfYLGbBzY9hbAZHUriMzdOpMtCJmzv2SL0q9jd7Xne2aZxQap61KBbZC7E9VhbXHsA_5cVTBK7-oZIajnH600nw_dkvUZoVKS3I1NJVwo3qvkn9TX9DjFaUaAcWaxVd6w3GEo78mRDiLglxsOxd_dRiCxuPmbWHn2i5roWc4-3mxYX4XIDA3uPJici4W4xrVJDnJ-K5yAvMm4tM9GvrgU_GP4DC-Q8QaWLKFKvyJ4_F5iOl3L-3YAvD2SNrp2mZ7KoqBitWiekqoZw9oCzsSeq0CFAko98T31OanEXCfOYGdwQ"
base = "https://api-sandbox.ess-dive.lbl.gov/"
header_authorization =  "bearer {}".format(token)
endpoint = "packages"

## Metadata
### Project
provider_wf = {
    "identifier": {
        "@type": "PropertyValue",
        "propertyID": "ess-dive",
        "value": "024b867d-25e3-4025-b68c-6598aded7aa2"
    }
 }
### Creators
creators =  [
   {
     "givenName": "Tristan",
     "familyName": "Goulden",
     "affiliation": "Battelle",
     "email": "tgoulden@battelleecology.org"
   },
   {
     "@id": "http://orcid.org/0000-0001-7924-040X",
     "givenName": "H. Marshall",
     "familyName": "Worsham",
     "affiliation": "University of California Berkeley",
     "email": "worsham@berkeley.edu"
   },
   {
     "givenName": "Bridget",
     "familyName": "Hass",
     "affiliation": "Battelle",
     "email": "bhass@battelleecology.org"
   },
   {
     "@id": "https://orcid.org/0000-0002-8453-8435",
     "givenName": "Eoin",
     "familyName": "Brodie",
     "affiliation": "Lawrence Berkeley National Laboratory",
     "email": "elbrodie@lbl.gov"
   },
   {
     "@id": "https://orcid.org/0000-0002-5633-4865",
     "givenName": "K. Dana",
     "familyName": "Chadwick",
     "affiliation": "NASA Jet Propulsion Laboratory",
     "email": "katherine.d.chadwick@jpl.nasa.gov"
   },
   {
     "@id": "https://orcid.org/0000-0003-3307-6098",
     "givenName": "Nicola",
     "familyName": "Falco",
     "affiliation": "Lawrence Berkeley National Laboratory", 
     "email": "nicolafalco@lbl.gov"
   },
   {
    "@id": "https://orcid.org/0000-0002-5982-6064",
    "givenName": "Kate", 
    "familyName": "Maher", 
    "affiliation": "Stanford University",
    "email": "kmaher@stanford.edu"      
   },
   {
    "@id": "https://orcid.org/0000-0002-2140-6072",
    "givenName": "Haruko", 
    "familyName": "Wainwright", 
    "affiliation": "Massachusetts Institute of Technology", 
    "email": "hmwainw@mit.edu",
   }, 
   {
    "@id": "https://orcid.org/0000-0002-3568-1155",
    "givenName": "Kenneth", 
    "familyName": "Williams", 
    "affiliation": "Lawrence Berkeley National Laboratory", 
    "email": "khwilliams@lbl.gov"
   }
 ]
creators

# Rest of the JSON-LD object
json_ld = {
 "@context": "http://schema.org/",
 "@type": "Dataset",
 "name": "NEON AOP Survey of Upper East River CO Watersheds: Waveform LiDAR Binary Data",
 "description": [
   "The waveform LiDAR data in this package were generated through a National Ecological Observatory Network Airborne Observation Platform (NEON AOP) acquisition over watersheds of interest surrounding Crested Butte, Colorado. The package contains 97 compressed file archives in 7-zip (.7z) format, each corresponding to one acquisition flightpath. Within each .7z archive is a set of constituent files describing properties of the LiDAR waveforms, such as return intensity, geolocation, outgoing pulse and other behavior of the sensor and signals. Once downloaded, the files must first be unzipped using the widely distributed command-line software utility 7z, using the command '7z x \[filename\].7z \[target_directory\]'. All files within the .7z archives can be opened in IDL, MatLab, or the open-source R statistical computing environment. Further details about the data package are in the attached user guide (neon_aop_crbu_waveformlidar_userguide.pdf)."
 ],
 "creator": creators,
 "datePublished": "2024-07-12",
 "keywords": [
   "EARTH SCIENCE > BIOSPHERE > VEGETATION",
   "EARTH SCIENCE > LAND SURFACE > TOPOGRAPHY",
   "Remote sensing", 
   "LiDAR", 
   "Waveform", 
   "NEON AOP"
 ],
 "variableMeasured": [
   "Waveform LiDAR",
   "LiDAR return intensity"
 ],
 "license": "http://creativecommons.org/licenses/by/4.0/",
 "spatialCoverage": [
   {
     "description": "The NEON AOP acquired the waveform LiDAR data over a 334 km<sup>2</sup> footprint in the East River (ER) watershed, a snow‐dominated headwater basin of the Upper Colorado River Basin (UCRB) located in the western United States. The ER is the designated testbed of Lawrence Berkeley National Laboratory's Watershed Function Scientific Focus Area (WFSFA). The ER is considered representative of many snow‐dominated headwaters of the Rocky Mountains. The study domain encompasses pristine alpine, subalpine, montane, and riparian ecosystems. The ER contains high‐energy mountain streams to low‐energy meandering floodplains and is eroding primarily into the Cretaceous, carbon‐rich, marine shale of the Mancos Formation.",
     "geo": [
       {
         "name": "Northwest",
         "latitude": 39.040,
         "longitude": -107.129
       },
       {
         "name": "Southeast",
         "latitude": 38.814,
         "longitude": -107.129
       }
     ]
   }
 ],
 "funder": {
   "@id": "http://dx.doi.org/10.13039/100006206",
   "name": "U.S. DOE > Office of Science > Biological and Environmental Research (BER)"
 },
 "temporalCoverage": {
   "startDate": "2018-06-12",
   "endDate": "2018-06-26"
 },
 "editor": {
   "@id": "http://orcid.org/0000-0001-7924-040X",
   "givenName": "H. Marshall",
   "familyName": "Worsham",
   "email": "worsham@berkeley.edu"
 },
 "provider": provider_wf,
 "measurementTechnique": [
   "Lawrence Berkeley National Laboratory (LBNL) contracted the NEON AOP to observe watersheds of interest surrounding Crested Butte, Colorado, with airborne remote sensing data, including LiDAR. The waveform LiDAR files in this package are the original data provided by NEON. The flight box design encompassed the watersheds, surveying a total area of 334 km<sup>2</sup> across 102 lines. The instrument used was an Optech Gemini, with a pulse density of 2-9 pulses m-2 across the study area (see final report document for detailed information). LiDAR data were processed with the Optech LMS software. This generated both LAS and ascii formats, and the ascii data were then read into the waveform processor. The waveform processor is IDL code written within NEON to read in the raw Optech waveform format, synchronize GPS time tags, calculate range, and then geolocate the waveforms onto the Earth surface. Currently the output waveform product format is a series of flat binary files for easy reading into R, IDL, Matlab, or equivalent analytical software."
 ]
}

# Submit metadata and data files
files_tuples_array = []
files_upload_directory = "/global/scratch/users/worsham/waveform_lidar"
files = os.listdir(files_upload_directory)

files_tuples_array.append((("json-ld", json.dumps(json_ld))))

for filename in files:
   file_directory = files_upload_directory + filename
   files_tuples_array.append((("data", open(file_directory, 'rb'))))

post_packages_url = "{}{}".format(base,endpoint)
post_package_response = requests.post(post_packages_url,
                                    headers={"Authorization":header_authorization},
                                    files= files_tuples_array)

if post_package_response.status_code == 201:
   # Success
   response=post_package_response.json()
   print(f"View URL:{response['viewUrl']}")
   print(f"Name:{response['dataset']['name']}")
else:
   # There was an error
   print(post_package_response.text)