This repository contains the necessary R code and data to reproduce the results of Parsons et al. (2021).
This project compared the efficacy of 4 different indices for small mammal density. Two indices were based 
on live trapping: minimum number alive and captures per unit effort. Two indices were based on camera
trapping: proportion of camera nights detecting a species and the number of independent photos. The data and 
scripts in this repository use raw capture and camera data to 1. estimate density 2. estimate index values and 
3. conduct linear regression to estimate the strength of relationships between indice and density estimates.


The code files are:

01_SECR-estimates_cam-only.R: 
This file uses the following data files:
1. SECR_CHIP_cam.txt - SECR input file for chipmunks
2. SECR_MOUSE_cam.txt - SECR input file for mice
3. SECR_Total_cam.txt - SECR input file for all small mammals
4. SECR_VOLE_cam.txt - SECR input file for voles
5. FullSession.csv - SECR trap grid for sites that had all capture occasions
6. Minustwotraps.csv - SECR trap grid for one site that only had 5 capture occasions

This file estimates the density of each small mammal group (mice, voles, chipmunks, total) at each site.
The output is the density data for each species at each site which can be compiled into the densityestimates.csv
file needed for the next script.

02_Index-estimates_cam_only.R

This file uses the following data files:
1. Stations.csv - list of camera locations at each site and camera locations
2. SmallMamTL1617.csv - processed camera data that includes each photo, site, camera, timestamp, and species present
3. densityestimates.csv - the density estimates produced by the first script
4. CaptureData.csv - the compiled capture data of all species

This file estimates the index values for each small mammal group at each site. The output is the DensityData.csv
file which contains the density estimates for each species and the index estimates for each species.

03_Analysis_cam-only.R

This file uses the following data file:
1. DensityData.csv - the density estimates and index estimates produced by scripts 1 and 2

This file takes the density and index estimates as input, conducts the analyses, and produces the results table and figures.

04_Bootstrap.R

This file is written to pair with scripts 02 and 03. It does not input any data,
but requires the same data as these files. This script runs the bootstrap procedure 
to estimate variation in camera index estimates and their respective linear regressions.