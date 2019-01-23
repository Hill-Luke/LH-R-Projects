library(tidyverse)
library(readxl)
#reading in the crosswalks
zcta_cid_xwalk<-read_excel("/Users/lukeofthehill/Desktop/Data Prep/Data/Community area and ZCTA.xlsx", sheet="Community area and zip code equ", col_names=TRUE)
zip_to_zicta<-read_excel("/Users/lukeofthehill/Desktop/Data Prep/Data/zip_to_zcta_2018.xlsx", col_names=TRUE)

#limiting the zip-to-zcta xwalk to only the needed columns
zicta<-select( zip_to_zicta,c("ZIP_CODE", "ZCTA")) 

#renaming the zcta column
zcta_cid_xwalk$ZCTA<-zcta_cid_xwalk$ZCTA5

#joining zipcode to the community id crosswalk by ZCTA
full_xwalk<-left_join(zcta_cid_xwalk,zicta, by="ZCTA", copy=FALSE)

#reading in the chicago clinic location data
clinics<-read.table(file = '/Users/lukeofthehill/Desktop/Data Prep/Data/Final Project/Chicago_Department_of_Public_Health_Clinic_Locations.tsv', sep = '\t', header = TRUE)

clinics$ZIP_CODE<-c(clinics$ZIP)
full_xwalk$ZIP_CODE<-c(full_xwalk$ZIP_CODE)

#Zip code are two different variable types
is.numeric(clinics$ZIP)
is.character(full_xwalk$ZIP_CODE)
full_xwalk$ZIP<-as.numeric(full_xwalk$ZIP_CODE)
is.numeric(full_xwalk$ZIP)

CommID<-select(full_xwalk, "CHGOCA","ZIP")

#Joining Community ID to the clinic table by Zip Code
clinic_full<-left_join(clinics,CommID, by = "ZIP")

#There are multiple ZIPs per Comm_ID


##Condom Distribution Data
condom_dist<-read_excel("/Users/lukeofthehill/Desktop/Data Prep/Data/Final Project/Condom_Distribution_Sites_LH.xlsx", col_names = TRUE)

condom_dist$ZIP<-as.numeric(condom_dist$`ZIP Code`)
condom_full<-left_join(condom_dist,CommID, by = "ZIP")

SUD_PROV<-read_excel("/Users/lukeofthehill/Desktop/Data Prep/Data/Final Project/SUD_PROVIDERS.xls", col_names = TRUE)

is.numeric(SUD_PROV$ZIP_CODE)
SUD_PROV$ZIP<-SUD_PROV$ZIP_CODE
SUD_PROV_FULL<-left_join(SUD_PROV,CommID, by = "ZIP")


