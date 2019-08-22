# load required libraries -------------------------------------------------
library(maptools)
library(raster)
library(rgdal)
library(sf)
library(pdftools)
library(tm)
library(RSpincalc)
library(RANN)
library(data.table)
library(e1071)
library(Metrics)
library(spatstat)
library(tmap)
library(gstat)
library(xts)
library(rgeos)
# Reading parcel boundary -------------------------------------------------
field_boundary <-
  readShapeSpatial(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/boundary/Kieftenweg_field_boundary.shp"
  )
crs(field_boundary) <- CRS('+init=EPSG:28992')
field_boundary <-
  spTransform(field_boundary, CRS('+init=EPSG:4326'))

# Reading Eurofins analysis data from PDF reports -----------------------------------------
parameter_names <- c(
  "DS",
  "VEM",
  "VEVI",
  "DVE",
  "OEB",
  "VOS",
  "FOSp",
  "OEB 2 uur",
  "FOSp 2 uur",
  "Structuurwaarde",
  "Verzadigingswrd.",
  "Ruw as",
  "VCOS(%OS)",
  "Ruw eiwit",
  "Oplosbr.ruw eiwit(%RE)",
  "Ruw vet",
  "Ruwe celstof",
  "Suiker",
  "NDF",
  "NDFvert.br.hd(%NDF)",
  "ADF",
  "ADL",
  "Natrium",
  "Kalium",
  "Magnesium",
  "Calcium",
  "Fosfor",
  "Zwavel",
  "Chloor",
  "Kat.AnionVerschil (meq)",
  "Mangaan (mg)",
  "Zink (mg)",
  "IJzer (mg)",
  "Koper (mg)"
)

parcel_name <- "kieftenweg"
sample_datafram <-
  data.frame(matrix(nrow = 23, ncol = length(parameter_names)))

colnames(sample_datafram) <- parameter_names

report_names <-
  list.files(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/eurofins/30042019/reports/"
  )
for (rep_name in report_names) {
  print(rep_name)
  report_indx <- strsplit(rep_name, '_')[[1]][1]
  i <- as.integer(strsplit(report_indx, parcel_name)[[1]][2])
  text <-
    pdf_text(
      paste0(
        "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/eurofins/30042019/reports/",
        rep_name
      )
    )
  report <- strsplit(text[[1]], ' ')
  report_char_list <- lapply(report, function(x)
    x[!x %in% ""])[[1]]
  
  index_of_DS <- match("DS", report_char_list)
  sample_datafram$DS[i] <-
    as.double(report_char_list[index_of_DS + 1])
  
  index_of_Ruw <- match("Ruw", report_char_list)
  sample_datafram$`Ruw as`[i] <-
    as.double(report_char_list[index_of_Ruw + 2])
  
  index_of_VEM <- match("VEM", report_char_list)
  sample_datafram$VEM[i] <-
    as.double(report_char_list[index_of_VEM + 2])
  
  index_of_VCOS <- match("VCOS", report_char_list)
  sample_datafram$`VCOS(%OS)`[i] <-
    as.double(sub(",", ".", report_char_list[index_of_VCOS + 2], fixed = TRUE))
  
  index_of_VEVI <- match("VEVI", report_char_list)
  sample_datafram$VEVI[i] <-
    as.double(report_char_list[index_of_VEVI + 2])
  
  index_of_eiwit <- match("eiwit", report_char_list)
  sample_datafram$`Ruw eiwit`[i] <-
    as.double(report_char_list[index_of_eiwit + 1])
  
  index_of_DVE <- match("DVE", report_char_list)
  sample_datafram$DVE[i] <-
    as.double(sub(",", ".", report_char_list[index_of_DVE + 2], fixed = TRUE))
  
  index_of_Oplosbr <- match("Oplosbr.ruw", report_char_list)
  sample_datafram$`Oplosbr.ruw eiwit(%RE)`[i] <-
    as.double(sub(",", ".", report_char_list[index_of_Oplosbr + 2], fixed = TRUE))
  
  index_of_OEB <- match("OEB", report_char_list)
  sample_datafram$OEB[i] <-
    as.double(sub(",", ".", report_char_list[index_of_OEB + 2], fixed = TRUE))
  
  index_of_vet <- match("vet", report_char_list)
  sample_datafram$`Ruw vet`[i] <-
    as.double(sub(",", ".", report_char_list[index_of_vet + 1], fixed = TRUE))
  
  index_of_VOS <- match("VOS", report_char_list)
  sample_datafram$VOS[i] <-
    as.double(sub(",", ".", report_char_list[index_of_VOS + 2], fixed = TRUE))
  
  index_of_celstof <- match("celstof", report_char_list)
  sample_datafram$`Ruwe celstof`[i] <-
    as.double(sub(",", ".", report_char_list[index_of_celstof + 1], fixed = TRUE))
  
  index_of_FOSp <- match("FOSp", report_char_list)
  sample_datafram$FOSp[i] <-
    as.double(sub(",", ".", report_char_list[index_of_FOSp + 2], fixed = TRUE))
  
  index_of_Suiker <- match("Suiker", report_char_list)
  sample_datafram$Suiker[i] <-
    as.double(sub(",", ".", report_char_list[index_of_Suiker + 1], fixed = TRUE))
  
  index_of_OEB_2uur <- match("uur", report_char_list)
  sample_datafram$`OEB 2 uur`[i] <-
    as.double(sub(",", ".", report_char_list[index_of_OEB_2uur + 2], fixed = TRUE))
  
  index_of_NDF <- match("NDF", report_char_list)
  sample_datafram$NDF[i] <-
    as.double(sub(",", ".", report_char_list[index_of_NDF + 1], fixed = TRUE))
  
  index_of_FOSp_2_uur <- match("NDF" , report_char_list)
  sample_datafram$`FOSp 2 uur`[i] <-
    as.double(sub(",", ".", report_char_list[index_of_FOSp_2_uur + 8], fixed = TRUE))
  
  index_of_NDFvert <-
    match("NDFvert.br.hd(%NDF)" , report_char_list)
  sample_datafram$`NDFvert.br.hd(%NDF)`[i] <-
    as.double(sub(",", ".", report_char_list[index_of_NDFvert + 1], fixed = TRUE))
  
  index_of_Structuurwaarde <-
    match("Structuurwaarde" , report_char_list)
  sample_datafram$Structuurwaarde[i] <-
    as.double(sub(",", ".", report_char_list[index_of_Structuurwaarde + 1], fixed = TRUE))
  
  index_of_ADF <- match("ADF" , report_char_list)
  sample_datafram$ADF[i] <-
    as.double(sub(",", ".", report_char_list[index_of_ADF + 1], fixed = TRUE))
  
  index_of_Verzadigingswrd. <-
    match("Verzadigingswrd." , report_char_list)
  sample_datafram$Verzadigingswrd.[i] <-
    as.double(sub(",", ".", report_char_list[index_of_Verzadigingswrd. + 1], fixed = TRUE))
  
  index_of_ADL <- match("ADL" , report_char_list)
  sample_datafram$ADL[i] <-
    as.double(sub(",", ".", report_char_list[index_of_ADL + 1], fixed = TRUE))
  
  index_of_Natrium <- match("Natrium" , report_char_list)
  sample_datafram$Natrium[i] <-
    as.double(sub(",", ".", report_char_list[index_of_Natrium + 1], fixed = TRUE))
  
  index_of_Kalium <- match("Kalium" , report_char_list)
  sample_datafram$Kalium[i] <-
    as.double(sub(",", ".", report_char_list[index_of_Kalium + 1], fixed = TRUE))
  
  index_of_Magnesium <- match("Magnesium" , report_char_list)
  sample_datafram$Magnesium[i] <-
    as.double(sub(",", ".", report_char_list[index_of_Magnesium + 1], fixed = TRUE))
  
  index_of_Calcium <- match("Calcium" , report_char_list)
  sample_datafram$Calcium[i] <-
    as.double(sub(",", ".", report_char_list[index_of_Calcium + 1], fixed = TRUE))
  
  index_of_Fosfor <- match("Fosfor" , report_char_list)
  sample_datafram$Fosfor[i] <-
    as.double(sub(",", ".", report_char_list[index_of_Fosfor + 1], fixed = TRUE))
  
  index_of_Zwavel <- match("Zwavel" , report_char_list)
  sample_datafram$Zwavel[i] <-
    as.double(sub(",", ".", report_char_list[index_of_Zwavel + 1], fixed = TRUE))
  
  index_of_Chloor <- match("Chloor" , report_char_list)
  sample_datafram$Chloor[i] <-
    as.double(sub(",", ".", report_char_list[index_of_Chloor + 1], fixed = TRUE))
  
  index_of_Kat.AnionVerschil <-
    match("Kat.AnionVerschil" , report_char_list)
  sample_datafram$`Kat.AnionVerschil (meq)`[i] <-
    as.double(sub(",", ".", report_char_list[index_of_Kat.AnionVerschil + 2], fixed = TRUE))
  
  index_of_Mangaan <- match("Mangaan" , report_char_list)
  sample_datafram$`Mangaan (mg)`[i] <-
    as.double(sub(",", ".", report_char_list[index_of_Mangaan + 2], fixed = TRUE))
  
  index_of_Zink <- match("Zink" , report_char_list)
  sample_datafram$`Zink (mg)`[i] <-
    as.double(sub(",", ".", report_char_list[index_of_Zink + 2], fixed = TRUE))
  
  index_of_IJzer <- match("IJzer" , report_char_list)
  sample_datafram$`IJzer (mg)`[i] <-
    as.double(sub(",", ".", report_char_list[index_of_IJzer + 2], fixed = TRUE))
  
  index_of_Koper <- match("Koper" , report_char_list)
  sample_datafram$`Koper (mg)`[i] <-
    as.double(sub(",", ".", report_char_list[index_of_Koper + 2], fixed = TRUE))
}




# Reading EuroFins analysis data from the table ---------------------------
EuroFins_df <-
  read.csv(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/eurofins/30042019/8896739_GemeenteHofvanTwente_7471DK_20190429.csv",
    header = T,
    sep = ";"
  )
names(EuroFins_df)
EuroFins_df <- EuroFins_df[, c(
  "Vocht",
  "DS",
  "VEM",
  "DVE",
  "DVE",
  "OEB",
  "VEVI",
  "VOS",
  "FOS",
  "SW",
  "Ruw.eiwit",
  "Ruwe.celst",
  "Ruw.as",
  "VCOS",
  "Suiker",
  "Ruw.vet",
  "Chloor",
  "NDF",
  "ADF",
  "ADL",
  "C.A.ion",
  "Natrium",
  "Kalium",
  "Magnesium",
  "Calcium",
  "Fosfor",
  "Mangaan",
  "Zink",
  "Koper",
  "IJzer",
  "Zwavel",
  "VEM.prod",
  "DVE.prod",
  "OEB.prod",
  "NDF.vs",
  "CW.vert."
)]

# reading and adding locaiton of Eurofins data ----------------------------------
Eurofins_sample_location <-
  read.csv(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/eurofins/30042019/samples_locations/Kieftenweg_sample_locations.csv",
    header = T,
    sep = ","
  )
Eurofins_sample_location_spdf <-
  SpatialPointsDataFrame(
    data = EuroFins_df[24:46, ],
    coords = Eurofins_sample_location[, 2:3],
    proj4string = CRS('+init=EPSG:4326')
  )
plot(field_boundary)
plot(Eurofins_sample_location_spdf,pch = 20, add = T)

# Reading sequoia products 30042019 --------------------------------------------------------
# preparing NDVI product
NDVI_data <-
  raster(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/process/pix4field/30042019/Export/NDVI.data.tif"
  )
NDVI_data <- projectRaster(NDVI_data, crs = '+init=EPSG:4326')
NDVI_data <- mask(NDVI_data, field_boundary)

tiff(
  paste0(path_to_save_maps, "NDVI", ".tiff"),
  units = "in",
  width = 10,
  height = 10,
  res = 300
)

plot(NDVI_data, main="Normalized Difference Vegetation Index (NDVI)")
plot(Eurofins_sample_location_spdf,pch = 20, col="red", add = T)

dev.off()


#preparing NDRE product
NDRE_data <- raster(
  "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/process/pix4field/30042019/Export/NDRE.data.tif"
)
NDRE_data <- projectRaster(NDRE_data, crs = '+init=EPSG:4326')
NDRE_data <- mask(NDRE_data, field_boundary)

tiff(
  paste0(path_to_save_maps, "NDRE", ".tiff"),
  units = "in",
  width = 10,
  height = 10,
  res = 300
)

plot(NDRE_data, main="Normalized Difference Red Edge Index (NDRE)")
plot(Eurofins_sample_location_spdf,pch = 20, col="red", add = T)

dev.off()


# preparing LCI product
LCI_data <-   raster(
  "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/process/pix4field/30042019/Export/LCI.data.tif"
)
LCI_data <- projectRaster(LCI_data, crs = '+init=EPSG:4326')
LCI_data <- mask(LCI_data, field_boundary)
tiff(
  paste0(path_to_save_maps, "LCI", ".tiff"),
  units = "in",
  width = 10,
  height = 10,
  res = 300
)
plot(LCI_data, main = "Leaf Chlorophyll Index (LCI)")
plot(Eurofins_sample_location_spdf,pch = 20,col="red", add = T)
dev.off()


# preparing RGB product
orthomosaic_RGB <- stack(
  "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/process/pix4field/30042019/Export/Orthomosaic.tif"
)
orthomosaic_RGB <-
  projectRaster(orthomosaic_RGB, crs = '+init=EPSG:4326')
orthomosaic_RGB <- mask(orthomosaic_RGB, field_boundary)

tiff(
  paste0(path_to_save_maps, "RGB", ".tiff"),
  units = "in",
  width = 10,
  height = 10,
  res = 300
)
plotRGB(orthomosaic_RGB, main="RGB mosaic")
plot(Eurofins_sample_location_spdf,pch = 20,col="red", add = T)
dev.off()


# Extracting and adding sequoia indices to ueofins data frame ------------------------
NDVI_values <-
  extract(NDVI_data, Eurofins_sample_location_spdf, buffer = 0.3)
Eurofins_sample_location_spdf@data$NDVI_seq <-
  sapply(NDVI_values, mean)

NDRE_values <-
  extract(NDRE_data, Eurofins_sample_location_spdf, buffer = 0.3)
Eurofins_sample_location_spdf@data$NDRE_seq <-
  sapply(NDRE_values, mean)

LCI_values <-
  extract(LCI_data, Eurofins_sample_location_spdf, buffer = 0.3)
Eurofins_sample_location_spdf@data$LCI_seq <-
  sapply(LCI_values, mean)

# Read hyperslit data tables --------------------------------------------------
georef_table <-
  read.csv(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/spectrometer/30042019/1/GeoRef.csv",
    header = T,
    sep = ","
  )
spectral_table <-
  read.csv(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/spectrometer/30042019/1/VcountRaw.csv",
    header = T,
    sep = ","
  )
waveleght_df <-
  read.csv(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/spectrometer/30042019/1/Wavelengths.csv",
    header = F,
    sep = ","
  )


# creating a spatialpointdataframe out of hyperslit locations -------------
spectrometer_locations <-
  SpatialPointsDataFrame(
    data = georef_table,
    coords = georef_table[, 7:6],
    proj4string = CRS('+init=EPSG:4326')
  )
height_filter_m <- 28
spectrometer_locations_filetered <-
  subset(spectrometer_locations, height > height_filter_m)


tiff(
  paste0(path_to_save_maps, "Hyper_RGB", ".tiff"),
  units = "in",
  width = 10,
  height = 10,
  res = 300
)
plotRGB(orthomosaic_RGB, main="RGB mosaic")
plot(spectrometer_locations_filetered,
     pch = 20,
     add=T,
     col = "black")

dev.off()



# creating char and numeric array of waveleght values ---------------------
waveleght_lables <- as.character(waveleght_df[1,])
waveleght_numeric <- as.numeric(waveleght_lables)


# generating white spectral signature -------------------------------------
row_num_white <- which(spectral_table$Type == 1)
white_spectral <- spectral_table[row_num_white, 6:1029]
white_int_time <- spectral_table[row_num_white, 5]
white_flux <- white_spectral / white_int_time


# Creating dataframe of spectral signatures -----------------------------------
type_index <- which(names(spectral_table) == "Type")
reflectance_df = spectral_table
reflectance_df$long <- NA
reflectance_df$lat <- NA

for (p in 1:nrow(reflectance_df)) {
  print(p)
  spectra_type <-  spectral_table[p, type_index]
  geo_id <- spectral_table[p, 3]
  
  if (spectra_type == 0) {
    spectra_flux <- spectral_table[p, 6:1029] / spectral_table[p, 5]
    reflectance_df[p, 6:1029] <- spectra_flux / white_flux
    reflectance_df$long[p] <-
      mean (georef_table[geo_id, 7], georef_table[geo_id + 1, 7])
    reflectance_df$lat[p] <-
      mean (georef_table[geo_id, 6], georef_table[geo_id + 1, 6])
    
  }
}

# visualising the hyperspectral samples/locations -------------------------
par(mfrow = c(1, 2))

xlim <- c(min(georef_table$longitude), max(georef_table$longitude))
ylim <- c(min(georef_table$latitude), max(georef_table$latitude))

for (p in 1:nrow(reflectance_df)) {
  ration_array <- as.numeric(reflectance_df[p, 6:1029])
  
  plot(
    waveleght_numeric[25:1024],
    ration_array[25:1024],
    type = "l",
    # xaxt = "n",
    xlab = 'Wavelength (nm)',
    ylab = 'Reflectance (ratio to reference)',
    ylim = c(0, 1.5)
  )
  
  grid()
  
  plot(
    reflectance_df$long[p],
    reflectance_df$lat[p],
    add = T,
    xlab = "Lat",
    ylab = "Long",
    xlim = xlim,
    ylim = ylim
  )
  
  plot(field_boundary, add = T)
  
  Sys.sleep(0.1)
}


# Creating a spatial point datafram from reflectance df  ------------------
hyperspectral_location_spdf <-
  SpatialPointsDataFrame(
    data = reflectance_df[4:nrow(reflectance_df),],
    coords = reflectance_df[4:nrow(reflectance_df), 1030:1031],
    proj4string = CRS('+init=EPSG:4326')
  )

# Finding closest hyperspectral data for eurroofins samples locations ----------
num_of_neighbors <- 1
near_neighbours <- nn2(
  hyperspectral_location_spdf@coords[, 1:2],
  Eurofins_sample_location_spdf@coords[, 1:2],
  k = num_of_neighbors,
  searchtype = "radius",
  radius = 0.5
)

# Extracting closest reflectance and adding them to spatial dataframe for analysis/modeling ----------------------------------
for (n in waveleght_lables) {
  newFieldNAme <- paste("nm", n, sep = "")
  Eurofins_sample_location_spdf@data[newFieldNAme] <- NA
  for (y in 1:nrow(Eurofins_sample_location_spdf@data)) {
    clst_point_id <- as.numeric (results[y, 2])
    clst_point_ids <- near_neighbours$nn.idx[y,]
    
    inx <- which(waveleght_lables == n)
    mean_reflect <-
      mean(hyperspectral_location_spdf@data[as.vector(clst_point_ids), 5 + inx])
    Eurofins_sample_location_spdf@data[y, which(names(Eurofins_sample_location_spdf@data) == newFieldNAme)] <-
      round(mean_reflect, 2)
  }
}


# Visualizing the extracted spectral signature at Eurofins sample locations --------
par(mfrow = c(1, 2))

idx_336.3 <-
  which (names(Eurofins_sample_location_spdf@data) == "nm336.3")
idx_823.6 <-
  which (names(Eurofins_sample_location_spdf@data) == "nm823.6")

for (v in 1:23) {
  reflectacne_mean <-
    Eurofins_sample_location_spdf@data[v, idx_336.3:idx_823.6]
  
  plot(
    waveleght_numeric[25:1024],
    as.numeric(reflectacne_mean)[25:1024],
    main = paste0("Sample ", v),
    xlab = 'Wavelength (nm)',
    ylab = 'Reflectance (ratio to reference)',
    ylim = c(0, 1.5),
    type = "l"
  )
  
  
  plot(
    Eurofins_sample_location_spdf@coords[, 1][v],
    Eurofins_sample_location_spdf@coords[, 2][v],
    add = T,
    xlab = "Lat",
    ylab = "Long",
    xlim = xlim,
    ylim = ylim
  )
  
  plot(field_boundary, add = T)
  
}


# Reading veris_scan data -------------------------------------
veris_scan_ph <-
  readShapeSpatial(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/veris_scan/ph.shp"
  )

veris_scan_alt <-
  readShapeSpatial(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/veris_scan/alt.shp"
  )

veris_scan_ecom <-
  readShapeSpatial(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/veris_scan/ecom.shp"
  )

veris_scan_pg <-
  readShapeSpatial(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/veris_scan/pg.shp"
  )

D2I_data <-
  raster(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/Soil_X_plorer_data/dorreweg-19_02_12_15_35_38_D2I.tif"
  )
D2I_data <- mask(D2I_data, field_boundary)
tiff(
  paste0(path_to_save_maps, "D2I", ".tiff"),
  units = "in",
  width = 10,
  height = 10,
  res = 300
)
plot(D2I_data, main="D2I")
plot(Eurofins_sample_location_spdf,pch = 20, col="red", add = T)
dev.off()



ECaR1_data <-
  raster(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/Soil_X_plorer_data/dorreweg-19_02_12_15_35_38_ECaR1.tif"
  )
ECaR1_data <- mask(ECaR1_data, field_boundary)
tiff(
  paste0(path_to_save_maps, "ECaR1_data", ".tiff"),
  units = "in",
  width = 10,
  height = 10,
  res = 300
)
plot(ECaR1_data, main="ECaR1_data")
plot(Eurofins_sample_location_spdf,pch = 20, col="red", add = T)
dev.off()


rWTC_data <-
  raster(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/Soil_X_plorer_data/dorreweg-19_02_12_15_35_38_rWTC.tif"
  )

rWTC_data <- mask(rWTC_data, field_boundary)
tiff(
  paste0(path_to_save_maps, "rWTC_data", ".tiff"),
  units = "in",
  width = 10,
  height = 10,
  res = 300
)
plot(rWTC_data, main="rWTC_data")
plot(Eurofins_sample_location_spdf,pch = 20, col="red", add = T)
dev.off()

Tillage_data <-
  raster(
    "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/data/Soil_X_plorer_data/dorreweg-19_02_12_15_35_38_Tillage.tif"
  )

Tillage_data <- mask(Tillage_data, field_boundary)
tiff(
  paste0(path_to_save_maps, "Tillage_data", ".tiff"),
  units = "in",
  width = 10,
  height = 10,
  res = 300
)
plot(Tillage_data, main="Tillage_data")
plot(Eurofins_sample_location_spdf,pch = 20, col="red", add = T)
dev.off()

# interpolating the veris scan data ---------------------------------------
selected_veris_scan_parm <- veris_scan_ph

dat.pp <-
  as(dirichlet(as.ppp(selected_veris_scan_parm)), "SpatialPolygons")
dat.pp <- as(dat.pp, "SpatialPolygons")

proj4string(dat.pp) <- CRS("+init=EPSG:4326")
proj4string(selected_veris_scan_parm) <- CRS("+init=EPSG:4326")

int.Z <- over(dat.pp, selected_veris_scan_parm, fn = mean)

thiessen <- SpatialPolygonsDataFrame(dat.pp, int.Z)

thiessen.crop <- crop(thiessen, field_boundary)

tm_shape(field_boundary) + tm_fill(alpha = .3, col = "grey") +
  tm_shape(thiessen.crop) +  tm_borders(alpha = .5, col = "black") +
  tm_shape(selected_veris_scan_parm) + tm_dots(col = "blue", scale = 0.5)

tm_shape(thiessen.crop) + tm_fill(
  col = "ph",
  style = "quantile",
  palette = "Reds",
  title = "PH"
) + tm_borders(alpha = .3, col = "black") +
  tm_shape(selected_veris_scan_parm) + tm_dots(col = "black", scale = 0.5) +
  tm_layout(
    legend.position = c("left", "bottom"),
    legend.text.size = 1.05,
    legend.title.size = 1.2,
    frame = FALSE
  )

grid <- spsample(field_boundary, type = 'regular', n = 10000)
idw <- idw(selected_veris_scan_parm@data[, 1] ~ 1,
           selected_veris_scan_parm,
           newdata = grid)

idw.output = as.data.frame(idw)
names(idw.output)[1:3] <- c("long", "lat", "prediction")
spg <- idw.output
coordinates(spg) <- ~ long + lat
gridded(spg) <- TRUE
raster_idw <- raster(spg)
projection(raster_idw) <- CRS("+init=EPSG:4326")


tiff(
  paste0(path_to_save_maps, "PH", ".tiff"),
  units = "in",
  width = 10,
  height = 10,
  res = 300
)

plot(raster_idw, main = "PH", col=colorRampPalette(c("blue","red"))(255))
plot(field_boundary, add = T)

dev.off()

tm_shape(raster_idw) + tm_raster(
  "prediction",
  style = "quantile",
  n = 100,
  palette = "Reds",
  legend.show = FALSE
) +
  tm_shape(field_boundary) + tm_borders(alpha = .5)


tm_shape(raster_idw) + tm_raster(
  "prediction",
  style = "quantile",
  n = 100,
  palette = "Blues",
  legend.show = FALSE
) +
  tm_shape(field_boundary) + tm_borders(alpha = .5,) +
  tm_shape(selected_veris_scan_parm) + tm_bubbles(
    size = "ph",
    col = "ph",
    palette = "Blues",
    style = "quantile",
    legend.size.show = FALSE,
    title.col = "PH"
  ) +
  tm_layout(
    legend.position = c("left", "bottom"),
    legend.text.size = 1.1,
    legend.title.size = 1.4,
    frame = FALSE,
    legend.bg.color = "white",
    legend.bg.alpha = 0.5
  )


masked_idw <- mask(raster_idw, field_boundary)

tm_shape(masked_idw) + tm_raster("prediction",
                                 style = "quantile",
                                 n = 100,
                                 legend.show = FALSE) +
  tm_shape(selected_veris_scan_parm) + tm_bubbles(
    size = "ph",
    col = "ph",
    palette = "Blues",
    style = "quantile",
    legend.size.show = FALSE,
    title.col = "Price Paid (£)"
  ) +
  tm_layout(
    legend.position = c("left", "bottom"),
    legend.text.size = 1.1,
    legend.title.size = 1.4,
    frame = FALSE
  )




# extracting soil data ----------------------------------------------------
near_neighbours_ph <- nn2(
  veris_scan_ph@coords[, 1:2],
  Eurofins_sample_location_spdf@coords[, 1:2],
  k = num_of_neighbors,
  searchtype = "radius",
  radius = 0.5
)
ph_point_ids <- near_neighbours_ph$nn.idx
Eurofins_sample_location_spdf@data$ph <-
  veris_scan_ph@data[ph_point_ids,]

near_neighbours_alt <- nn2(
  veris_scan_alt@coords[, 1:2],
  Eurofins_sample_location_spdf@coords[, 1:2],
  k = num_of_neighbors,
  searchtype = "radius",
  radius = 0.5
)
alt_point_ids <- near_neighbours_alt$nn.idx
Eurofins_sample_location_spdf@data$alt <-
  veris_scan_alt@data[alt_point_ids, 1]

near_neighbours_ecom <- nn2(
  veris_scan_ecom@coords[, 1:2],
  Eurofins_sample_location_spdf@coords[, 1:2],
  k = num_of_neighbors,
  searchtype = "radius",
  radius = 0.5
)
ecom_point_ids <- near_neighbours_ecom$nn.idx
Eurofins_sample_location_spdf@data$ecom <-
  veris_scan_ecom@data[ecom_point_ids, 1]

D2I_values <-
  extract(D2I_data, Eurofins_sample_location_spdf, buffer = 0.3)
Eurofins_sample_location_spdf@data$D2I <- sapply(D2I_values, mean)


ECaR1_values <-
  extract(ECaR1_data, Eurofins_sample_location_spdf, buffer = 0.3)
Eurofins_sample_location_spdf@data$ECaR1 <-
  sapply(ECaR1_values, mean)

rWTC_values <-
  extract(rWTC_data, Eurofins_sample_location_spdf, buffer = 0.3)
Eurofins_sample_location_spdf@data$rWTC <- sapply(rWTC_values, mean)

Tillage_values <-
  extract(Tillage_data, Eurofins_sample_location_spdf, buffer = 0.3)
Eurofins_sample_location_spdf@data$Tillage <-
  sapply(Tillage_values, mean)

# calculating correlation waveleght vs measured parameters by Eurofis ----------
Eurofins_sample_location_spdf@data$SW <-
  as.numeric(Eurofins_sample_location_spdf@data$SW)
Eurofins_sample_location_spdf@data$VCOS <-
  as.numeric(Eurofins_sample_location_spdf@data$VCOS)
Eurofins_sample_location_spdf@data$Chloor <-
  as.numeric(Eurofins_sample_location_spdf@data$Chloor)
Eurofins_sample_location_spdf@data$Kalium <-
  as.numeric(Eurofins_sample_location_spdf@data$Kalium)
Eurofins_sample_location_spdf@data$Magnesium <-
  as.numeric(Eurofins_sample_location_spdf@data$Magnesium)
Eurofins_sample_location_spdf@data$Calcium <-
  as.numeric(Eurofins_sample_location_spdf@data$Calcium)
Eurofins_sample_location_spdf@data$Fosfor <-
  as.numeric(Eurofins_sample_location_spdf@data$Fosfor)
Eurofins_sample_location_spdf@data$Koper <-
  as.numeric(Eurofins_sample_location_spdf@data$Koper)
Eurofins_sample_location_spdf@data$Zwavel <-
  as.numeric(Eurofins_sample_location_spdf@data$Zwavel)
Eurofins_sample_location_spdf@data$CW.vert. <-
  as.numeric(Eurofins_sample_location_spdf@data$CW.vert.)
Eurofins_sample_location_spdf@data$Natrium <-
  as.numeric(Eurofins_sample_location_spdf@data$Natrium)

Correlation_table <-
  round(cor(Eurofins_sample_location_spdf@data), 3)
Correlation_df <- as.data.frame(Correlation_table)

# visualizing correlation between parameter value and DS ------------------
idx_336.3 <- which (names(Correlation_df) == "nm336.3")
idx_823.6 <- which (names(Correlation_df) == "nm823.6")
path_to_save <-
  "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/process/Hamed/figs/corelations/"
for (par_index in 1:(idx_336.3 - 2)) {
  cor_for_plot <- Correlation_df[par_index, idx_336.3:idx_823.6]
  max_cor_value <- max(abs(cor_for_plot), na.rm = T)
  indx_max_cor <- which(abs(cor_for_plot) == max_cor_value)
  
  tiff(
    paste0(path_to_save, par_index, "_cor.tiff"),
    units = "in",
    width = 10,
    height = 10,
    res = 300
  )
  
  plot(
    waveleght_numeric,
    cor_for_plot,
    type = "l",
    xlab = "Wavelength (nm)",
    ylab = "correlation coefficient",
    main = colnames(Correlation_df)[par_index],
    ylim = c(-1, 1)
  )
  grid()
  
  legend("topright", c(
    paste0("highest correlation", cor_for_plot[indx_max_cor]),
    paste0("wavelength: ", waveleght_numeric[indx_max_cor], " nm")
  ),
  cex = 1.5)
  
  dev.off()
}

# train a SVM model -------------------------------------------------------
target_par_data <- Eurofins_sample_location_spdf@data$Ruw.as
save_map_name <- "Ruw.as"

idx_336.3 <-
  which (names(Eurofins_sample_location_spdf@data) == "nm336.3")
idx_823.6 <-
  which (names(Eurofins_sample_location_spdf@data) == "nm823.6")

predictors_df_spectral_part <-
  Eurofins_sample_location_spdf@data[, idx_336.3:idx_823.6]

predictors_df_extera <-
  Eurofins_sample_location_spdf@data[, c(
    "NDVI_seq",
    "NDRE_seq",
    "LCI_seq",
    "ph",
    "alt",
    "ecom",
    "D2I",
    "ECaR1",
    "rWTC",
    "Tillage"
  )]
predictors_df <-
  cbind(predictors_df_spectral_part, predictors_df_extera)

model_svm <- svm(
  x = predictors_df,
  y = target_par_data,
  type = "eps-regression",
  kernel = "linear"
)


plot(
  model_svm$fitted,
  target_par_data,
  ylim = c(
    min(model_svm$fitted, target_par_data),
    max(model_svm$fitted, target_par_data)
  ),
  xlim = c(
    min(model_svm$fitted, target_par_data),
    max(model_svm$fitted, target_par_data)
  ),
  
  ylab = "Eurofins",
  xlab = "Estimated by model",
  main = save_map_name
  
)
abline(1, 1)
rmse(model_svm$fitted, target_par_data)


# radius = 0.05
# buffer = 0.05
# prediction_grid <- spsample(field_boundary, type = 'regular', n = 10000)
# final_predicted_df <- data.frame(prediction_grid@coords)
# 
# NDVI_values_grid<- extract(NDVI_data,prediction_grid, buffer=buffer)
# ndvi_vec <- sapply(NDVI_values_grid,mean)
# 
# NDRE_values_grid<- extract(NDRE_data,prediction_grid, buffer=buffer)
# ndre_vec <- sapply(NDRE_values_grid,mean)
# 
# LCI_values_grid<- extract(LCI_data,prediction_grid, buffer=buffer)
# lci_vec <- sapply(LCI_values_grid,mean)
# 
# D2I_values_grid<- extract(D2I_data,prediction_grid, buffer=buffer)
# D2I_vec <- sapply(D2I_values_grid,mean)
# 
# ECaR1_values_grid<- extract(ECaR1_data,prediction_grid, buffer=buffer)
# ECaR1_vec <- sapply(ECaR1_values_grid,mean)
# 
# rWTC_values_grid<- extract(rWTC_data,prediction_grid, buffer=buffer)
# rWTC_vec <- sapply(rWTC_values_grid,mean)
# 
# Tillage_values_grid<- extract(Tillage_data,prediction_grid, buffer=buffer)
# Tillage_vec <- sapply(Tillage_values_grid,mean)
# 
# num_of_neighbors <- 1
# near_neighbours <- nn2(hyperspectral_location_spdf@coords[160:nrow(hyperspectral_location_spdf@coords),1:2],
#                        prediction_grid@coords[,1:2],
#                        k = num_of_neighbors,
#                        searchtype = "radius",
#                        radius = radius)
# 
# near_neighbours_ph <- nn2(veris_scan_ph@coords[,1:2],
#                           prediction_grid@coords[,1:2],
#                           k = num_of_neighbors,
#                           searchtype = "radius",
#                           radius = radius)
# 
# near_neighbours_alt <- nn2(veris_scan_alt@coords[,1:2],
#                            prediction_grid@coords[,1:2],
#                            k = num_of_neighbors,
#                            searchtype = "radius",
#                            radius = radius)
# 
# near_neighbours_ecom <- nn2(veris_scan_ecom@coords[,1:2],
#                             prediction_grid@coords[,1:2],
#                             k = num_of_neighbors,
#                             searchtype = "radius",
#                             radius = radius)


final_predicted_df$predicted_val <- NA
for (y in 1:nrow(prediction_grid@coords)) {
  print(y)
  
  clst_point_ids <- near_neighbours$nn.idx[y,]
  input_reflect <-
    hyperspectral_location_spdf@data[clst_point_ids, 6:(6 + 1023)]
  
  ph_point_ids <- near_neighbours_ph$nn.idx[y,]
  input_ph <- veris_scan_ph@data[ph_point_ids,]
  
  alt_point_ids <- near_neighbours_alt$nn.idx[y,]
  input_alt <- veris_scan_alt@data[alt_point_ids,][1]
  
  ecom_point_ids <- near_neighbours_ecom$nn.idx[y,]
  input_ecom <- veris_scan_ecom@data[ecom_point_ids,][1]
  
  NDVI_val <- ndvi_vec[y]
  ndre_val <- ndre_vec[y]
  lci_val <- lci_vec[y]
  D2I_val <- D2I_vec[y]
  ECaR1_val <- ECaR1_vec[y]
  rWTC_val <- rWTC_vec[y]
  Tillage_val <- Tillage_vec[y]
  
  
  if (is.na(NDVI_val) ) {
    NDVI_val <- 0
  }
  
  if (is.na(ndre_val) ) {
    ndre_val <- 0
  }
  
  if (is.na(lci_val) ) {
    lci_val <- 0
  }
  
  if (is.na(D2I_val) ) {
    D2I_val <- 0
  }
  
  if (is.na(ECaR1_val) ) {
    ECaR1_val <- 0
  }
  
  if (is.na(rWTC_val) ) {
    rWTC_val <- 0
  }
  
  if (is.na(Tillage_val) ) {
    Tillage_val <- 0
  }
  
  
  input_data <-
    cbind(
      input_reflect,
      NDVI_val,
      ndre_val,
      lci_val,
      input_ph,
      input_alt,
      input_ecom,
      D2I_val,
      ECaR1_val,
      rWTC_val,
      Tillage_val
    )
  
  
  final_predicted_df$predicted_val[y] <-
    predict(model_svm, input_data)
}






prediction_spdf <-
  SpatialPointsDataFrame(
    data = data.frame(final_predicted_df$predicted_val),
    coords = final_predicted_df[, 1:2],
    proj4string = CRS('+init=EPSG:4326')
  )
idw_prediction <- idw(prediction_spdf@data[, 1] ~ 1,
                      prediction_spdf,
                      newdata = prediction_grid,
                      idp = 1)

idw_prediction.output = as.data.frame(idw_prediction)
names(idw_prediction.output)[1:3] <- c("long", "lat", "prediction")
spg <- idw_prediction.output
coordinates(spg) <- ~ long + lat
gridded(spg) <- TRUE
raster_idw_wave <- raster(spg)
projection(raster_idw_wave) <- CRS("+init=EPSG:4326")
plot(raster_idw_wave, main = save_map_name)

path_to_save_maps <-
  "D:/Dropbox/UT/ITC/mineral_valley_project_pilot/Results/Kieftenweg/process/Hamed/figs/maps/"
tiff(
  paste0(path_to_save_maps, save_map_name, ".tiff"),
  units = "in",
  width = 10,
  height = 10,
  res = 300
)
plot(raster_idw_wave, main = save_map_name)
dev.off()
writeRaster(
  raster_idw_wave,
  paste0(path_to_save_maps, save_map_name, "_georef", ".tiff"),
  options = c('TFW=YES'),
  overwrite=TRUE
)


# generating reflectance rasters ------------------------------------------
reflectance_df_filtered <-
  reflectance_df[complete.cases(reflectance_df), ]

hyperspectral_single_wave <-
  SpatialPointsDataFrame(
    data = data.frame(reflectance_df_filtered[, 1000]),
    coords = reflectance_df_filtered[, 1030:1031],
    proj4string = CRS('+init=EPSG:4326')
  )


grid_wave <- spsample(field_boundary, type = 'regular', n = 10000)
idw_wave <- idw(
  hyperspectral_single_wave@data[, 1] ~ 1,
  hyperspectral_single_wave,
  newdata = grid_wave,
  idp = 5
)

idw_wave.output = as.data.frame(idw_wave)
names(idw_wave.output)[1:3] <- c("long", "lat", "prediction")
spg <- idw_wave.output
coordinates(spg) <- ~ long + lat
gridded(spg) <- TRUE
raster_idw_wave <- raster(spg)
projection(raster_idw_wave) <- CRS("+init=EPSG:4326")
plot(raster_idw_wave)
