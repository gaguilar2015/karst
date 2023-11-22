library(sf)

CURRENT_IPP <- "200.32.244.19"

#CURRENT_IPP <- '192.168.0.50'
conn <- dbConnect(
  drv = RPostgreSQL::PostgreSQL(),
  host = CURRENT_IPP,
  dbname = "census_2023",
  user = "postgres",
  password = "c3n5u5@postgres2022"
)


#geo_buildings <- st_read(conn, "building_2023")

geo_buildings <- st_read(conn, query = "select interview__key, ed_2020, ed_2023, cluster, blk_newn_2023, bldg_number, bldg_newn, ctv, alternate_ctv, ctv_code, shape from building_2023")
geo_bnames <- st_read(conn, query = "select * from building_2023 limit 10")
dbDisconnect(conn) 

geo_centers <- geo_buildings |> 
  filter(
    interview__key %in% ksample$interview_key
  ) |> 
  select(interview__key, shape) |> 
  mutate(
    center_point_x = st_coordinates(st_centroid(st_transform(shape, "EPSG:4326")))[,'X'],
    center_point_y = st_coordinates(st_centroid(st_transform(shape, "EPSG:4326")))[,'Y']
  ) |> 
  st_drop_geometry() |> 
  as.data.frame() |> 
  rename(
    GPS_Building_Navig__Longitude = center_point_x,
    GPS_Building_Navig__Latitude = center_point_y
  )

csample <- readr::read_delim('./blackman_ontario.txt', delim = '\t') |> 
  select(-c(hh_gps__Longitude, hh_gps__Latitude)) |> 
  left_join(
    geo_centers,
    by = c('interview' = 'interview__key')
  )

write.table(csample, './blackman_ontario_new.txt', sep = '\t', quote = F)

ksample_new <- ksample |> 
  select(-c(GPS_Building_Navig__Longitude, GPS_Building_Navig__Latitude))  |> 
  left_join(
    geo_centers,
    by = c('interview_key' = 'interview__key')
  )

writexl::write_xlsx(ksample_new, './ksample2.xlsx')
