# file id information -----------------------------------------------------
# @author: josh weinflash
# @created: 2017-08-22
# @purpose: script to construct leaflet of weighted median price per county

# NOTE: this analysis is a lot easier to do with the `sf` package. Since
#       it's not available in the Kaggle environment we have to use `sp`
#       instead.

# load libraries ----------------------------------------------------------
library("leaflet")
library("data.table")

# load rent data ----------------------------------------------------------
l_cols = readr::cols_only(State_Name = "c",
                          County     = "c",
                          Median     = "i",
                          Samples    = "i")

df_rent = readr::read_csv("../raw-data/raw-rent.csv", col_types = l_cols)

# convert invalid UTF-8 characters that seem to be present ----------------
df_rent$County = iconv(df_rent$County, "UTF-8", "UTF-8", sub = "")

# load county geometry ----------------------------------------------------
m_geom = maps::map("county", fill = TRUE, plot = FALSE)

# build ID field in rent data to match geometry ---------------------------
df_rent = as.data.table(df_rent)

df_rent[, ID := sprintf("%s,%s", tolower(State_Name), 
                        tolower(stringr::str_trim(stringr::str_replace(County, "County", ""))))]

# calculate weighted median rent per county -------------------------------
df_rent = df_rent[Samples != 0, .(State_Name = State_Name[1],
                                  County = County[1],
                                  w_median = (1/sum(Samples))*sum(Samples*Median)),
                  by = "ID"]

# match rent and geometry data --------------------------------------------
df_rent = dplyr::left_join(tibble::tibble(ID = m_geom$names), df_rent, 
                           by = "ID")

# wrap in sp data structure -----------------------------------------------
sp_rent = sp::SpatialPolygonsDataFrame(maptools::map2SpatialPolygons(m_geom, IDs = m_geom$names),
                                       data = as.data.frame(df_rent), match.ID = "ID")

# build color palette for leaflet map -------------------------------------
f_pale = colorQuantile("RdYlBu", domain = sp_rent@data$w_median, 
                       n = 10, reverse = TRUE)

# build labels ------------------------------------------------------------
v_labs = sprintf(stringr::str_c("<strong>County:</strong> %s<br>",
                                "<strong>State:</strong> %s<br>",
                                "<strong>Weighted median rent:</strong> $%s",
                                collapse = ""),
                 sp_rent@data$County, sp_rent@data$State_Name, 
                 formatC(sp_rent@data$w_median, format = "f", digits = 0, big.mark = ","))

v_labs = purrr::map(v_labs, htmltools::HTML)

# set options -------------------------------------------------------------
l_hl_options = highlightOptions(weight = 5, color = "#666", dashArray = "",
                                fillOpacity = 0.4, bringToFront = TRUE)

l_lb_options = labelOptions(style = list("font-weight" = "normal"), 
                            textsize = "12px")

# build map ---------------------------------------------------------------
m = leaflet(sp_rent, width = "100%")
m = addTiles(m)

m = addPolygons(m,
                fillColor = ~f_pale(w_median),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.4,
                highlight = l_hl_options,
                label = v_labs,
                labelOptions = l_lb_options)

m = addLegend(m, 
              position = "bottomright",
              pal = f_pale,
              values = sp_rent@data$w_median, 
              title = "Rent percentile")

# save to file ------------------------------------------------------------
htmlwidgets::saveWidget(m, "../output/leaflet.html")
