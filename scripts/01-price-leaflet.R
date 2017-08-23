# file id information -----------------------------------------------------
# @author: josh weinflash
# @created: 2017-08-22
# @purpose: script to construct leaflet of weighted median price per county

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
sf_geom = sf::st_as_sf(maps::map("county", fill = TRUE, plot = FALSE))

# calculate weighted median rent per county, and build ID field -----------
df_rent = as.data.table(df_rent)

df_rent = df_rent[, .(w_median = sum((Samples/sum(Samples))*Median)), 
                  by = c("County", "State_Name")]

df_rent[, ID := sprintf("%s,%s", tolower(State_Name), 
                        tolower(stringr::str_trim(stringr::str_replace(County, "County", ""))))]

# join rent and geometry data ---------------------------------------------
sf_geom = dplyr::left_join(sf_geom, df_rent, by = "ID")

# build color palette for leaflet map -------------------------------------
f_pale = colorQuantile("RdYlBu", domain = sf_geom$w_median, 
                       n = 10, reverse = TRUE)

# build labels ------------------------------------------------------------
v_labs = sprintf(stringr::str_c("<strong>County:</strong> %s<br>",
                                "<strong>State:</strong> %s<br>",
                                "<strong>Median rent:</strong> $%s",
                                collapse = ""),
                 sf_geom$County, sf_geom$State_Name, 
                 formatC(sf_geom$w_median, format = "f", digits = 0, big.mark = ","))

v_labs = purrr::map(v_labs, htmltools::HTML)

# set options -------------------------------------------------------------
l_hl_options = highlightOptions(weight = 5, color = "#666", dashArray = "",
                                fillOpacity = 0.4, bringToFront = TRUE)

l_lb_options = labelOptions(style = list("font-weight" = "normal"), 
                            textsize = "12px")

# build map ---------------------------------------------------------------
m = leaflet(sf_geom)
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
              values = sf_geom$w_median, title = "Price percentile")

# save to file ------------------------------------------------------------
htmlwidgets::saveWidget(m, "../output/leaflet.html")
