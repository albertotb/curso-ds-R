library(jsonlite)
library(xml2)


cadena <- iconv(
            readLines("./data/Bicimad_Stations_201906.json"), 
            from = "latin1", 
            to = "UTF8")

data <- fromJSON(cadena)


aloj <- read_xml("./data/alojamientos_v1_es.xml")

services <- xml_find_all(aloj, ".//service")

res <- list()
for (i in seq_along(services)) {
  node <- services[i]
  res[[i]] <- list(id = xml_attr(node, "id"),
                   name = xml_text(xml_find_all(node, ".//name")),
                   cp = xml_text(xml_find_all(node, ".//zipcode")))
}

df <- bind_rows(res)
