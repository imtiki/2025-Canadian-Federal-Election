source("maps/setup.R")


lpc_pal = colorFactor(palette = "Reds", 
                          domain = lpc2025$`LPC Vote Share`)
cpc_pal = colorFactor(palette = "Blues", 
                      domain = cpc2025$`CPC Vote Share`)
ndp_pal = colorFactor(palette = "Oranges", 
                      domain = cpc2025$`NDP Vote Share`)
election_results = leaflet() %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(fillColor = ~cpc_pal(cpc2025$`CPC Vote Share`), 
              color = "black",
              data = boundaries2023, weight = 0.5,
              group = "CPC",
              label = ~paste(cpc2025$`Electoral District`, ":",
                             "CPC", " got ", as.character(cpc2025$`CPC Vote Share`), "% of the vote") 
  ) %>%
  addPolygons(fillColor = ~lpc_pal(lpc2025$`LPC Vote Share`), 
              color = "black",
              data = boundaries2023, weight = 0.5,
              group = "LPC",
              label = ~paste(lpc2025$`Electoral District`, ":",
                           "LPC", " got ", as.character(lpc2025$`LPC Vote Share`), "% of the vote") 
  ) %>%
  addPolygons(fillColor = ~ndp_pal(lpc2025$`NDP Vote Share`), 
              color = "black",
              data = boundaries2023, weight = 0.5,
              group = "NDP",
              label = ~paste(lpc2025$`Electoral District`, ":",
                           "NDP", " got ", as.character(ndp2025$`NDP Vote Share`), "% of the vote") 
  ) %>%
  addLayersControl(
    baseGroups = c("CPC", "LPC", "NDP"),
    options = layersControlOptions(collapsed = FALSE)
  )

saveWidget(election_results, file = "maps/results.html")