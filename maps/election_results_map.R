source("maps/setup.R")

election2025 = election2025[order(election2025$ID),]
parties = c( "#00F7FF", "blue", "#29D462", "red", "orange")
parties_pal = colorFactor(palette = parties, 
                          domain = election2025$winning_party)
election_results = leaflet() %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(fillColor = ~parties_pal(election2025$winning_party), 
              color = "black",
              data = boundaries2023, weight = 0.5,
              label = ~paste(election2025$`Electoral District`, ":",
                             election2025$winning_party, " won with ", as.character(election2025$winning_vote_share), "% of the vote") 
  )
saveWidget(election_results, file = "maps/election_results.html")