classification_raster <- matrix(c(0, 49, 1, # agriculture
                                  49, 99, 2, # forest
                                  99, 119, 3, # grassland
                                  119, 129, 4, # shrubland
                                  129, 149, 3, # grassland
                                  149, 159, 5, # sparse_vegetation
                                  159, 179, 2, # forest
                                  179, 189, 6, # wetland
                                  189, 199, 7, # urban
                                  199, 209, 8, # bare areas
                                  209, 255, 9),
                                ncol = 3, byrow = TRUE)
