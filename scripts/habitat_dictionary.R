
agriculture = c("agricultural", "agriculture", "cocao_grove", "crop_field", "cultivated_areas", "cultivated_land", "cultivation", "cultivations",
                "distal_cultivations", "distal_cultivation", "farmbush", "farmland", "farms", "fields", "immature_cocoa", "mature_cocoa",
                "millet_field", "nearby_cultivations", "new_farm", "old_food_farm", "orchard", "palm_field", "palm_plantation", "pb", "peanut_field",
                "plantations", "proximal_cultivations", "rice_fields", "remote_cultivations", "remote_orchard", "rice_field", "orchards", "university_farm")
names(agriculture) <- rep("agriculture", length(agriculture))
airport = c("airport")
names(airport) <- rep("airport", length(airport))
alluvial_plain = c("alluvial_plain", "floodplains")
names(alluvial_plain) <- rep("alluvial_plain", length(alluvial_plain))
buildings = c("house", "houses", "indoor", "rural_houses", "urban_houses", "urban_buildings", "university_buildings", "village_houses", "warehouse")
names(buildings) <- rep("buildings", length(buildings))
buildings_periphery = c("around_buildings", "buildings", "building_periphery", "coach_station", "coach station", "henhouse", "industrial", "industrial_site", "market", "marketplaces", "markets",
                        "surroundings_well", "university_campus", "work_camp", "peri-domestic")
names(buildings_periphery) <- rep("buildings_periphery", length(buildings_periphery))
botanical_garden = c("botanical_garden")
names(botanical_garden) <- rep("botanical_garden", length(botanical_garden))
burned = c("burnt_habitat")
names(burned) <- rep("burned", length(burned))
clay_soil = c("clay_soil")
names(clay_soil) <- rep("clay_soil", length(clay_soil))
nature_reserve = c("biosphere_reserve", "national_park")
names(nature_reserve) <- rep("nature_reserve", length(nature_reserve))
natural_habitat = c("natural_habitat", "natural_habitats", "never_flooded_habitats", "outdoor", "plateau", "plateaus", "stony_ground", "waste_land")
names(natural_habitat) <- rep("natural_habitat", length(natural_habitat))
fallow = c("fallow", "fallow_field", "fallow_land")
names(fallow) <- rep("fallow", length(fallow))
forest = c("dense_forest", "dry_forest", "dryland_forest", "forest", "forest_edge", "high_forest", "intact_forest", "naturally_regenerated_forest", "nearby_forest",
           "primary_forest", "ravine", "ravine_forest", "rehabilitated_forest", "sacred_grove", "secondary_forest", "swamp_forest", "swampy_forest",
           "woodland", "flooded_swamp_forest", "open_forest", "secondary_rainforest", "natural_clearings", "primary_rainforest")
names(forest) <- rep("forest", length(forest))
degraded_forest = c("degraded_forest", "disturbed_forest", "forest_mining", "forest_patch", "rural_sylvatic", "teak_plantation")
names(degraded_forest) <- rep("degraded_forest", length(degraded_forest))
gardens = c("gardens")
names(gardens) <- rep("gardens", length(gardens))
island = c("island")
names(island) <- rep("island", length(island))
lacustrial = c("lacustrian")
names(lacustrial) <- rep("lacustrial", length(lacustrial))
lateritic_soil = c("mesic_lateritic")
names(lateritic_soil) <- rep("lateritic_soil", length(lateritic_soil))
marsh = c("marsh", "swamp")
names(marsh) <- rep("marsh", length(marsh))
multiple = c("mosaic")
names(multiple) <- rep("multiple", length(multiple))
oasis = c("oasis")
names(oasis) <- rep("oasis", length(oasis))
raphial = c("raphial")
names(raphial) <- rep("raphial", length(raphial))
sand_dunes = c("sandy, sandy_beach", "sandy_dune", "sandy_plain", "sandy_soil", "sandy_valley", "xeric", "coastal_dunes", "dunes", "sandy", "sandy_beach")
names(sand_dunes) <- rep("sand_dunes", length(sand_dunes))
savannah = c("savanna", "savannah", "sudan_savannah")
names(savannah) <- rep("savannah", length(savannah))
unburned = c("unburnt_habitat")
names(unburned) <- rep("unburned", length(unburned))
urban = c("urban", "urban_area", "urban_centre", "landfill_site")
names(urban) <- rep("urban", length(urban))
peri_urban = c("peri_urban", "peri_urban_agriculture", "peripheral_quarters", "periphery", "town_surroundings", "peri-urban", "suburbia")
names(peri_urban) <- rep("peri_urban", length(peri_urban))
vegetation = c("bush", "bushy_vegetation", "elephant_grass_field", "grassland", "herbaceous", "hill_slopes", "hill_tops", "low_lying", "scrub", "secondary_bush",
               "shrubland", "shrubs", "tall_grass", "altered_bush", "thicket", "grasses")
names(vegetation) <- rep("vegetation", length(vegetation))
village = c("nomad_settlement", "peri_domestic", "refugee_dwelling", "residential_area", "rural", "village", "village_outskirts", "village_surrounding",
            "village_surroundings", "villages")
names(village) <- rep("village", length(village))
waterway = c("creek", "creeks", "dry_riverbed", "dry_wadi", "dry_waterhole", "irrigation_canals", "lake_shore", "river", "river_bank", "river_crossing",
             "river_shore", "riverine", "riverside", "seasonal_tributary", "water_edge")
names(waterway) <- rep("waterway", length(waterway))
habitat_dictionary <- c(agriculture, airport, alluvial_plain, botanical_garden, buildings, buildings_periphery,
                        burned, clay_soil, degraded_forest, fallow, forest, gardens, island,
                        lacustrial, lateritic_soil, marsh, multiple, natural_habitat, nature_reserve, oasis,
                        peri_urban, raphial, sand_dunes, savannah, unburned, urban, vegetation, village, waterway) %>%
  setNames(names(.), .)

write_rds(habitat_dictionary, here("data_clean", "habitat_dictionary.rds"))
