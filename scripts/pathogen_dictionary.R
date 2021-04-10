anaplasma <- c("anaplasma")
names(anaplasma) <- "anaplasma_species"
arenaviridae <- c("arenaviridae_spp", "arenavirus", "arenavirus_spp", "arenaviruses")
names(arenaviridae) <- rep("arenaviridae_species", length(arenaviridae))
babesia <- c("babesia_spp")
names(babesia) <- "babesia_species"
bartonella <- c("bartonella", "bartonella_spp")
names(bartonella) <- rep("bartonella_species", length(bartonella))
borrelia <- c("borrelia", "borrelia_crocidurae", "borrelia_spp", "borrellia_spp")
names(borrelia) <- rep("borrelia", length(borrelia))
coxiella <- c("coxiella_burnetii")
names(coxiella) <- "coxiella_burnetii"
e_coli <- c("e_coli_esbl")
names(e_coli) <- "e_coli_esbl"
ehrlichia <- c("ehrlichia")
names(ehrlichia) <- "ehrlichia_species"
eimeria <- c("eimeria_spp")
names(eimeria) <- "eimeria_species"
flavivirus <- c("flavivirus")
names(flavivirus) <- "flavivirus_species"
hantavirus <- c("hantavirus")
names(hantavirus) <- "hantavirus_species"
hydatid <- "hydatigera_species"
names(hydatid) <- "hydatigera_species"
k_pneumoniae <- "k_pneumoniae_esbl"
names(k_pneumoniae) <- "k_pneumoniae_esbl"
lassa <- c("lassa", "lassa mammarenavirus", "lassa_mammarenavirus", "lassa_mammarenavirus_Ag", "lassa_mammarenavirus_antibody", "lassa_mammarenavirus_antigen",
           "lassa_mammarenavirus_IgG_Ab")
names(lassa) <- rep("lassa_mammarenavirus", length(lassa))
leishmania <- c("leishmania_major", "leishmania_spp")
names(leishmania) <- rep("leishmania_species", length(leishmania))
leptospirosis <- "leptospirosis_spp"
names(leptospirosis) <- "leptospirosis_species"
mammarenavirus <- "mammarenavirus"
names(mammarenavirus) <- "mammarenavirus_species"
mycobacteria <- c("mycobacteria_spp", "mycobacterium_spp")
names(mycobacteria) <- "mycobacterium_species"
mycoplasma <- "mycoplasma_spp"
names(mycoplasma) <- "mycoplasma_species"
orentia <- "orentia"
names(orentia) <- "orentia_species"
orthopoxvirus <- c("orthopoxvirus", "orthopoxvirus_spp")
names(orthopoxvirus) <- "orthopoxvirus_species"
phlebovirus <- "phleboviruses"
names(phlebovirus) <- "phlebovirus_species"
plagiorchis <- "plagiorchis_species"
names(plagiorchis) <- "plagiorchis_species"
plasmodium <- "plasmodium_spp"
names(plasmodium) <- "plasmodium_species"
rickettsia <- "rickettsia"
names(rickettsia) <- "rickettsia_species"
rvf <- "rift_valley_fever"
names(rvf) <- "rift_valley_fever_virus"
schistosoma <- c("schistosoma_mansoni", "schistosoma_spp")
names(schistosoma) <- rep("schistosoma_species", length(schistosoma))
strongyloides <- c("strongyloides_spp")
names(strongyloides) <- "strongyloides_species"
toxoplasma <- "toxoplasma_gondii"
names(toxoplasma) <- "toxoplasma_gondii"
trichuris <- "trichuris_spp"
names(trichuris) <- "trichuris_species"
trypanosoma <- c("trypanasoma_lewisi", "trypanosoma_spp")
names(trypanosoma) <- rep("trypanosoma_species", length(trypanosoma))
usutu <- c("usutu_virus")
names(usutu) <- "usutu_virus"

pathogen_name <- c(anaplasma, arenaviridae, babesia, bartonella, borrelia, coxiella, e_coli, ehrlichia, eimeria, flavivirus, hantavirus, hydatid,
                   k_pneumoniae, lassa, leishmania, leptospirosis, mammarenavirus, mycobacteria, mycoplasma, orentia, orthopoxvirus, phlebovirus,
                   plagiorchis, plasmodium, rickettsia, rvf, schistosoma, strongyloides, toxoplasma, trichuris, trypanosoma, usutu) %>%
  setNames(names(.), .)

write_rds(pathogen_name, here("data_clean", "pathogen_dictionary.rds"))
