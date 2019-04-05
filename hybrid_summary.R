##Script to summarise hybrid data collected from galooli

#import libraries
library(xlsx)
library(dplyr)

### read files
hybriddata1 <- read.xlsx("Summary - Configurable - ATC Uganda.xlsx", sheetName = "data", colClasses = "character")
hybriddata2 <- read.xlsx("Summary - Configurable - ATC Uganda 2.xlsx", sheetName = "data", colClasses = "character")

####clean data
##clean names of hybriddata1
names(hybriddata1) <- sapply(names(hybriddata1), function(x) gsub("\\(", "", x))
names(hybriddata1) <- sapply(names(hybriddata1), function(x) gsub("\\)", "", x))
names(hybriddata1) <- sapply(names(hybriddata1), function(x) gsub("\\-", "", x))
names(hybriddata1) <- sapply(names(hybriddata1), function(x) gsub("\\,", "", x))
names(hybriddata1) <- sapply(names(hybriddata1), function(x) gsub("\\.", "", x))
names(hybriddata1) <- sapply(names(hybriddata1), function(x) gsub(" ", "", x))
names(hybriddata1) <- tolower(names(hybriddata1))

##clean names of hybriddata2
names(hybriddata2) <- sapply(names(hybriddata2), function(x) gsub("\\(", "", x))
names(hybriddata2) <- sapply(names(hybriddata2), function(x) gsub("\\)", "", x))
names(hybriddata2) <- sapply(names(hybriddata2), function(x) gsub("\\-", "", x))
names(hybriddata2) <- sapply(names(hybriddata2), function(x) gsub("\\,", "", x))
names(hybriddata2) <- sapply(names(hybriddata2), function(x) gsub("\\.", "", x))
names(hybriddata2) <- sapply(names(hybriddata2), function(x) gsub(" ", "", x))
names(hybriddata2) <- tolower(names(hybriddata2))

##clean rows of hybriddata 1
hybriddata1$usagetimedg1 <- as.numeric(as.character(hybriddata1$usagetimedg1))
hybriddata1$usagetimebattery <- as.numeric(as.character(hybriddata1$usagetimebattery))
hybriddata1$usagetimeunknown <- as.numeric(as.character(hybriddata1$usagetimeunknown))
hybriddata1$dg2ignitioncount <- as.numeric(as.character(hybriddata1$dg1ignitioncount))
hybriddata1$usagetimegrid <- as.numeric(as.character(hybriddata1$usagetimegrid))
hybriddata1$usagetimesolar <- as.numeric(as.character(hybriddata1$usagetimesolar))
hybriddata1$generalinformationsiteid <- as.numeric(as.character(hybriddata1$generalinformationsiteid))
hybriddata1$generalinformationsitename <- as.character(hybriddata1$generalinformationsitename)
hybriddata1$atskwhtotal <- as.numeric(as.character(hybriddata1$atskwhtotal))  ##added line
hybriddata1$extrafield8 <- as.character(hybriddata1$extrafield8)
hybriddata1$extrafield8 <- sapply(hybriddata1$extrafield8, function(x) gsub("\\-", "", x))
hybriddata1$extrafield8 <- sapply(hybriddata1$extrafield8, function(x) gsub(" ", "", x))
hybriddata1$extrafield8 <- tolower(hybriddata1$extrafield8)

##clean rows of hybriddata 2
hybriddata2$usagetimedg1 <- as.numeric(as.character(hybriddata2$usagetimedg1))
hybriddata2$usagetimebattery <- as.numeric(as.character(hybriddata2$usagetimebattery))
hybriddata2$usagetimeunknown <- as.numeric(as.character(hybriddata2$usagetimeunknown))
hybriddata2$dg2ignitioncount <- as.numeric(as.character(hybriddata2$dg1ignitioncount))
hybriddata2$usagetimegrid <- as.numeric(as.character(hybriddata2$usagetimegrid))
hybriddata2$usagetimesolar <- as.numeric(as.character(hybriddata2$usagetimesolar))
hybriddata2$generalinformationsiteid <- as.numeric(as.character(hybriddata2$generalinformationsiteid))
hybriddata2$generalinformationsitename <- as.character(hybriddata2$generalinformationsitename)
hybriddata2$atskwhtotal <- as.numeric(as.character(hybriddata2$atskwhtotal)) 
hybriddata2$extrafield8 <- as.character(hybriddata2$extrafield8)
hybriddata2$extrafield8 <- sapply(hybriddata2$extrafield8, function(x) gsub("\\-", "", x))
hybriddata2$extrafield8 <- sapply(hybriddata2$extrafield8, function(x) gsub(" ", "", x))
hybriddata2$extrafield8 <- tolower(hybriddata2$extrafield8)

##04/04/2019 code
##filter sites with hybrids
hybriddata1 <- hybriddata1 %>% filter(generalinformationhybridstatus == "YES")
hybriddata2 <- hybriddata2 %>% filter(generalinformationhybridstatus == "YES")

##clean out sites with zero kwh
hybriddata1 <- hybriddata1 %>% filter(atskwhtotal != 0)
hybriddata2 <- hybriddata2 %>% filter(atskwhtotal != 0)

## create column with kW
hybriddata1 <- hybriddata1 %>% mutate(siteload = atskwhtotal/24)
hybriddata2 <- hybriddata2 %>% mutate(siteload = atskwhtotal/24)

##sites with hybrid and having grid
hybridgrid1 <- hybriddata1 %>% filter(sitelayout %in% c("TL_UG_OUTDOOR_ONGRID", "TL_UG_INDOOR_ONGRID_HYBRID_REV03",
                                      "TL_UG_INDOOR_ONGRID_HYBRID", "TL_UG_INDOOR_ONGRID_HYBRID_REV2",
                                      "TL_UG_INDOOR_ONGRID_REV01", "TL_UG_INDOOR_ONGRID_REV2",
                                      "TL_UG_OUTDOOR_ONGRID", " TL_UG_OUTDOOR_ONGRID_HYBRID_REV1",
                                      "TL_UG_INDOOR_ONGRID_HYBRID_REV0", "TL_UG_INDOOR_ONGRID_HYBRID_REV1",
                                      "TL_UG_INDOOR_ONGRID_REV0", " TL_UG_INDOOR_ONGRID_REV1",
                                      " TL_UG_OUTDOOR_ONGRID_HYBRID", "TL_UG_OUTDOOR_ONGRID_REV2"))

##sites with hybrid and not having grid
hybridfull1 <- hybriddata1 %>% filter(!(sitelayout %in% c("TL_UG_OUTDOOR_ONGRID", "TL_UG_INDOOR_ONGRID_HYBRID_REV03",
                                                        "TL_UG_INDOOR_ONGRID_HYBRID", "TL_UG_INDOOR_ONGRID_HYBRID_REV2",
                                                        "TL_UG_INDOOR_ONGRID_REV01", "TL_UG_INDOOR_ONGRID_REV2",
                                                        "TL_UG_OUTDOOR_ONGRID", " TL_UG_OUTDOOR_ONGRID_HYBRID_REV1",
                                                        "TL_UG_INDOOR_ONGRID_HYBRID_REV0", "TL_UG_INDOOR_ONGRID_HYBRID_REV1",
                                                        "TL_UG_INDOOR_ONGRID_REV0", " TL_UG_INDOOR_ONGRID_REV1",
                                                        " TL_UG_OUTDOOR_ONGRID_HYBRID", "TL_UG_OUTDOOR_ONGRID_REV2")))

##site with hybrid and solar
hybridsolar1 <- hybriddata1 %>% filter(sitelayout %in% c("TL_UG_INDOOR_OFFGRID_HYBRID_SOLAR_REV03",
                                                         "TL_UG_INDOOR_OFFGRID_HYBRID_SOLAR",
                                                         "TL_UG_INDOOR_OFFGRID_HYBRID_SOLAR_REV2",
                                                         "TL_UG_OUTDOOR_OFFGRID_HYBRID_SOLAR"))

##sites with hybrid only
nameshybridonly <- c("TL_UG_INDOOR_OFFGRID", "TL_UG_INDOOR_OFFGRID_HYBRID",
                     "TL_UG_INDOOR_OFFGRID_HYBRID_REV0", "TL_UG_INDOOR_OFFGRID_HYBRID_REV1",
                     "TL_UG_INDOOR_OFFGRID_HYBRID_REV2", "TL_UG_INDOOR_OFFGRID_REV0",
                     "TL_UG_INDOOR_OFFGRID_REV1", "TL_UG_INDOOR_OFFGRID_REV2",
                     "TL_UG_OUTDOOR_OFFGRID", "TL_UG_OUTDOOR_OFFGRID_HYBRID",
                    "TL_UG_OUTDOOR_OFFGRID_HYBRID_REV03")
##sites with only hybrid
hybridonly1 <- hybriddata1 %>% filter(sitelayout %in% nameshybridonly)

##hybrid only sites with LiBs
hybridonlylibs <- hybridonly1 %>% filter(extrafield8 %in% c("libincell", 
                                                            "liblgchem", 
                                                            "libacmereime"))

#hybrid only libs
hybridonlylibs <- hybridonly1 %>% filter(extrafield8 %in% c("libincell", "liblgchem", "libacmereime"))
####Visualisation graphs
#site load Vs battery time
ggloadbatterytime <- ggplot(data = hybridonly1, aes(usagetimebattery, siteload))
#ggloadbatterytime + geom_point() + geom_smooth()
#ggloadbatterytime + geom_point() + geom_smooth(method = "lm")
#ggloadbatterytime <- ggplot(data = hybridonly1, aes(usagetimebattery, siteload, color = extrafield8))
#ggloadbatterytime + geom_point() + geom_smooth(method = "lm")
#ggloadbatterytime + geom_point() + geom_smooth(method = "lm", se = FALSE)
#gghybridonlylib <- ggplot(data = hybridonlylibs, aes(usagetimebattery, siteload))
#gghybridonlylib + geom_point()
#gghybridonlylib + geom_point() + geom_smooth(method = "lm")
#gghybridonlylib <- ggplot(data = hybridonlylibs, aes(usagetimebattery, siteload, color = extrafield8))
#gghybridonlylib + geom_point() + geom_smooth(method = "lm")
#gghybridonlylib + geom_boxplot()
#ggplot(data = hybridsolar1, aes(usagetimesolar, fill = extrafield8)) + geom_histogram() + facet_grid(.~extrafield8)
png(filename = "siteloaddgignitionsolar.png", width = 960, height = 960)
qplot(siteload, data = hybridsolar1, fill = dg1ignitioncount, alpha = .5, facets = ~dg1ignitioncount) +  
        geom_vline(xintercept = 2) + labs(title = "Ignition counts as per site loads on solar sites")
dev.off()

png(filename = "siteloaddgignitionlib.png", width = 960, height = 960)
qplot(siteload, data = hybridonlylibs, fill = dg1ignitioncount, alpha = .5, facets = ~dg1ignitioncount) +  
            geom_vline(xintercept = 2) + labs(title = "Ignition counts as per site loads on LiB sites")
dev.off()

