#' @include joinLocEvent.R
#' @include joinTreeData.R
#' @include joinQuadData.R
#' @include joinRegenData.R
#' @include joinMicroShrubData.R
#' @title makeSppList: creates a species list for each plot
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by rename
#' @importFrom magrittr %>%
#'
#' @description This function creates a plot-level species list from live trees, microplots,
#' quadrats, and additional species lists. This function only works for complete events and active plots.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#' @return Returns a dataframe with species list for each plot.
#'
#' @examples
#' importData()
#'
#' # Compile number of exotic species found per plot in most recent survey for all parks
#' exo_spp <- makeSppList(speciesType = 'exotic', from = 2015, to = 2018)
#' exo_spp$present<-ifelse(is.na(exo_spp$Latin_Name), 0, 1)
#' num_exo_per_plot <- exo_spp %>% group_by(Plot_Name) %>% summarise(numspp=sum(present, na.rm=T))
#'
#' # Compile species list for a given panel of a park
#' PETE_spp <- makeSppList(park = 'PETE', from = 2015, to = 2015)
#'
#' #--- arrange and drop unnecessary fields.
#' PETE_spp_final <- PETE_spp %>% arrange(Plot_Name, Latin_Name) %>%
#'   select(Plot_Name, Latin_Name, Common, tree.stems, stocking.index, avg.quad.cover, shrub.cover,
#'   addspp.present)
#'
#' #--- make species list for a given plot from 2015
#' PETE_021_2015_spp <- PETE_spp %>% filter(Plot_Name == 'PETE-021') %>%
#'   select(Plot_Name, Year, Latin_Name, Common) %>% droplevels()
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
makeSppList<-function(speciesType = c('all', 'native', 'exotic'), park = 'all',
  from = 2007, to = 2019, QAQC = FALSE, panels = 1:4, locType = 'VS',
  output, ...){

  speciesType<-match.arg(speciesType)

  park.plots <- force(joinLocEvent(park = park, from = from, to = to, QAQC = QAQC, locType = locType,
                                   rejected = FALSE, eventType = 'complete',
                                   panels = panels, output='short'))

  trees1 <- force(joinTreeData(park = park, from = from, to = to, QAQC = QAQC, locType = locType,
                               output = 'short', status = 'live'))

  trees2 <- trees1 %>% group_by(Event_ID, TSN, Latin_Name, Common) %>%
                       summarise(tree.stems = length(DBH > 10),
                                 tree.BAcm2 = sum(BA_cm2)) %>% ungroup()

  trees3 <- trees2 %>% select(Event_ID, TSN, tree.stems, tree.BAcm2)

  regen1 <- force(joinRegenData(park = park, from = from, to = to, QAQC = QAQC, locType = locType,
                                output = 'short'))

  regen2 <- regen1 %>% select(Event_ID, TSN, seed.den, sap.den, stock, avg.cover, avg.freq) %>%
                       rename(avg.sd.cover = avg.cover,
                              avg.sd.freq = avg.freq)

  quads1 <- force(joinQuadData(park = park, from = from, to = to, QAQC = QAQC, locType = locType,
                               output = 'short'))

  quads2 <- quads1 %>% select(Event_ID, TSN, avg.cover, avg.freq, new_2019)

  shrub1 <- force(joinMicroShrubData(park = park, from = from, to = to, QAQC = QAQC,
                                     locType = locType, output = 'short'))

  shrub2 <- shrub1 %>% select(Event_ID, TSN, present.old, cover)

  addspp2 <- addspp %>% select(Event_ID, TSN) %>% mutate(addspp = 1)

  comb1 <- merge(trees3, regen2, by = c("Event_ID", "TSN"), all.x = TRUE, all.y = TRUE)
  comb2 <- merge(comb1, quads2, by = c("Event_ID", "TSN"), all.x = TRUE, all.y = TRUE)
  comb3 <- merge(comb2, shrub2, by = c("Event_ID", "TSN"), all.x = TRUE, all.y = TRUE)
  comb4 <- merge(comb3, addspp2, by = c("Event_ID", "TSN"), all.x = TRUE, all.y = TRUE)
  comb5 <- comb4 %>% filter(TSN!=-9999999951)

  comb6<-merge(comb5,
               plants[,c("TSN", "Latin_Name", "Common", "Exotic", "Tree", "Shrub", "Herbaceous",
                         "Graminoid", "Indicator_MIDN")],
               by="TSN", all.x = TRUE)

  comb7 <- if (speciesType == 'native'){filter(comb6, Exotic == FALSE)
    } else if (speciesType == 'exotic'){filter(comb6, Exotic == TRUE)
    } else if (speciesType == 'all'){(comb6)
    }

  comb8 <- merge(park.plots, comb7, by = "Event_ID", all.x = TRUE, all.y = FALSE)

  colnames(comb8)<-c("Event_ID", "Location_ID", "Unit_Code", "Plot_Name", "Plot_Number",
                     "X_Coord", "Y_Coord", "Panel", "Year", "Event_QAQC", "cycle",
                     "TSN", "tree.stems", "tree.BAcm2", "seed.den", "sap.den", "stocking.index",
                     "avg.sd.cover", "avg.sd.freq",
                     "avg.quad.cover", "avg.quad.freq", "new_2019",
                     "shrub.present.old", "shrub.cover", "addspp.present", "Latin_Name",
                     "Common", "Exotic", "Tree", "Shrub", "Herbaceous", "Graminoid", "Indicator_MIDN")

  na_nums <- c("tree.stems", "tree.BAcm2", "seed.den", "sap.den", "stocking.index",
               "avg.sd.cover", "avg.sd.freq","avg.quad.cover", "avg.quad.freq",
               "shrub.present.old", "shrub.cover", "addspp.present")

  na_chrs <- c("Latin_Name", "Common")
  na_TF <- c("Tree", "Shrub", "Vine", "Graminoid", "Herbaceous", "Fern_Ally", "Exotic", "Indicator_MIDN")

  comb8[ , na_nums][is.na(comb8[ , na_nums])] <- 0
  comb8[ , na_chrs][is.na(comb8[, na_chrs])] <- "No species recorded"
  comb8[ , "TSN"][is.na(comb8[, "TSN"])] <- -9999999951
  comb8[ , "new_2019"][is.na(comb8[ , "new_2019"])] <- FALSE

  comb8 <- comb8 %>% mutate(shrub.cover = ifelse(Year > 2010 & is.na(shrub.cover), 0, shrub.cover),
                            shrub.present.old = ifelse(Year <= 2010 & is.na(shrub.present.old), 0,
                                                       shrub.present.old),
                            present = ifelse(TSN != -9999999951, 1, 0))


  return(data.frame(comb8))
  } # end of function
