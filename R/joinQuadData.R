#' @include joinLocEvent.R
#' @title joinQuadData: compiles quadrat species data
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by left_join
#' @importFrom magrittr %>%
#'
#' @description This function combines quadrat species cover data with species names
#' and allows you to filter on species types, park, years, and visit type. Note that the
#' Shrub guild also includes woody vine species. Note that quadrat seedling data, including
#' percent cover and quadrat frequency of tree species that are seedling sized are reported
#' in joinRegenData and not summarized here.
#'
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only, including Robinia pseudoacacia}
#' \item{"native_noROBPSE"}{Returns native species except Robinia pseudoacacia}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#' @return Returns a dataframe with average quadrat cover and quadrat frequency for each species
#' detected in the quadrats species section of the protocol. Starting in 2019 all woody species
#' were estimated for percent cover and all life stages were lumped into one percent cover estimate.
#' The new_2019 column indicates whether a species was recorded in avg.cover and avg.freq because
#' of the protocol change in 2019. Seedling-sized tree species will continue to get percent cover
#' estimates for 2019-2022 to maintain protocol overlap, and these data are covered in joinRegenData().
#'
#' @examples
#' importData()
#' # compile quadrat data for exotic species in PETE for all years
#' PETE_quads <- joinQuadData(park = 'PETE', speciesType = 'exotic')
#'
#' # compile native species only for all parks in most recent survey
#' native_quads <- joinQuadData(speciesType = 'native', from = 2015, to = 2018)
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
joinQuadData <- function(speciesType = c('all', 'native', 'native_noROBPSE', 'exotic'),
                         park = 'all', from = 2007, to = 2019, QAQC = FALSE, locType = 'VS',
                         panels = 1:4, output, ...){

  speciesType <- match.arg(speciesType)

  # Prepare the quadrat data
  quadsamp$numQuadrats <- apply(quadsamp[ , c(3:14)], 1, sum)
  park.plots <- force(joinLocEvent(park = park, from = from, to = to, QAQC = QAQC,
                                 locType = locType, panels = panels, output = 'short'))


  quads1 <- merge(park.plots, quadsamp[,c("Event_ID","numQuadrats")], by="Event_ID", all.x = TRUE)

  plants <- plants %>% mutate(Tree = ifelse(Latin_Name == "Rhamnus cathartica", FALSE, Tree))

  quads2<-merge(quads1,
    quads[,c("Event_ID", "TSN", "qA2_Cover_Class_ID", "qA5_Cover_Class_ID", "qA8_Cover_Class_ID",
             "qAA_Cover_Class_ID", "qB2_Cover_Class_ID", "qB5_Cover_Class_ID", "qB8_Cover_Class_ID",
             "qBB_Cover_Class_ID", "qC2_Cover_Class_ID", "qC5_Cover_Class_ID", "qC8_Cover_Class_ID",
             "qCC_Cover_Class_ID")], by = 'Event_ID', all.x = TRUE)

 # Convert coverclasses to midpoints for all 8 quadrats

  quad_names <- names(quads2[14:25])

  quads2[ , quad_names][quads2[ , quad_names] == 1] <- 0.1
  quads2[ , quad_names][quads2[ , quad_names] == 2] <- 1.5
  quads2[ , quad_names][quads2[ , quad_names] == 3] <- 3.5
  quads2[ , quad_names][quads2[ , quad_names] == 4] <- 7.5
  quads2[ , quad_names][quads2[ , quad_names] == 5] <- 17.5
  quads2[ , quad_names][quads2[ , quad_names] == 6] <- 37.5
  quads2[ , quad_names][quads2[ , quad_names] == 7] <- 62.5
  quads2[ , quad_names][quads2[ , quad_names] == 8] <- 85
  quads2[ , quad_names][quads2[ , quad_names] == 9] <- 97.5

  new.names<-c('A2','A5','A8','AA','B2','B5','B8','BB','C2','C5','C8','CC')

  colnames(quads2) <- c(names(quads2[1:13]), new.names)

  quads2[ , new.names][is.na(quads2[ , new.names])] <- 0
  quads3 <- quads2 %>% mutate(avg.cover = (A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/numQuadrats) #%>% select(Event_ID:TSN,avg.cover)
  quads3[ , c(new.names, "avg.cover")][quads3[ , c(new.names, "avg.cover")] > 0] <- 1
  quads3 <- quads3 %>% mutate(avg.freq = (A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/numQuadrats)

  #intersect(names(park.plots), names(quads3))
  quads3b <- quads3[ , c("Location_ID", "Event_ID", "numQuadrats", "TSN", "A2", "A5",
                         "A8", "AA", "B2", "B5", "B8", "BB", "C2", "C5", "C8", "CC",
                         "avg.cover", "avg.freq")]

  quads3c <- merge(park.plots[, c("Location_ID", "Event_ID", "Plot_Name")],
                   quads3b, by = c("Location_ID", "Event_ID"),
                   all.x = TRUE, all.y = TRUE)

  quads4 <- merge(quads3c, plants[ , c('TSN', "Latin_Name", "Common", "Tree", "Shrub",
                                      "Vine", "Graminoid", "Herbaceous", "Fern_Ally", "Exotic",
                                      "Indicator_MIDN")],
                      by = "TSN", all.x = TRUE)

  quads5 <- if (speciesType == 'native'){filter(quads4, Exotic == FALSE)
  } else if (speciesType == 'exotic'){filter(quads4, Exotic == TRUE)
  } else if (speciesType == 'native_noROBPSE'){
    filter(quads4, Exotic == FALSE, Latin_Name !="Robinia pseudoacacia")
  } else if (speciesType == 'all'){(quads4)
  }


  quads6 <- quads5 %>% mutate(avg.cover = ifelse(TSN == -9999999951, 0, avg.cover),
                              avg.freq = ifelse(TSN == -9999999951, 0, avg.freq)) %>%
                       select(Event_ID, TSN, numQuadrats:Indicator_MIDN)

  quad_comb <- merge(park.plots, quads6, by = "Event_ID", all.x = TRUE) %>% select(-numQuadrats)

  quad_comb2 <- merge(quad_comb, quadsamp[,c("Event_ID","numQuadrats")], by="Event_ID", all.x = TRUE)

  na_nums <- c("A2", "A5", "A8", "AA", "B2", "B5", "B8", "BB", "C2", "C5", "C8", "CC",
              "avg.cover", "avg.freq")
  na_chrs <- c("Latin_Name", "Common")
  na_TF <- c("Tree", "Shrub", "Vine", "Graminoid", "Herbaceous", "Fern_Ally", "Exotic", "Indicator_MIDN")

  quad_comb2[ , na_nums][is.na(quad_comb2[, na_nums])] <-0
  quad_comb2[ , na_chrs][is.na(quad_comb2[, na_chrs])] <- "No species recorded"
  quad_comb2[ , "TSN"][is.na(quad_comb2[, "TSN"])] <- -9999999951

  quad_final <- quad_comb2[ , c("Location_ID", "Event_ID", "Unit_Code", "Plot_Name",
                                "Plot_Number", "X_Coord", "Y_Coord", "Panel", "Year",
                                "Event_QAQC", "cycle", "TSN", "numQuadrats", "A2", "A5",
                                "A8", "AA", "B2", "B5", "B8", "BB", "C2", "C5", "C8", "CC",
                                "avg.cover", "avg.freq", "Latin_Name", "Common",
                                "Tree", "Shrub", "Vine", "Graminoid", "Herbaceous",
                                "Fern_Ally", "Exotic", "Indicator_MIDN")]

  quad_final <- quad_final %>% mutate(new_2019 = ifelse((Tree == TRUE | Shrub == TRUE) &
                                                         Indicator_MIDN == FALSE &
                                                         !is.na(Tree) & !is.na(Shrub) &
                                                         Year == 2019, TRUE, FALSE))

  return(data.frame(quad_final))
} # end of function

