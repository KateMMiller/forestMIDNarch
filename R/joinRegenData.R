#' @include joinLocEvent.R
#' @title joinRegenData: compiles seedling and sapling data
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by ungroup
#' @importFrom magrittr %>%
#' @importFrom stringr str_sub str_pad
#'
#' @description This function combines seedling and sapling data, and calculates stocking index at the 1 sq. m level.
#' Quadrat percent cover is also averaged across the quadrats, and only includes cover of seedling sized tree species
#' (i.e. the original protocol method). Stocking index thresholds are 2 for areas with low deer, and 8 for areas
#' with high deer. Must run importData first.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species}
#' \item{"native"}{Returns native species only, including Robinia pseudoacacia}
#' \item{"native_noROBPSE"}{Returns native species except Robinia pseudoacacia}
#' \item{"exotic"}{Returns exotic species only, not including Robinia pseudoacacia}
#' }
#' @param canopyForm Allows you to filter on canopy species only or include all species.
#' \describe{
#' \item{"all"}{Returns all species, including low canopy species.}
#' \item{"canopy"}{Default. Returns canopy-forming species only}
#'}
#' @param units Calculates seedling and sapling densities based on different units.
#' \describe{
#' \item{"sq.m"}{Default. Returns seedling and sapling densities per square meter.}
#' \item{"ha"}{Returns seedling and sapling densities per hectare}
#' \item{"acres"}{Returns densities per acre}
#'}
#' @param height Determines whether all height classes are included, or just >15cm seedlings.
#' \describe{
#' \item{"ht15"}{Default. Returns seedlings >=15cm and all saplings.}
#' \item{"all"}{Returns all seedlings and saplings, including 5-15cm seedlings in VAFO.}
#'}
#'
#'
#' @return returns a dataframe with seedling and sapling densities, stocking index,
#' quadrat seedling cover and quadrat seedling frequency. Quadrat frequency is based on
#' cover > 0 in a quadrat.
#'
#' @examples
#' importCSV('./forest_csvs/')
#' # Compile seedling and sapling data for all parks and all species in most recent cycle,
#' # and only include seedlings >=15cm tall (default).
#' regen_data <- joinRegenData(canopyForm = 'all', from = 2015, to = 2018)
#'
#' # compile regen data for canopy-forming (default), native species of all size classes in VAFO for all years
#' VAFO_regen <- joinRegenData(park = 'VAFO', speciesType = 'native', height = 'all')
#'
#' # Compile seedling and sapling densities as stems/ha for all parks in most recent survey
#' regen_data <- joinRegenData(units = 'ha', from = 2015, to = 2018)
#'
#' @export
#'
#------------------------
# Joins quadrat and microplot tables and filters by park, year, and plot/visit type
#------------------------
joinRegenData <- function(speciesType = c('all', 'native', 'native_noROBPSE', 'exotic'), canopyForm = c('canopy', 'all'),
  units = c('sq.m', 'ha', 'acres'), park = 'all', from = 2007, to = 2019, QAQC = FALSE,
  locType = 'VS', height=c('ht15','all'), panels=1:4, output, ...){

  speciesType <- match.arg(speciesType)
  canopyForm <- match.arg(canopyForm)
  units <- match.arg(units)
  height <- match.arg(height)

  park.plots <- force(joinLocEvent(park = park, from = from, to = to, QAQC = QAQC, locType = locType,
                                 rejected = F, panels = panels, output='verbose'))

  park.plots <- park.plots %>% select(Location_ID, Event_ID, Unit_Code, Plot_Name, Plot_Number, X_Coord, Y_Coord,
                                      Panel, Year, Event_QAQC, cycle, Loc_Type)

# Prepare the seedling data

  quad1 <- merge(park.plots, quadsamp[,1:14], by = 'Event_ID', all.x = TRUE)
  quad1$NumQuads <- rowSums(quad1[,14:25])

  seed <- merge(quad1[,c(1:12,26)], sdlg[,c(1:4,6:12)], by = "Event_ID", all.x = T)
  seed[,17:22][is.na(seed[,17:22])] <- 0
  seed$Cover <- as.numeric(seed$Cover)
  seed$freq <- ifelse(!is.na(seed$Cover) & seed$Cover>0, 1, 0)


  seed2 <- seed %>% group_by(Event_ID,TSN) %>%
                    summarise(NumQuads = first(NumQuads),
                              seed5.15m2 = sum(Seedlings_5_15cm)/NumQuads,
                              seed15.30m2 = sum(Seedlings_15_30cm)/NumQuads,
                              seed30.100m2 = sum(Seedlings_30_100cm)/NumQuads,
                              seed100.150m2 = sum(Seedlings_100_150cm)/NumQuads,
                              seed150pm2 = sum(Seedlings_Above_150cm)/NumQuads,
                              avg.cover = sum(Cover)/NumQuads,
                              avg.freq = sum(freq)/NumQuads)
  #} # need to add a path for including 5-15 in summary below

  seed3 <- seed2 %>% mutate(seed.stock = (1*seed15.30m2 + 2*seed30.100m2 + 20*seed100.150m2 + 50*seed150pm2),
                            seed.dens.m2 = seed15.30m2 + seed30.100m2 + seed100.150m2 + seed150pm2)

  # Prepare the sapling data
  saps1 <- merge(micro,
                 saps[,c("Microplot_Sapling_Data_ID", "Microplot_Characterization_Data_ID", "Tree_ID",
                         "DBH", "Status_ID")],
                 by = "Microplot_Characterization_Data_ID", all.y = TRUE, all.x = TRUE)

  saps2 <- merge(saps1, trees[,c("Location_ID", "Tree_ID", "TSN", "Tree_Number_MIDN")],
                 by = c("Tree_ID"), all.x = TRUE, all.y = FALSE)

  saps3 <- merge(park.plots, saps2, by = c('Location_ID', 'Event_ID'), all.x = TRUE)

  saps4 <- merge(saps3, plants[ , c("TSN", "Latin_Name", "Common", 'Canopy_Exclusion','Exotic')],
                 by = "TSN", all.x = TRUE)

  saps4$DBH[is.na(saps4$DBH)] <- 0

  saps4 <- saps4 %>% mutate(sap = ifelse(DBH > 0 & DBH < 10, 1, 0),
                            Status_ID = ifelse(is.na(Status_ID), 'nospp', paste(Status_ID)))

  saps5 <- saps4 %>% group_by(Event_ID, TSN, Status_ID, Loc_Type) %>%
                     summarise(Latin_Name = first(Latin_Name),
                               Common = first(Common),
                               Canopy_Exclusion = first(Canopy_Exclusion),
                               Exotic = first(Exotic),
                               sap.stems = sum(sap, na.rm=T),
                               avg.sap.dbh = mean(DBH, na.rm=T)) %>%
                     ungroup() %>%
                     mutate(wgt.sap.stock = ifelse(Loc_Type != 'Deer', 50/((pi*3^2)*3), 50/100),
                     wgt.sap.dens = ifelse(Loc_Type != 'Deer', (pi*3^2)*3, 100))

  alive <- c("AB", "AF", "al", "AL", "AM", "AS", "RB", "RF", "RL", "RS")

  saps6 <- saps5 %>% filter(Status_ID %in% alive) %>%
                     group_by(Event_ID, TSN, Latin_Name, Common, Exotic) %>%
                     summarise(sap.dens.m2 = sum(sap.stems)/first(wgt.sap.dens),
                               sap.stock = first(wgt.sap.stock)*sum(sap.stems)) %>%
                     droplevels()

  saps7 <- merge(park.plots, saps6, by = "Event_ID", all.x = TRUE) # left join sapling back to plot visit data to show all plots
  saps7$sap.stock[is.na(saps7$sap.stock)] <- 0
  saps7$sap.dens.m2[is.na(saps7$sap.dens.m2)] <- 0

# Combine seedling and sapling data
  regen1 <- merge(park.plots, seed3, by = 'Event_ID', all.x = TRUE, all.y = FALSE)
  regen2 <- merge(regen1, saps7[,c("Event_ID","TSN","sap.dens.m2","sap.stock")],
                  by = c("Event_ID", "TSN"), all.x = TRUE, all.y = TRUE)
  regen3 <- merge(regen2, plants[,c('TSN', 'Latin_Name', 'Common', 'Exotic', 'Canopy_Exclusion')],
                  by = 'TSN', all.x = TRUE, all.y = FALSE)

  regen4 <- if(canopyForm == 'canopy'){filter(regen3, Canopy_Exclusion == FALSE)
     } else if(canopyForm == 'all'){(regen3)
     }

  regen5 <- if (speciesType == 'native'){filter(regen4, Exotic == FALSE)
     } else if (speciesType == 'exotic'){filter(regen4, Exotic == TRUE)
     } else if (speciesType == 'native_noROBPSE'){
         filter(regen4, Exotic == FALSE, Latin_Name !="Robinia pseudoacacia")
     } else if (speciesType == 'all'){(regen4)
     }

  regen5[,15:25][is.na(regen5[,15:25])] <- 0

  regen5 <- regen5 %>% mutate(stock = seed.stock + sap.stock) %>% select(-seed.stock, -sap.stock)

  regen6 <- if (units == 'sq.m'){
    regen5 %>%
        mutate(
          seed5.15 = seed5.15m2,
          seed15.30 = seed15.30m2,
          seed30.100 = seed30.100m2,
          seed100.150 = seed100.150m2,
          seed150p = seed150pm2,
          seed.den = seed.dens.m2,
          sap.den = sap.dens.m2)
    } else if (units == 'ha'){
    regen5 %>%
      mutate(
        seed5.15 = seed5.15m2*10000,
        seed15.30 = seed15.30m2*10000,
        seed30.100 = seed30.100m2*10000,
        seed100.150 = seed100.150m2*10000,
        seed150p = seed150pm2*10000,
        seed.den = seed.dens.m2*10000,
        sap.den = sap.dens.m2*10000)
    } else if (units == 'acres'){
    regen5 %>%
      mutate(
        seed5.15 = seed5.15m2*4046.856,
        seed15.30 = seed15.30m2*4046.856,
        seed30.100 = seed30.100m2*4046.856,
        seed100.150 = seed100.150m2*4046.856,
        seed150p = seed150pm2*4046.856,
        seed.den = seed.dens.m2*4046.856,
        sap.den = sap.dens.m2*4046.856)
    }

  regen7 <- regen6 %>% select(Event_ID, TSN, Latin_Name, Common, Exotic, Canopy_Exclusion,
                              seed5.15, seed15.30, seed30.100, seed100.150, seed150p,
                              seed.den, sap.den, stock, avg.cover, avg.freq) %>% droplevels()

  regen8 <- merge(park.plots, regen7, by="Event_ID", all.x = TRUE, all.y = TRUE)

  regen8[,18:26][is.na(regen8[,18:26])] <- 0

  regen9 <- if(height != 'all'){
    regen8 %>% select(-seed5.15)
  } else {regen8}

  return(data.frame(regen9))
} # end of function
