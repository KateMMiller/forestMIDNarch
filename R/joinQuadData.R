#' @include joinLocEvent.R
#' @title joinQuadData: compiles quadrat species data
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by left_join
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
#'
#' @description This function combines quadrat species and tree seedling cover data with species names
#' and allows you to filter on species types, park, years, and visit type. Note that the Shrub guild also
#' includes woody vine species. Note that tree cover is included in avg.cover and avg.freq are from quadrat species data
#' and avg.sd.cover and avd.sd.freq are from quadrat seedling data. Starting in 2019, tree seedlings are collected in both.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#' @return Returns a dataframe with cover class midpoints for each quadrat and includes guild for each species.
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
joinQuadData <- function(speciesType = c('all', 'native','exotic'), park = 'all', from = 2007,
                         to = 2018, QAQC = FALSE, locType = 'VS', panels = 1:4, output, ...){

  speciesType<-match.arg(speciesType)

  # Prepare the quadrat data
  quadsamp$numQuadrats<-apply(quadsamp[,c(3:14)], 1,sum)
  park.plots<-force(joinLocEvent(park = park, from = from, to = to, QAQC = QAQC,
                                 locType = locType, panels = panels, output = 'short'))


  quads1<-merge(park.plots, quadsamp[,c("Event_ID","numQuadrats")], by="Event_ID", all.x=T)

  plants<-plants %>% mutate(Tree=ifelse(Latin_Name=="Rhamnus cathartica",0,Tree))

  quads2<-merge(quads1,
    quads[,c("Event_ID","TSN","qA2_Cover_Class_ID","qA5_Cover_Class_ID","qA8_Cover_Class_ID","qAA_Cover_Class_ID",
    "qB2_Cover_Class_ID","qB5_Cover_Class_ID","qB8_Cover_Class_ID","qBB_Cover_Class_ID",
    "qC2_Cover_Class_ID","qC5_Cover_Class_ID","qC8_Cover_Class_ID","qCC_Cover_Class_ID")],by='Event_ID',all.x=T)

 # Convert coverclasses to midpoints for all 8 quadrats
  quads2[,14:25][quads2[,14:25]==1]<-0.1
  quads2[,14:25][quads2[,14:25]==2]<-1.5
  quads2[,14:25][quads2[,14:25]==3]<-3.5
  quads2[,14:25][quads2[,14:25]==4]<-7.5
  quads2[,14:25][quads2[,14:25]==5]<-17.5
  quads2[,14:25][quads2[,14:25]==6]<-37.5
  quads2[,14:25][quads2[,14:25]==7]<-62.5
  quads2[,14:25][quads2[,14:25]==8]<-85
  quads2[,14:25][quads2[,14:25]==9]<-97.5

  old.names<-names(quads2[,14:25])
  new.names<-c('A2','A5','A8','AA','B2','B5','B8','BB','C2','C5','C8','CC')

  colnames(quads2)<-c(names(quads2[1:13]), new.names)

  quads2[,c(14:25)][is.na(quads2[,c(14:25)])]<-0
  quads3<-quads2 %>% mutate(avg.cover=(A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/numQuadrats) #%>% select(Event_ID:TSN,avg.cover)
  quads3[,c(14:25)][quads3[,c(14:25)]>0]<-1
  quads3<-quads3 %>% mutate(avg.freq=(A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/numQuadrats)

  # Now to add the tree seedling cover data
  seed<-merge(quads1,sdlg[,c(1:4,11)], by=c("Event_ID"),all.x=T)
  seed<-seed %>% mutate(CoverClass=Cover) %>% select(-Cover)

  seed<-seed %>% mutate(Cover=case_when(
    is.na(CoverClass) ~ 0,
    CoverClass == 1 ~ 0.1,
    CoverClass == 2 ~ 1.5,
    CoverClass == 3 ~ 3.5,
    CoverClass == 4 ~ 7.5,
    CoverClass == 5 ~ 17.5,
    CoverClass == 6 ~ 37.5,
    CoverClass == 7 ~ 62.5,
    CoverClass == 8 ~ 85,
    CoverClass == 9 ~ 97.5
  ))

  seed2<-seed %>% group_by(Event_ID,Quadrat,TSN) %>%
    summarise(numQuadrats=first(numQuadrats),Cover=sum(Cover))%>%
    left_join(park.plots,.,by="Event_ID") %>% ungroup()

  seed.wide<-seed2 %>% tidyr::spread(Quadrat, Cover, fill=0)  %>% #select(-26) %>%
    mutate(avg.sd.cover=(A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/numQuadrats)

  seed.wide<-seed.wide[,c(1:11,13,12,14:26)]
  seed.wide[,14:25][seed.wide[,14:25]>0]<-1
  seed.wide<-seed.wide %>% mutate(avg.sd.freq=(A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/numQuadrats) %>%
    select(Event_ID, cycle, TSN, avg.sd.cover, avg.sd.freq)

  #quad.comb<-rbind(quads3,seed.wide)
  intersect(names(quads3), names(seed.wide))
  quad.comb <- merge(quads3, seed.wide, by=c("Event_ID", "cycle", "TSN"), all.x=T, all.y=T)

  quad.comb2<-merge(quad.comb,plants[,c('TSN',"Latin_Name","Common","Tree","Shrub","Vine","Graminoid","Herbaceous",
    "Fern_Ally","Exotic")],
    by="TSN", all.x=T)

  quad.comb3<-if (speciesType=='native'){filter(quad.comb2,Exotic==FALSE)
    } else if (speciesType=='exotic'){filter(quad.comb2,Exotic==TRUE)
    } else if (speciesType=='all'){(quad.comb2)
    }

  quad.comb4<-quad.comb3 %>% mutate(avg.cover=ifelse(TSN==-9999999951,0,avg.cover),
    avg.freq=ifelse(TSN==-9999999951,0,avg.freq)) %>% select(Event_ID, TSN, numQuadrats:Exotic)

  quad_final<-merge(park.plots, quad.comb4, by = "Event_ID", all.x = TRUE)

  return(data.frame(quad_final))
} # end of function

