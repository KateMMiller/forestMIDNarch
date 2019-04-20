#' @include joinQuadData.R
#' @title sumQuadGuilds: summarizes quadrat species data by guilds
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by first
#' @importFrom magrittr %>%
#'
#' @description This function summarizes output from joinQuadData and calculates average cover and quadrat frequency for each guild. Average cover is corrected for number of quadrats sampled.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Returns all species.}
#' \item{"native"}{Default. Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#' @return Returns a dataframe with average quadrat cover, percent quadrat frequency and quadrat frequency count for tree,shrub/vine,herbaceous,and graminoid. Data are sither summarized for all species, native only, exotic only, or invasive only.
#'
#' @examples
#' importData()
#'
#' # compile exotic quad data for all parks and most recent survey.
#' exo_guilds <- sumQuadGuilds(speciesType = 'exotic', from = 2015, to = 2018)
#'
#' # compile native quad data for more recent survey in COLO, with ferns and forbs split in separate guilds
#' COLO_guilds <- sumQuadGuilds(speciesType = 'native', from = 2015, to = 2018)
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
sumQuadGuilds<-function(speciesType=c('native','exotic','all'), park='all',from=2006, to=2018, QAQC=FALSE, locType='VS', output,...){
  if(!requireNamespace("tidyr", quietly = TRUE)){
    stop("Package 'tidyr' needed for this function to work. Please install it.", call. = FALSE)
  }

  speciesType<-match.arg(speciesType)
  # Prepare the quadrat data
  park.plots<-force(joinLocEvent(park=park,from=from,to=to,QAQC=QAQC,locType=locType,output='short'))
  quads1<-force(joinQuadData(park=park, from=from,to=to,QAQC=QAQC,locType=locType,speciesType=speciesType,
    output='short'))
  quads1<-quads1 %>% mutate(Tree=ifelse(Tree+Shrub>1,0,Tree),Shrub=ifelse(Tree+Shrub>1,1,Shrub)) %>%
    filter(Latin_Name!='No species recorded') %>% droplevels()

  quads2<-if (speciesType=='native'){filter(quads1,Exotic==FALSE)
  } else if (speciesType=='exotic'){filter(quads1,Exotic==TRUE)
  } else if (speciesType=='all'){(quads1)
  }

  # gather to get every combination of plot visit and guild
  quads3<-quads2 %>% group_by(Event_ID,Tree,Shrub,Herbaceous,Graminoid) %>% summarise(avg.cover=sum(avg.cover),
    A2=ifelse(sum(A2)>0,1,0),A5=ifelse(sum(A5)>0,1,0),A8=ifelse(sum(A8)>0,1,0),AA=ifelse(sum(AA)>0,1,0),
    B2=ifelse(sum(B2)>0,1,0),B5=ifelse(sum(B5)>0,1,0),B8=ifelse(sum(B8)>0,1,0),BB=ifelse(sum(BB)>0,1,0),
    C2=ifelse(sum(C2)>0,1,0),C5=ifelse(sum(C5)>0,1,0),C8=ifelse(sum(C8)>0,1,0),CC=ifelse(sum(CC)>0,1,0),
    avg.freq=(A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/first(numQuadrats)) %>%
    mutate(guild= case_when(Tree == 1 ~ 'Tree',
      Shrub == 1 ~ 'Shrub',
      Herbaceous == 1 ~ 'Herbaceous',
      Graminoid == 1 ~ 'Graminoid')) %>% ungroup() %>% select(Event_ID,guild,avg.cover,avg.freq)

  quads3$guild<-as.factor(quads3$guild)

  park.plots2<-park.plots %>% mutate(Graminoid=1,Herbaceous=1,Shrub=1,Tree=1) %>%
    tidyr::gather(key=guild,value=pres,Graminoid:Tree) %>% select(-pres)
  # makes a matrix with every plot visit and every combination of guild

  quads.comb1<-merge(park.plots2,quads3,by=c("Event_ID","guild"),all.x=T)
  quads.comb1[,13:14][is.na(quads.comb1[,13:14])]<-0

  quads.comb2<-quads.comb1 %>% select(Location_ID,Event_ID,Unit_Code:cycle,guild,avg.cover:avg.freq)

  return(data.frame(quads.comb2))
} # end of function

