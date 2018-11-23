#' @include joinQuadData.R
#' @title sumQuadGuilds
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
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
sumQuadGuilds<-function(speciesType='native', park='all',from=2006, to=2018, QAQC=FALSE, locType='VS', output,...){
  # Prepare the quadrat data
  park.plots<-force(joinLocEvent(park=park,from=from,to=to,QAQC=QAQC,locType=locType,output='short'))
  quads1<-force(joinQuadData(park=park, from=from,to=to,QAQC=QAQC,locType=locType,speciesType=speciesType,
    output='short'))
  quads1<-quads1 %>% mutate(Shrub=ifelse(Shrub+Vine>0,1,0)) %>% select(-Vine)

  quads2<-if (speciesType=='native'){filter(quads1,Exotic==FALSE)
  } else if (speciesType=='exotic'){filter(quads1,Exotic==TRUE)
  } else if (speciesType=='all'){(quads1)
  } else if (speciesType!='native'|speciesType!='exotic'|speciesType!='all'){
    stop("speciesType must be either 'native','exotic', or 'all'")}

  # gather to get every combination of plot visit and guild
  quads3<-quads2 %>% group_by(Event_ID,Tree,Shrub,Herbaceous,Graminoid) %>% summarise(avg.cover=sum(avg.cover),
    avg.freq=(ifelse(sum(A2)>1,1,0)+ifelse(sum(A5)>0,1,0)+ifelse(sum(A8)>0,1,0)+ifelse(sum(AA)>0,1,0)+
        ifelse(sum(B2)>0,1,0)+ifelse(sum(B5)>0,1,0)+ifelse(sum(B8)>0,1,0)+ifelse(sum(BB)>0,1,0)+
        ifelse(sum(C2)>0,1,0)+ifelse(sum(C5)>0,1,0)+ifelse(sum(C8)>0,1,0)+ifelse(sum(CC)>0,1,0))/first(numQuadrats)) %>%
    mutate(guild= case_when(Tree == 1 ~ 'Tree',
      Shrub == 1 ~ 'Shrub',
      Herbaceous == 1 ~ 'Herbaceous',
      Graminoid == 1 ~ 'Graminoid')) %>% ungroup() %>% select(-(Tree:Graminoid))
  quads3$guild<-as.factor(quads3$guild)

  park.plots2<-park.plots %>% mutate(Graminoid=1,Herbaceous=1,Shrub=1,Tree=1) %>%
    gather(key=guild,value=pres,Graminoid:Tree) %>% select(-pres)
  # makes a matrix with every plot visit and every combination of guild

  quads.comb1<-merge(park.plots2,quads3,by=c("Event_ID","guild"),all.x=T)
  quads.comb1[,13:14][is.na(quads.comb1[,13:14])]<-0

  quads.comb2<-quads.comb1 %>% select(Location_ID,Event_ID,Unit_Code:cycle,guild,avg.cover:avg.freq)

  return(data.frame(quads.comb2))
} # end of function

