# This should help you get started:
  
library(maptools)
library(ggplot2)
library(ggalt)
library(ggthemes)
library(tibble)
library(viridis)

ID_5651dt <- as.data.table(ID_5651)[,`:=`("Posizione"=`Posizione Prevalente`, "AgeCut"=`Classe età`)]
ID_5651dt[,Provincia:=gsub("Provincia Autonoma di ","",Provincia)]
ID_5651dt[,Provincia:=gsub("-Cesena","",Provincia)]
ID_5651dt[,Provincia:=gsub("/","-",Provincia)]

# LavoProv_cast1 <- dcast(ID_5651dt[,.SD,
#                                 .SDcols=colnames(ID_5651dt)[1:(length(colnames(ID_5651dt))-2)]],
#                         ... ~ `Posizione Prevalente` , value.var = "Lavoratori" )

LavoProv_val1 <- ID_5651dt[,sum(Lavoratori),.(Provincia,Cittadinanza,Posizione)]
LavoProv_val1Cast <- dcast(LavoProv_val1, Provincia + Posizione ~ Cittadinanza, value.var = "V1")
LavoProv_val1Cast[,ExtraConPerc:= round((Extracomunitari/Comunitari)*100,6)]

LavProvAnno <- ID_5651dt[,sum(Lavoratori,na.rm = T),.(Anno,Provincia,Posizione,Cittadinanza)] # ,Regione,AgeCut,sesso
LavProvAnno_cast <- dcast(LavProvAnno, Anno + Provincia + Posizione ~ Cittadinanza, value.var = "V1")
# LavProvAnno_cast[,ExtraConPerc:= round((Extracomunitari/Comunitari)*100,2)]

# get italy region map
italy_map <- map_data("italy")

cnames <-aggregate(cbind(long, lat) ~ region, data = italy_map, FUN = function(x) mean(range(x)))
cnames$angle <-0
head(cnames)
# your data will need to have these region names
print(sort(unique(italy_map$region)))
print(sort(unique(LavoProv_val1Cast$Provincia)))

unique(italy_map$region)[!(unique(italy_map$region) %in% unique(LavoProv_val1Cast$Provincia))]
# we'll simulate some data for this
set.seed(1492)
choro_dat <- tibble(region=rep(unique(italy_map$region),length(unique(LavoProv_val1Cast$Posizione))),
                    value=LavoProv_val1Cast[Provincia %in% italy_map$region,
                                            ExtraConPerc],
                    posizione=LavoProv_val1Cast[Provincia %in% italy_map$region,
                                                Posizione])
# we'll use this in a bit
# italy_proj <- "+proj=aea +lat_1=38.15040684902542
# +lat_2=44.925490198742295 +lon_0=12.7880859375"

gg <- ggplot()

# lay down the base layer
gg <- gg + geom_map(data=italy_map, map=italy_map,
                    aes(long, lat, map_id=region),
                    color="#b2b2b2", size=0.1, fill=NA) 

# fill in the regions with the data
for(p in unique(choro_dat$posizione)) {
print(
 gg + geom_map(data=choro_dat[choro_dat$posizione==p,], map=italy_map,
                    aes(fill=value, map_id=region),
                    color="#b2b2b2", size=0.1) + scale_fill_viridis_b(name="Perc ExtraCom") +
    ggtitle(p)
)
}
# + facet_wrap(vars(posizione), scales = "free")

# great color palette (use a better legend title)
gg <- gg + scale_fill_viridis(name="Perc ExtraCom")

# decent map projection for italy choropleth
# gg <- gg + coord_proj(italy_proj)

# good base theme for most maps
gg <- gg + theme_map()

# move the legend
# gg <- gg + theme(legend.position=c(0.95, 0.3))

gg

## This uses a continuous color palette for the region fill. You may want
## to consider binning data and using a discrete fill (IMO that's usually
## a better choice for most choropleths).

#### mappe per anno
chor_dat2 <- LavProvAnno_cast[,.(sum(Comunitari, na.rm = T),sum(Extracomunitari, na.rm = T)),
                              .(Anno,Provincia)]
chor_dat2[,ExtraConPerc:= round((V2/V1)*100,2)]
gg <- ggplot()

# lay down the base layer
gg <- gg + geom_map(data=italy_map, map=italy_map,
                    aes(long, lat, map_id=region),
                    color="#b2b2b2", size=0.1, fill=NA) 

# fill in the regions with the data
for(p in unique(chor_dat2$Anno)) {
  print(
    gg + geom_map(data=chor_dat2[chor_dat2$Anno==p,], map=italy_map,
                  aes(fill=ExtraConPerc, map_id=Provincia),
                  color="#b2b2b2", size=0.1) + scale_fill_viridis_b(name="Perc ExtraCom") +
      
      ggtitle(p)+ theme_map()
  )
}
# + facet_wrap(vars(posizione), scales = "free")

pl <- gg + geom_map(data=chor_dat2[chor_dat2$Anno==p,], map=italy_map,
                    aes(fill=ExtraConPerc, map_id=Provincia),
                    color="#b2b2b2",size=0.1) + #scale_fill_viridis_b(name="Perc ExtraCom") +
  expand_limits(x = italy_map$long, y = italy_map$lat) +
  theme_few()+
  theme(legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank()) +
  scale_fill_gradient(low="white", high="blue") +
  guides(fill = guide_colorbar(barwidth = 10, barheight = .5)) + 
  geom_text(data=cnames, aes(long, lat, label = region,  
                             angle=angle, map_id =NULL), size=2.5) + 
  ggtitle(p)#+ theme_map()
pl

ggplotly(pl)
