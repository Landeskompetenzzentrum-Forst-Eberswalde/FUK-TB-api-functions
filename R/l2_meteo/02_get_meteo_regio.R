# 
# fill meteo by regionalized data
# save rda
# 

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr","sp","sf","terra")
for(ii in 1:length(ll)){aa <-ll[ii];if(!aa%in%rownames(installed.packages()))install.packages(aa, dependencies = TRUE); library(aa, character.only = TRUE)}


# GLOBALS G ------------------------------------------------------------------
G <-list() ### list workfiles and facts
aa <-getActiveDocumentContext()$path
bb <-unlist(str_split(aa,"\\/"))
### name
G$n_script <-str_sub(bb[length(bb)],1,-3) 
G$n_plot <-str_sub(G$n_script,6,9)
### time
G$t_date <-Sys.Date() 
G$t_year <-as.integer(format(Sys.Date(),"%Y")); # G$year <-2023;
### dir
G$d_home <-dirname(aa); message(G$d_home);
G$d_in <-file.path(G$d_home,"input");  list.files(G$d_in)
aa <-unlist(str_split(G$d_home,"/LFE/"));
G$d_in2 <-paste(aa[1],"klima/02_regionalisierung/DWDregio/02_dwd_nc2tif",sep="/");  list.files(G$d_in2)
G$d_out <-paste(G$d_home,"output",sep="/"); if(!dir.exists(G$d_out)){dir.create(G$d_out)};
G$d_in1 <-G$d_out;
### end
print(G)

# set data -------------------------------------------------------------------------

### xy 
aa <-file.path(G$d_in,"l2_bb_be.csv"); list.files(G$d_in);
bb <-read.table(aa,header = T,sep=",",dec="."); 
bb <-bb[bb$code_location_mm%in%c("F"),]; bb <-bb[bb$dist<3000,]; bb <-bb[!bb$code_plot%in%c(1101),];
cc <-coordinates(data.frame(x=bb$x_4326,y=bb$y_4326));
dd <-SpatialPoints(cc,CRS("+init=epsg:4326 +datum=WGS84")); 
xy <-SpatialPointsDataFrame(dd,bb); 
### tb
list.files(G$d_in1);
load(file.path(G$d_in1,"01_get_meteo_day_.rda")); list.files(G$d_in1);
### id
names(MM); id <-paste0(xy$code_plot,"_FF"); id%in%names(MM);
id <-id[id%in%names(MM)]; id <-str_sub(id,1,-4);
xy <-xy[xy@data$code_plot%in%id,]
### var
names(MM[[1]]);
var1 <-c("t_mean","precip","et_fao"); 
list.files(G$d_in2); list.files(file.path(G$d_in2,"hyras"));
var2 <-c("hyras/air_temperature_mean","hyras/precipitation","et_fao");
### nam
nam1 <-c("temperatur","niederschlag","verdunstung");
### dat
dat1 <-seq.Date(as.Date("01-01-1990","%d-%m-%Y"),Sys.Date(),by="day")

# clip regio ----------------------------------------------------------------
REG <-list();
ll <-c(1990:G$t_year);
kk <-1; ii <-1;    # list.files(file.path(G$d_in2,var2[jj]));
for(kk in 1:length(var2))
{
  REG[[var2[kk]]] <-NULL;
  xx <-data.frame(matrix(NA,0,c(nrow(xy)+1))); 
  colnames(xx) <-c("date",xy$code_plot);
  for(ii in 1:length(ll))
  {
    uu <-file.path(G$d_in2,var2[kk],ll[ii]);
    aa <-list.files(uu);
    aa <-aa[str_detect(aa,".tif$")];  aa <-aa[str_detect(aa,paste0("_",ll[ii],"_"))];
    if(length(aa)<365){message("missing dates in yr"); message(ll[ii])}
    bb <-seq.Date(as.Date(paste0(ll[ii],"-01-01")),as.Date(paste0(ll[ii],"-12-31")), by="day");
    bb <-data.frame(day=bb,jul=as.integer(format(bb,"%j")))
    jj <-1; 
    for(jj in 1:length(aa))
    {
      cc <-aa[jj]; print(cc);
      dd <-unlist(str_split(cc,as.character("_")));
      ee <-bb[bb$jul%in%dd[c(length(dd)-1)],];
      ff <-rast(file.path(uu,cc)); 
      gg <-spTransform(xy,crs(ff)); # plot(ff); plot(gg,add=T)
      hh <-extract(ff,st_as_sf(gg));
      xx[c(nrow(xx)+1),] <-NA;
      xx[c(nrow(xx)),] <-c(as.character(ee[,1]),round(hh[,2],4));
    }
  }
  REG[[var2[kk]]] <-xx[order(xx$date, decreasing = F),];
}
out <-paste(G$n_script,".rda",sep="_");
save(REG,file = file.path(G$d_out,out));
# load(file.path(G$d_out,out))

# CLEAN ------------------------------------------------------------------------------
rm(list = ls());
cat("//014")

