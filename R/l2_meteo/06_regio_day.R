# 
# read regionalized data
# clip plots 2 list
# save rda
#
# note: solar radiation ends with year 2020 (so far)

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr","sp","sf","terra")
for(ii in 1:length(ll)){aa <-ll[ii];if(!aa%in%rownames(installed.packages()))install.packages(aa, dependencies = TRUE); library(aa, character.only = TRUE)}


# GLOBALS G ------------------------------------------------------------------
G <-list() ### list workfiles and facts
aa <-getActiveDocumentContext()$path
bb <-unlist(str_split(aa,"\\/"))
### name
G$n_script <-str_sub(bb[length(bb)],1,-3);
G$n_project <-bb[length(bb)-1];
G$n_plot <-str_sub(G$n_script,6,9)
### time
G$t_date <-Sys.Date() 
G$t_year <-as.integer(format(Sys.Date(),"%Y")); # G$year <-2023;
### dir
G$d_home <-dirname(aa); message(G$d_home);
G$d_in <-file.path(G$d_home,"input");  list.files(G$d_in)
G$d_in1 <-file.path(G$d_home,"output/rda");  list.files(G$d_in1)
aa <-unlist(str_split(G$d_home,"/lfe_repos/"));
G$d_in2 <-paste(aa[1],"lfe_repos/FUK-Meteo/DWDregio/output/02_dwd_nc2tif/",sep="/");  list.files(G$d_in2)
G$d_out <-file.path(G$d_home,"output"); list.files(G$d_out)
G$d_in1 <-file.path(G$d_out ,"rda");  list.files(G$d_in1)
G$d_out1 <-file.path(G$d_out ,"rda");  list.files(G$d_out1)
### end
print(G)

# set data -------------------------------------------------------------------------

### xy 
aa <-file.path(G$d_in,"l2_bb_be.csv"); list.files(G$d_in);
bb <-read.table(aa,header = T,sep=",",dec="."); 
bb <-bb[bb$code_location_mm%in%c("F"),]; bb <-bb[bb$dist<3000,];
cc <-coordinates(data.frame(x=bb$x_4326,y=bb$y_4326));
dd <-SpatialPoints(cc,CRS("+init=epsg:4326 +datum=WGS84")); 
xy <-SpatialPointsDataFrame(dd,bb); 

### icpf_day
list.files(G$d_in1);
load(file.path(G$d_in1,"05_pet_day_Mp.rda"))

# clip regio ----------------------------------------------------------------
### var
list.files(G$d_in2); 
aa <-list.files(file.path(G$d_in2,"hyras")); aa;
bb <-paste("hyras",aa,sep="/");
var <-c(bb,"et_fao")
### nam
nam <-c("AT_max","AT","AT_min","RH","PR","RS","ET");
### dat
dat <-seq.Date(as.Date("01-01-1960","%d-%m-%Y"),Sys.Date(),by="day");
### list
REG <-list();
ll <-c(1961:G$t_year);
kk <-5;   # list.files(file.path(G$d_in2,var2[jj]));
for(kk in 1:length(var))
{
  REG[[nam[kk]]] <-NULL;
  xx <-data.frame(matrix(NA,0,c(nrow(xy)+1))); 
  colnames(xx) <-c("date",xy$code_plot);
  ii <-1;
  for(ii in 1:length(ll))
  {
    uu <-file.path(G$d_in2,var[kk],ll[ii]);
    aa <-list.files(uu); aa <-aa[str_detect(aa,".tif$")];  
    if(length(aa)==0){message("missing year - next"); message(ll[ii]); message(nam[kk]); 
      REG[[nam[kk]]] <-xx[order(xx$date, decreasing = F),]; next;}
    aa <-aa[str_detect(aa,paste0("_",ll[ii],"_"))];
    if(length(aa)<365)
      {message("missing dates in yr"); message(ll[ii]);}
    bb <-seq.Date(as.Date(paste0(ll[ii],"-01-01")),as.Date(paste0(ll[ii],"-12-31")), by="day");
    bb <-data.frame(day=bb,jul=as.integer(format(bb,"%j")))
    jj <-2; 
    for(jj in 1:length(aa))
    {
      cc <-aa[jj]; print(cc);
      dd <-unlist(str_split(cc,as.character("_")));
      ee <-bb[bb$jul%in%dd[c(length(dd)-1)],]; print(ee);
      ff <-rast(file.path(uu,cc)); 
      gg <-spTransform(xy,crs(ff)); # plot(ff); plot(gg,add=T)
      hh <-extract(ff,st_as_sf(gg));
      xx[c(nrow(xx)+1),] <-NA;
      xx[c(nrow(xx)),] <-c(as.character(ee[,1]),round(hh[,2],4));
      ### end jj
    }
    ### end ii
  }
  REG[[nam[kk]]] <-xx[order(xx$date, decreasing = F),];
  ### end kk
}

# SAVE ------------------------------------------------------------------------
out <-paste(G$n_script,".rda",sep="_");
save(REG,file = file.path(G$d_out1,out));
# load(file.path(G$d_out1,out))

# CLEAN ------------------------------------------------------------------------------
rm(list = ls());
cat("//014")

