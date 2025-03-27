# 
# read clean meteo daily
# calculate pet
# save rda
# 

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr","data.table","sp")
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
G$d_in1 <-file.path(G$d_home,"output/rda"); list.files(G$d_in1);
G$d_out <-file.path(G$d_home,"output"); if(!dir.exists(G$d_out)){dir.create(G$d_out)};
G$d_out1 <-file.path(G$d_out,"rda"); if(!dir.exists(G$d_out1)){dir.create(G$d_out1)};
#G$d_out2 <-file.path(G$d_out,G$n_script); if(!dir.exists(G$d_out2)){dir.create(G$d_out2)};

### end
print(G)

# FUNCTION ---------------------------------------------------------------------
list.files(G$d_in);
source(file.path(G$d_in,"offiziell_DWD_FAO56_callscript.R"))

# LOAD data -------------------------------------------------------------------------

### Mh
list.files(G$d_in1);
load(file.path(G$d_in1,"04_ex_day_Mi.rda")); list.files(G$d_in1);

### plot coordinates - xy 
{
  aa <-file.path(G$d_in,"l2_bb_be.csv"); list.files(G$d_in);
  bb <-read.table(aa,header = T,sep=",",dec="."); 
  bb <-bb[bb$code_location_mm%in%c("F"),]; bb <-bb[bb$dist<3000,];
  aa <-file.path(G$d_in,"si_plt.csv"); list.files(G$d_in);
  ee <-read.table(aa,header = T,sep=";",dec="."); 
  bb <-merge(bb,ee[,colnames(ee)%in%c("code_plot","altitude_m")],by="code_plot",all.x=T)
  cc <-coordinates(data.frame(x=bb$x_4326,y=bb$y_4326));
  dd <-SpatialPoints(cc,CRS("+init=epsg:4326 +datum=WGS84"));
  dd <-spTransform(dd,CRS("+proj=longlat")); 
  xy <-SpatialPointsDataFrame(dd,bb); 
}

# ET0_FAO --------------------------------------------------
aa <-data.frame(id=c(1:length(xy@data$code_plot)),
  date=NA, doy=NA, long=xy@coords[,1], lat=xy@coords[,2],
  alt=xy@data$altitude_m
)
tt <-seq.Date(as.Date("1996-01-01"),as.Date(Sys.Date()),"day");
ii <-1; bb <-NULL;
for(ii in 1:length(tt))
{
  message(tt[ii])
  ### date
  bb  <-aa;
  bb$date <-tt[ii];
  bb$doy <- as.integer(format(tt[ii],"%j"));
  ### values 
  jj <-1; cc <-names(Me)
  for(jj in 1:length(Me))
  {
    cc <-Me[[jj]]; cc <-cc[cc$date%in%tt[ii],];
    bb[jj,"tadm"] <-cc$AT;
    bb[jj,"tadx"] <-cc$AT_max;
    bb[jj,"tadn"] <-cc$AT_min;
    bb[jj,"rsds"] <-cc$SR*0.0036;
    bb[jj,"wsdm"] <-cc$WS;
    bb[jj,"rfmean"] <-cc$RH;
  }
  ### pet
  {
    dd <-na.omit(bb)
    dd$pet <-DWD_FAO_56(
      z=10,
      latitude = dd$lat,
      altitude = dd$alt,
      tm = dd$tadm,
      tmax = dd$tadx,
      tmin = dd$tadn,
      wind = dd$wsdm,
      doy = dd$doy,
      sundur_h = NA,
      rsds = dd$rsds,
      rfmean = dd$rfmean,
      vapdm = NA,
      tempversion = "FAO",
      full.return = F
    )
  }
  ### save
  jj <-1;
  for(jj in 1:nrow(dd))
  {
    hh <-dd[jj,"id"]; gg <-dd[jj,"date"];
    Me[[hh]][Me[[jj]]$date%in%gg,"PET"] <-round(dd[jj,"pet"],4);
  }
}


# PLOT years ------------------------------------------------------
G$d_temp <-file.path(G$d_out,G$n_script); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ll <-names(Me);
ii <-1;
for(ii in 1:length(ll))
{
  aa <-Me[[ll[ii]]]; message(ll[ii]); 
  ### name
  nam <-colnames(aa)[!colnames(aa)%in%c("date")];
  nam <-nam[nam%in%c("PET")];
  kk <-1;
  for(kk in 1:length(nam))
  {
    ### names
    {
      ### nam_y
      nam_y <-nam[kk];
      if(nam_y%in%c("AT","AT_min","AT_mean")){nam_y <-paste0(nam_y," [°C]")}
      if(nam_y%in%c("PR")){nam_y <-paste0(nam_y," [mm]")}
      if(nam_y%in%c("RH")){nam_y <-paste0(nam_y," [%]")}
      if(nam_y%in%c("SR")){nam_y <-paste0(nam_y," [W m-2]")}
      if(nam_y%in%c("WS")){nam_y <-paste0(nam_y," [m s-1]")}
      if(nam_y%in%c("PET")){nam_y <-paste0(nam_y," [mm]")}
    }
    ### data
    bb <-aa[is.na(aa$date)==F,]; bb$dat <-as.Date(bb$date); bb$wert <-bb[,nam[kk]];
    bb <-bb[,c("dat","wert")]; bb$yr <-format(bb$dat,"%Y"); bb$doy <-as.integer(format(bb$dat,"%j"));
    yr <-levels(as.factor(bb$yr));
    bb <-bb[!format(bb$dat,"%m-%d")%in%c("02-29"),]; # ignore leap years
    ### color
    cc <-colorRampPalette(c("grey10","grey90","gold","orange"));
    ccc <-cc(length(yr));
    ccc[length(ccc)] <-"blue3"; ccc[length(ccc)-1] <-"green3"; ccc[length(ccc)-2] <-"red3";
    ### limits
    t0 <-1; t1 <-356; # summary(bb$wert)
    z0 <- floor(min(bb$wert,na.rm=T));
    z1 <- ceiling(max(bb$wert,na.rm=T));
    ### window
    graphics.off();
    out <-file.path(G$d_temp,paste(nam[kk],ll[ii],".png",sep="_"));
    png(out, units="mm", width=160, height=100, res=300);
    par(mar=c(3,3,2,1),mgp=c(2,1,0),lab=c(12,5,7)); # par()
    ### base
    plot(c(z0,z1)~c(t0,t1),type="l",ylim=c(z0,z1),
         ylab=nam_y,xlab="DOY",col="white",
         main=paste(nam[kk]," - ",ll[ii],sep=""));
    grid(nx = NA, ny=NULL); # par()$usr
    ### legend
    if(nam[kk]%in%c("rh_max","rh_mean","rh_min")){legend("bottom",ncol=10,yr,col=ccc,pch=16,cex=.5)};
    if(!nam[kk]%in%c("rh_max","rh_mean","rh_min")){legend("top",ncol=10,yr,col=ccc,pch=16,cex=.5)};
    ### years
    jj <-1;
    for(jj in 1:length(yr))
    {
      dd <-bb[bb$yr%in%yr[jj],]; dd <-dd[order(dd$doy),];
      lines(dd$wert~dd$doy,dd,col=ccc[jj])
    }
    ### save
    graphics.off(); 
  }
}


# SAVE --------------------------------------------------------------------
Mp <-Me;
out <-paste(G$n_script,"Mp.rda",sep="_");
save(Mp,file = file.path(G$d_out1,out));

# load(file.path(G$d_out1,out)); Me <-Mp;

# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")