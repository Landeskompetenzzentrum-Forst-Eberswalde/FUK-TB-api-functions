# 
# load tb meteo data
# calculate pet
# save rda
# 

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr","data.table","sp","Evapotranspiration")
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
aa <-unlist(str_split(G$d_home,"/R/"));
G$d_in1 <-file.path(aa[1],"R/tb_access/output/02_save_rda"); list.files(G$d_in1);
aa <-unlist(str_split(G$d_home,"/LFE/"));
G$d_out <-file.path(G$d_home,"output"); if(!dir.exists(G$d_out)){dir.create(G$d_out)};
### end
print(G)

# set data -------------------------------------------------------------------------

### xy 
aa <-file.path(G$d_in,"l2_bb_be.csv"); list.files(G$d_in);
bb <-read.table(aa,header = T,sep=",",dec="."); 
bb <-bb[bb$code_location_mm%in%c("F"),]; bb <-bb[bb$dist<3000,]; bb <-bb[!bb$code_plot%in%c(1101),];
aa <-file.path(G$d_in,"si_plt.csv"); list.files(G$d_in);
ee <-read.table(aa,header = T,sep=";",dec="."); 
bb <-merge(bb,ee[,colnames(ee)%in%c("code_plot","altitude_m")],by="code_plot",all.x=T)
cc <-coordinates(data.frame(x=bb$x_4326,y=bb$y_4326));
dd <-SpatialPoints(cc,CRS("+init=epsg:4326 +datum=WGS84"));
dd <-spTransform(dd,CRS("+proj=longlat")); 
xy <-SpatialPointsDataFrame(dd,bb); 
### tb
list.files(G$d_in1);
load(file.path(G$d_in1,"02_save_rda_meteo_ff_.rda")); list.files(G$d_in1);
### clean
suppressWarnings(rm("aa","bb","cc","dd","ee","ii","ll")); gc();


# loop pet  --------------------------------------------------
MM <-list();
data("constants"); # ET constants
ll <-names(TB); ll;
ll <-ll[!ll%in%c("1101_FF")]
ii <-4; 
for(ii in 1:length(ll))
{
  aa <-TB[[ll[ii]]]; message(ll[ii]); 
  pp <-str_replace(ll[ii],"_FF",""); pp;
  gg <-xy[xy@data$code_plot%in%pp,];
  ### merge variables
  {
    bb <-data.frame(aa["L_Temp"]);  #names(aa);
    colnames(bb) <-c("date",str_sub(colnames(bb)[2],1,-7)); bb[bb[,2]%in%9999,2] <-NA;
    cc <-data.frame(aa["L_Temp_max"]); 
    colnames(cc) <-c("date",str_sub(colnames(cc)[2],1,-7)); cc[cc[,2]%in%9999,2] <-NA;
    bb <-merge(bb,cc,by="date",all=T);
    cc <-data.frame(aa["L_Temp_min"]);
    colnames(cc) <-c("date",str_sub(colnames(cc)[2],1,-7)); cc[cc[,2]%in%9999,2] <-NA;
    bb <-merge(bb,cc,by="date",all=T);
    cc <-data.frame(aa["W_Gesch"]);
    colnames(cc) <-c("date",str_sub(colnames(cc)[2],1,-7)); cc[cc[,2]%in%9999,2] <-NA;
    bb <-merge(bb,cc,by="date",all=T);
    cc <-data.frame(aa["G_Str"]);
    colnames(cc) <-c("date",str_sub(colnames(cc)[2],1,-7)); cc[cc[,2]%in%9999,2] <-NA;
    bb <-merge(bb,cc,by="date",all=T);
    cc <-data.frame(aa["L_Feuchte"]); 
    colnames(cc) <-c("date",str_sub(colnames(cc)[2],1,-7)); cc[cc[,2]%in%9999,2] <-NA;
    bb <-merge(bb,cc,by="date",all=T);
    cc <-data.frame(aa["Nied_unb"]); 
    colnames(cc) <-c("date",str_sub(colnames(cc)[2],1,-7)); cc[cc[,2]%in%9999,2] <-NA;
    bb <-merge(bb,cc,by="date",all=T);
  }
  ### ex out of range
  {
    summary(bb);
    cc <-"L_Temp"; c0 <-60; c1 <- -60;
    dd <-bb[bb[,cc] > c0 & is.na(bb[,cc])==F,];
    if(nrow(dd)>0){bb[rownames(bb)%in%rownames(dd),cc] <-NA; print(paste(cc," --- ",nrow(dd)," obs > ",c0))}
    dd <-bb[bb[,cc] < c1 & is.na(bb[,cc])==F,];
    if(nrow(dd)>0){bb[rownames(bb)%in%rownames(dd),cc] <-NA; print(paste(cc," --- ",nrow(dd)," obs < ",c0))}
    cc <-"L_Temp_max"; c0 <-60; c1 <- -60;
    dd <-bb[bb[,cc] > c0 & is.na(bb[,cc])==F,];
    if(nrow(dd)>0){bb[rownames(bb)%in%rownames(dd),cc] <-NA; print(paste(cc," --- ",nrow(dd)," obs > ",c0))}
    dd <-bb[bb[,cc] < c1 & is.na(bb[,cc])==F,];
    if(nrow(dd)>0){bb[rownames(bb)%in%rownames(dd),cc] <-NA; print(paste(cc," --- ",nrow(dd)," obs < ",c0))}
    cc <-"L_Temp_min"; c0 <-60; c1 <- -60;
    dd <-bb[bb[,cc] > c0 & is.na(bb[,cc])==F,];
    if(nrow(dd)>0){bb[rownames(bb)%in%rownames(dd),cc] <-NA; print(paste(cc," --- ",nrow(dd)," obs > ",c0))}
    dd <-bb[bb[,cc] < c1 & is.na(bb[,cc])==F,];
    if(nrow(dd)>0){bb[rownames(bb)%in%rownames(dd),cc] <-NA; print(paste(cc," --- ",nrow(dd)," obs < ",c0))}
    cc <-"W_Gesch"; c0 <- 50; c1 <- 0;
    dd <-bb[bb[,cc] > c0 & is.na(bb[,cc])==F,];
    if(nrow(dd)>0){bb[rownames(bb)%in%rownames(dd),cc] <-NA; print(paste(cc," --- ",nrow(dd)," obs > ",c0))}
    dd <-bb[bb[,cc] < c1 & is.na(bb[,cc])==F,];
    if(nrow(dd)>0){bb[rownames(bb)%in%rownames(dd),cc] <-NA; print(paste(cc," --- ",nrow(dd)," obs < ",c0))}
    cc <-"G_Str"; c0 <- 1000; c1 <- 0;
    dd <-bb[bb[,cc] > c0 & is.na(bb[,cc])==F,];
    if(nrow(dd)>0){bb[rownames(bb)%in%rownames(dd),cc] <-NA; print(paste(cc," --- ",nrow(dd)," obs > ",c0))}
    dd <-bb[bb[,cc] < c1 & is.na(bb[,cc])==F,];
    if(nrow(dd)>0){bb[rownames(bb)%in%rownames(dd),cc] <-NA; print(paste(cc," --- ",nrow(dd)," obs < ",c0))}
    cc <-"L_Feuchte"; c0 <- 100; c1 <- 0;
    dd <-bb[bb[,cc] > c0 & is.na(bb[,cc])==F,];
    if(nrow(dd)>0){bb[rownames(bb)%in%rownames(dd),cc] <-NA; print(paste(cc," --- ",nrow(dd)," obs > ",c0))}
    dd <-bb[bb[,cc] < c1 & is.na(bb[,cc])==F,];
    if(nrow(dd)>0){bb[rownames(bb)%in%rownames(dd),cc] <-NA; print(paste(cc," --- ",nrow(dd)," obs < ",c0))}
    cc <-"Nied_unb"; c0 <-500; c1 <- 0;
    dd <-bb[bb[,cc] > c0 & is.na(bb[,cc])==F,];
    if(nrow(dd)>0){bb[rownames(bb)%in%rownames(dd),cc] <-NA; print(paste(cc," --- ",nrow(dd)," obs > ",c0))}
    dd <-bb[bb[,cc] < c1 & is.na(bb[,cc])==F,];
    if(nrow(dd)>0){bb[rownames(bb)%in%rownames(dd),cc] <-NA; print(paste(cc," --- ",nrow(dd)," obs < ",c0))}
    summary(bb);
  }
  ### CONSTANTS
  {
    constants$z <-10; # hoehe windmessung
    constants$Elev <-gg@data$altitude_m; # höhe über null
    constants$lat <-gg@coords[,2];
    constants$lat_rad <-gg@coords[,2]*pi/180;
  }
  ### INPUT
  {
    cc <-data.frame(
      Year=format(bb$date,"%Y"),
      Month=format(bb$date,"%m"),
      Day=format(bb$date,"%d"),
      Hour=format(bb$date,"%H"),
      Minute=format(bb$date,"%M"),
      Temp=bb$L_Temp,
      RH=bb$L_Feuchte,
      uz=bb$W_Gesch,
      Rs=bb$G_Str,
      Precip=bb$Nied_unb
    ); # summary(cc)
    cc <-cc[cc$Minute%in%c("00"),];
    cc$Rs <-cc$Rs*0.0036; # W/m2 ->MJ/m2  
    var <-c("Year","Month","Day","Hour","Temp","RH","uz","Rs","Precip");
    clim_in <- ReadInputs(varnames=var,
                          climatedata=cc[,var], 
                          constants, 
                          stopmissing=c(10,10,3),
                          timestep = "subdaily",
                          interp_missing_days =F, 
                          interp_missing_entries =F, 
                          interp_abnormal = F, 
                          missing_method = "monthly average", 
                          abnormal_method = "monthly average")
  }
  ### ET
  {
    et_fao <-ET.PenmanMonteith(clim_in,constants,
                                    ts="daily",solar="data",wind="yes",crop="short",
                                    message="yes",AdditionalStats="no",save.csv="no");
  }
  ### table
  {
    ### ET
    dd <-data.frame(et_fao$ET.Daily);
    ee <-data.frame(date=rownames(dd),et_fao=dd[,1]);
    ff <-data.frame(clim_in$Tmax);
    gg <-data.frame(date=rownames(ff),t_max=ff[,1]);
    ee <-merge(ee,gg,by="date",all=T);
    ff <-data.frame(clim_in$Tmin);
    gg <-data.frame(date=rownames(ff),t_min=ff[,1]);
    ee <-merge(ee,gg,by="date",all=T);
    ff <-data.frame(clim_in$RHmax);
    gg <-data.frame(date=rownames(ff),rh_max=ff[,1]);
    ee <-merge(ee,gg,by="date",all=T);
    ff <-data.frame(clim_in$RHmin);
    gg <-data.frame(date=rownames(ff),rh_min=ff[,1]);
    ee <-merge(ee,gg,by="date",all=T);
    ff <-data.frame(clim_in$uz);
    gg <-data.frame(date=rownames(ff),wind=ff[,1]);
    ee <-merge(ee,gg,by="date",all=T);
    ff <-data.frame(clim_in$Rs);
    gg <-data.frame(date=rownames(ff),rad=ff[,1]);
    ee <-merge(ee,gg,by="date",all=T);
    ff <-data.frame(clim_in$Precip);
    gg <-data.frame(date=rownames(ff),precip=ff[,1]);
    ee <-merge(ee,gg,by="date",all=T);
    ## cc
    cc$date <-paste(cc$Year,cc$Month,cc$Day,sep="-");
    dd <-tapply(cc$Temp, cc$date,function(x){mean(x,na.rm=T)});
    ff <-data.frame(date=names(dd),t_mean=dd);
    ee <-merge(ee,ff,by="date",all=T);
    dd <-tapply(cc$RH, cc$date,function(x){mean(x,na.rm=T)});
    ff <-data.frame(date=names(dd),rh_mean=dd);
    ee <-merge(ee,ff,by="date",all=T);
  }
  ### save
  MM[[ll[ii]]] <-ee;
}

# PLOT years ------------------------------------------------------
G$d_temp <-file.path(G$d_out,"years"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ll <-names(MM);
ii <-1;
for(ii in 1:length(ll))
{
  aa <-MM[[ii]];
  ### name
  nam <-colnames(aa)[-1];
  kk <-1;
  for(kk in 1:length(nam))
  {
    ### names
    {
      ### nam_y
      nam_y <-nam[kk];
      if(nam_y%in%c("et_fao","precip")){nam_y <-paste0(nam_y," [mm]")}
      if(nam_y%in%c("t_max","t_min","t_mean")){nam_y <-paste0(nam_y," [°C]")}
      if(nam_y%in%c("rh_max","rh_min","rh_mean")){nam_y <-paste0(nam_y," [%]")}
      if(nam_y%in%c("rad")){nam_y <-paste0(nam_y," [MJ m-2]")}
      if(nam_y%in%c("wind")){nam_y <-paste0(nam_y," [m s-1]")}
    }
    ### data
    bb <-aa[is.na(aa$date)==F,]; bb$dat <-as.Date(bb$date); bb$wert <-bb[,nam[kk]];
    bb <-bb[,c("dat","wert")]; bb$yr <-format(bb$dat,"%Y"); bb$doy <-as.integer(format(bb$dat,"%j"));
    yr <-levels(as.factor(bb$yr));
    bb <-bb[!format(bb$dat,"%m-%d")%in%c("02-29"),]; # ignore leap years
    ### color
    cc <-colorRampPalette(c("grey10","grey90","gold","orange"));
    ccc <-cc(length(yr));
    ### limits
    t0 <-1; t1 <-356;
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
    legend("top",ncol=10,yr,col=ccc,pch=16,cex=.5)
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

# SAVE --------------------------------------------------------
out <-paste(G$n_script,".rda",sep="_");
save(MM,file = file.path(G$d_out,out));
# load(file.path(G$d_out,out))

# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")