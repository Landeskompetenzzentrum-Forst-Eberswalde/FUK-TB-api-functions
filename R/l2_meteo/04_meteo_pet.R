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
G$d_in1 <-file.path(G$d_home,"output/rda"); list.files(G$d_in1);
G$d_out <-file.path(G$d_home,"output"); if(!dir.exists(G$d_out)){dir.create(G$d_out)};
### end
print(G)

# LOAD data -------------------------------------------------------------------------

### Mh
list.files(G$d_in1);
load(file.path(G$d_in1,"01_meteo_hour_Mh.rda")); list.files(G$d_in1);

### ET constants
data("constants"); 

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

# GET day  --------------------------------------------------
PET <-list();
G$d_temp <-file.path(G$d_out,paste(G$n_script)); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ll <-names(Mh); ll; # ll <-ll[!ll%in%c("1101_FF")]
ii <-1; 
for(ii in 1:length(ll))
{
  ### input
  aa <-Mh[[ll[ii]]]; message(ll[ii]); 
  pp <-str_replace(ll[ii],"_FF",""); pp;
  xy_ <-xy[xy@data$code_plot%in%pp,];
  ### exclude data
  {
    if(ll[ii]%in%c("1101_FF"))
    {
      ### missing wind in 2023-2024
      dd <-cc[cc$date > as.POSIXct("2023-02-03 23:00:00") & cc$date < as.POSIXct("2023-08-29 00:00:00"),];
      ee <-cc[cc$date > as.POSIXct("2024-03-13 23:00:00") & cc$date < as.POSIXct("2024-11-28 00:00:00"),];
      ff <-rbind(dd,ee);
      cc <-cc[!rownames(cc)%in%rownames(ff),];
      # dd <-cc[is.na(cc$W_Gesch),]
    }
  }
  ### ET - clim_in
  {
    dd <-data.frame(
      Year=format(cc$date,"%Y"),
      Month=format(cc$date,"%m"),
      Day=format(cc$date,"%d"),
      Hour=format(cc$date,"%H"),
      Temp=cc$L_Temp,
      RH=cc$L_Feuchte,
      uz=cc$W_Gesch,
      Rs=cc$G_Str,
      Precip=cc$Nied_unb
    ); # summary(dd)
    ### variables
    var <-colnames(dd); # summary(dd$Rs)
    dd$Rs <-(dd$Rs*(60*60*24))/1e6; # W/m2 ->MJ/m2  
    # https://stackoverflow.com/questions/40826466/solar-energy-conversion-w-m2-to-mj-m2
    ### ReadInputs
    clim_in <- ReadInputs(varnames=var,
                          climatedata=dd[,var], 
                          constants, 
                          stopmissing=c(10,10,3),
                          timestep = "subdaily",
                          interp_missing_days =F, 
                          interp_missing_entries =F, 
                          interp_abnormal = F, 
                          missing_method = "monthly average", 
                          abnormal_method = "monthly average")
  }
  ### ET - et_fao
  {
    constants$z <-10; # hoehe windmessung
    constants$Elev <-xy_@data$altitude_m; # höhe über null
    constants$lat <-xy_@coords[,2];
    constants$lat_rad <-xy_@coords[,2]*pi/180;
    et_fao <-ET.PenmanMonteith(clim_in,constants,
                                    ts="daily",solar="data",wind="yes",crop="short",
                                    message="yes",AdditionalStats="no",save.csv="no");
  }
  ### data.frame - ee
  {
    ### ET.Daily
    hh <-data.frame(et_fao$ET.Daily);
    ee <-data.frame(date=rownames(hh),et_fao=hh[,1]);
    ff <-data.frame(clim_in$Tmax);
    gg <-data.frame(date=rownames(ff),t_max=ff[,1]);
    ee <-merge(ee,gg,by="date",all.x=T);
    ff <-data.frame(clim_in$Tmin);
    gg <-data.frame(date=rownames(ff),t_min=ff[,1]);
    ee <-merge(ee,gg,by="date",all.x=T);
    ff <-data.frame(clim_in$RHmax);
    gg <-data.frame(date=rownames(ff),rh_max=ff[,1]);
    ee <-merge(ee,gg,by="date",all=T);
    ff <-data.frame(clim_in$RHmin);
    gg <-data.frame(date=rownames(ff),rh_min=ff[,1]);
    ee <-merge(ee,gg,by="date",all.x=T);
    ff <-data.frame(clim_in$uz);
    gg <-data.frame(date=rownames(ff),wind=ff[,1]);
    ee <-merge(ee,gg,by="date",all.x=T);
    ff <-data.frame(clim_in$Rs);
    gg <-data.frame(date=rownames(ff),rad=ff[,1]);
    ee <-merge(ee,gg,by="date",all.x=T);
    ff <-data.frame(clim_in$Precip);
    gg <-data.frame(date=rownames(ff),precip=ff[,1]);
    ee <-merge(ee,gg,by="date",all.x=T);
    ## mean temp & rh
    cc$day <-as.Date(cc$date,tz="");
    dd <-tapply(cc$L_Temp, cc$date,function(x){mean(x,na.rm=T)});
    ff <-data.frame(date=names(dd),obs=dd); colnames(ff)[2] <-"t_mean";
    ee <-merge(ee,ff,by="date",all.x=T);
    dd <-tapply(cc$L_Feuchte, cc$date,function(x){mean(x,na.rm=T)});
    ff <-data.frame(date=names(dd),obs=dd); colnames(ff)[2] <-"rh_mean";
    ee <-merge(ee,ff,by="date",all.x=T);
    ### date
    ee$date <-as.Date(ee$date,tz="");
  }
  ### STAT_out_DOY
  {
    STAT_out_DOY[[ll[ii]]] <-NA; dd <-data.frame(NULL); 
    ee$year <-format(ee$date,"%Y");
    ff <-data.frame(year=levels(as.factor(ee$year)));
    kk <-2;
    for(kk in 2:ncol(ee))
    {
      if(kk==ncol(ee)){next}
      cc <-tapply(ee[,kk], ee$year, function(x){length(x[is.na(x)==F])});
      gg <-data.frame(year=names(cc),n=as.integer(cc))
      colnames(gg)[2] <-colnames(ee)[kk]
      ff <-merge(ff,gg,by="year");
    }
    STAT_out_DOY[[ll[ii]]] <-ff; 
    ### plot
    {
      ### limit - STAT_out
      x_min <-1; x_max <-ncol(ff)-1;
      y_min <-1; y_max <-nrow(ff)
      ### color
      cc <-colorRampPalette(c("red3","orange","green4"));
      ccc <-cc(367);
      ### window
      graphics.off();
      out <-file.path(G$d_temp,paste("STAT_out_DOY",ll[ii],".png",sep="_"));
      png(out, units="mm", width=160, height=y_max*10, res=300);
      par(mar=c(4,4,2,0),mgp=c(1,1,0),lab=c(5,5,7)); # par()
      ### base
      plot(c(y_min,y_max)~c(x_min,x_max),
           col="white",xlim=c(x_min,x_max+1),ylim=c(y_min-1,y_max),
           xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
           main=paste("Anzahl an Tage pro Jahr und Variable - ",ll[ii]))
      ### axis
      text(x=c(x_min:x_max)+0.5,y=par("usr")[3]-0.45,labels=colnames(ff)[-1],srt=90,xpd=NA)
      text(x=par("usr")[1]-0.45,y=c(y_min:y_max)-0.5,labels=ff[,1],srt=0,xpd=NA)
      ### rect
      kk <-2;
      for(kk in 2:ncol(ff))
      {
        oo <-1;
        for(oo in 1:y_max)
        {
          gg <-ff[oo,kk]; hh <-ccc[gg+1]; 
          if(ff[oo,1]%in%seq(1900,2100,by=4) & gg%in%366){hh <-"blue3"}
          if(!ff[oo,1]%in%seq(1900,2100,by=4) & gg%in%365){hh <-"blue3"}
          rect(kk-1,oo-1,kk,oo,col=hh,border=hh);
          text(kk-0.5,oo-0.5,labels=gg,col="black");
        }
      }
      ### save
      graphics.off();
    }
  }
  ### STAT_out_MIN
  {
    STAT_out_MIN[[ll[ii]]] <-NA; dd <-data.frame(NULL); 
    ee$year <-format(ee$date,"%Y");
    ff <-data.frame(year=levels(as.factor(ee$year)));
    hh <-colnames(ee); hh <-hh[!hh%in%c("date","year")]
    kk <-1;
    for(kk in 1:length(hh))
    {
      rr <-hh[kk];
      cc <-tapply(ee[,rr], ee$year, function(x){min(x,na.rm=T)});
      gg <-data.frame(year=names(cc),n=round(as.numeric(cc),1)); colnames(gg)[2] <-rr;
      ff <-merge(ff,gg,by="year");
    }
    STAT_out_MIN[[ll[ii]]] <-ff; 
    ### plot
    {
      ### limit - STAT_out
      x_min <-1; x_max <-ncol(ff)-1;
      y_min <-1; y_max <-nrow(ff)
      ### color
      cc <-colorRampPalette(c("blue3","grey60"));
      ccc <-cc(100);
      ### window
      graphics.off();
      out <-file.path(G$d_temp,paste("STAT_out_MIN",ll[ii],".png",sep="_"));
      png(out, units="mm", width=160, height=y_max*10, res=300);
      par(mar=c(4,4,2,0),mgp=c(1,1,0),lab=c(5,5,7)); # par()
      ### base
      plot(c(y_min,y_max)~c(x_min,x_max),
           col="white",xlim=c(x_min,x_max+1),ylim=c(y_min-1,y_max),
           xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
           main=paste("(Tages-)Minimum pro Jahr und Variable - ",ll[ii]))
      ### axis
      text(x=c(x_min:x_max)+0.5,y=par("usr")[3]-0.45,labels=colnames(ff)[-1],srt=90,xpd=NA)
      text(x=par("usr")[1]-0.45,y=c(y_min:y_max)-0.5,labels=ff[,1],srt=0,xpd=NA)
      ### rect
      kk <-3;
      for(kk in 2:ncol(ff))
      {
        oo <-1;
        for(oo in 1:y_max)
        {
          gg <-ff[oo,kk];
          rr <-floor(rank(ff[,kk])/max(rank(ff[,kk]))*100); print(rr)
          hh <-ccc[rr[oo]];
          rect(kk-1,oo-1,kk,oo,col=hh,border=hh);
          text(kk-0.5,oo-0.5,labels=gg,col="black",cex=.8);
        }
      }
      ### save
      graphics.off();
    }
  }
  ### STAT_out_MAX
  {
    STAT_out_MIN[[ll[ii]]] <-NA; dd <-data.frame(NULL); 
    ee$year <-format(ee$date,"%Y");
    ff <-data.frame(year=levels(as.factor(ee$year)));
    hh <-colnames(ee); hh <-hh[!hh%in%c("date","year")]
    kk <-1;
    for(kk in 1:length(hh))
    {
      rr <-hh[kk];
      cc <-tapply(ee[,rr], ee$year, function(x){min(x,na.rm=T)});
      gg <-data.frame(year=names(cc),n=round(as.numeric(cc),1)); colnames(gg)[2] <-rr;
      ff <-merge(ff,gg,by="year");
    }
    STAT_out_MIN[[ll[ii]]] <-ff; 
    ### plot
    {
      ### limit - STAT_out
      x_min <-1; x_max <-ncol(ff)-1;
      y_min <-1; y_max <-nrow(ff)
      ### color
      cc <-colorRampPalette(c("grey60","red3"));
      ccc <-cc(100);
      ### window
      graphics.off();
      out <-file.path(G$d_temp,paste("STAT_out_MAX",ll[ii],".png",sep="_"));
      png(out, units="mm", width=160, height=y_max*10, res=300);
      par(mar=c(4,4,2,0),mgp=c(1,1,0),lab=c(5,5,7)); # par()
      ### base
      plot(c(y_min,y_max)~c(x_min,x_max),
           col="white",xlim=c(x_min,x_max+1),ylim=c(y_min-1,y_max),
           xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
           main=paste("(Tages-)Maximum pro Jahr und Variable - ",ll[ii]))
      ### axis
      text(x=c(x_min:x_max)+0.5,y=par("usr")[3]-0.45,labels=colnames(ff)[-1],srt=90,xpd=NA)
      text(x=par("usr")[1]-0.45,y=c(y_min:y_max)-0.5,labels=ff[,1],srt=0,xpd=NA,cex=.8)
      ### rect
      kk <-3;
      for(kk in 2:ncol(ff))
      {
        oo <-1;
        for(oo in 1:y_max)
        {
          gg <-ff[oo,kk];
          rr <-floor(rank(ff[,kk])/max(rank(ff[,kk]))*100); print(rr)
          hh <-ccc[rr[oo]];
          rect(kk-1,oo-1,kk,oo,col=hh,border=hh);
          text(kk-0.5,oo-0.5,labels=gg,col="black");
        }
      }
      ### save
      graphics.off();
    }
  }
  ### STAT_out_MEANSUM
  {
    STAT_out_MIN[[ll[ii]]] <-NA; dd <-data.frame(NULL); 
    ee$year <-format(ee$date,"%Y");
    ff <-data.frame(year=levels(as.factor(ee$year)));
    hh <-colnames(ee); hh <-hh[!hh%in%c("date","year")]
    kk <-8;
    for(kk in 1:length(hh))
    {
      rr <-hh[kk]; 
      if(rr%in%c("precip","et_fao")){cc <-tapply(ee[,rr], ee$year, function(x){sum(x,na.rm=T)})}
      if(!rr%in%c("precip","et_fao")){cc <-tapply(ee[,rr], ee$year, function(x){mean(x,na.rm=T)})}
      gg <-data.frame(year=names(cc),n=round(as.numeric(cc),1)); colnames(gg)[2] <-rr;
      ff <-merge(ff,gg,by="year");
    }
    STAT_out_MIN[[ll[ii]]] <-ff; 
    ### plot
    {
      ### limit - STAT_out
      x_min <-1; x_max <-ncol(ff)-1;
      y_min <-1; y_max <-nrow(ff)
      ### color
      cc <-colorRampPalette(c("grey60","green3"));
      ccc <-cc(100);
      ### window
      graphics.off();
      out <-file.path(G$d_temp,paste("STAT_out_MEANSUM",ll[ii],".png",sep="_"));
      png(out, units="mm", width=160, height=y_max*10, res=300);
      par(mar=c(4,4,2,0),mgp=c(1,1,0),lab=c(5,5,7)); # par()
      ### base
      plot(c(y_min,y_max)~c(x_min,x_max),
           col="white",xlim=c(x_min,x_max+1),ylim=c(y_min-1,y_max),
           xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
           main=paste("Mittel/Summe je Variable - ",ll[ii]))
      ### axis
      text(x=c(x_min:x_max)+0.5,y=par("usr")[3]-0.45,labels=colnames(ff)[-1],srt=90,xpd=NA)
      text(x=par("usr")[1]-0.45,y=c(y_min:y_max)-0.5,labels=ff[,1],srt=0,xpd=NA,cex=.8)
      ### rect
      kk <-3;
      for(kk in 2:ncol(ff))
      {
        oo <-1;
        for(oo in 1:y_max)
        {
          gg <-ff[oo,kk];
          rr <-floor(rank(ff[,kk])/max(rank(ff[,kk]))*100); print(rr)
          hh <-ccc[rr[oo]];
          rect(kk-1,oo-1,kk,oo,col=hh,border=hh);
          text(kk-0.5,oo-0.5,labels=gg,col="black");
        }
      }
      ### save
      graphics.off();
    }
  }
  ### save
  MM[[ll[ii]]] <-ee;
}

# PLOT years ------------------------------------------------------
G$d_temp <-file.path(G$d_out,paste(G$n_script,"years",sep="-")); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ll <-names(MM);
ii <-1;
for(ii in 1:length(ll))
{
  aa <-MM[[ii]];
  ### name
  nam <-colnames(aa)[!colnames(aa)%in%c("date","year")];
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

# SAVE --------------------------------------------------------
G$d_temp <-file.path(G$d_out,"rda"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
out <-paste(G$n_script,"MM.rda",sep="_");
save(MM,file = file.path(G$d_temp,out));
out <-paste(G$n_script,"stat_in.rda",sep="_");
save(STAT_in,file = file.path(G$d_temp,out));
out <-paste(G$n_script,"stat_out_DOY.rda",sep="_");
save(STAT_out_DOY,file = file.path(G$d_temp,out));
out <-paste(G$n_script,"stat_out_MIN.rda",sep="_");
save(STAT_out_MIN,file = file.path(G$d_temp,out));
out <-paste(G$n_script,"stat_out_MAX.rda",sep="_");
save(STAT_out_MAX,file = file.path(G$d_temp,out));
# load(file.path(G$d_out,out))

# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")