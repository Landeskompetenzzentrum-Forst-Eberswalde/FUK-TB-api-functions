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

### plot coordinates - xy 
{
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
}

### tb values - TB
{
  list.files(G$d_in1);
  load(file.path(G$d_in1,"02_save_rda_meteo_ff_.rda")); list.files(G$d_in1);
}

### clean
suppressWarnings(rm("aa","bb","cc","dd","ee","ii","ll")); gc();


# get day  --------------------------------------------------
MM <-list(); STAT_in <-list(); STAT_out <-list();
G$d_temp <-file.path(G$d_out,"stat"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
data("constants"); # ET constants
ll <-names(TB); ll;
ll <-ll[!ll%in%c("1101_FF")]
ii <-1; 
for(ii in 1:length(ll))
{
  ### input
  aa <-TB[[ll[ii]]]; message(ll[ii]); 
  pp <-str_replace(ll[ii],"_FF",""); pp;
  xy_ <-xy[xy@data$code_plot%in%pp,];
  ### data.frame - bb
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
  ### summary bb - STAT_in
  {
    STAT_in[[ll[ii]]] <-NA; dd <-data.frame(NULL); 
    jj <-8;
    for(jj in 2:ncol(bb))
    {
      cc <-bb[,c(1,jj)]; cc <-cc[is.na(cc[,2])==F,];
      ### table
      kk <-jj-1;
      dd[kk,"var"] <-colnames(bb)[jj];
      dd[kk,"nrow"] <-nrow(cc);
      dd[kk,"start_day"] <-min(format(cc$date,"%Y-%m-%d"));
      dd[kk,"end_day"] <-max(format(cc$date,"%Y-%m-%d"));
      ### time steps 
      ee <-diff(cc$date); ff <-summary(as.factor(ee));
      ww <-as.integer(names(ff)); 
      timesteps <-c(1,5,10,15,30,60,999); uu <-1;
      for(uu in 1:length(timesteps))
        {
          
          dd[kk,paste0("p_",timesteps[uu],"min")] <-0; 
          hh <-round(as.integer(ff[names(ff)%in%timesteps[uu]])/sum(ff)*100,4); 
          if(timesteps[uu]%in%999){hh <-round(sum(ff[as.integer(names(ff))>timesteps[uu-1]])/sum(ff)*100,4)}
          if(length(hh)>0){dd[kk,paste0("p_",timesteps[uu],"min")] <-hh};
        };
      ### number of larger time steps
      dd[kk,paste0("n_miss_","1h")] <-sum(ff[as.integer(names(ff))>60 & !as.integer(names(ff))>120]); 
      dd[kk,paste0("n_miss_","2h")] <-sum(ff[as.integer(names(ff))>120 & !as.integer(names(ff))>180]); 
      dd[kk,paste0("n_miss_","3h")] <-sum(ff[as.integer(names(ff))>180 & !as.integer(names(ff))>240]); 
      dd[kk,paste0("n_miss_","999min")] <-sum(ff[as.integer(names(ff))>240]); 
      ### summary
      ee <-summary(cc[,2]);
      if(!"NA's"%in%names(ee)){dd[kk,"NA's"] <-0};
      uu <-1;
      for(uu in 1:length(ee))
      {
        ff <-names(ee)[uu];
        dd[kk,paste0("s_",tolower(ff))] <-as.numeric(ee[uu]);
      }
      ### out of range - doc & ex
      c0 <- -999; c1 <- 999;
      if(dd[kk,"var"]%in%"L_Temp"){c0 <- -60; c1 <- 60;}
      if(dd[kk,"var"]%in%"L_Temp_max"){c0 <- -60; c1 <- 60;}
      if(dd[kk,"var"]%in%"L_Temp_min"){c0 <- -60; c1 <- 60;}
      if(dd[kk,"var"]%in%"W_Gesch"){c0 <-0; c1 <- 50;}
      if(dd[kk,"var"]%in%"G_Str"){c0 <-0; c1 <- 1000;}
      if(dd[kk,"var"]%in%"L_Feuchte"){c0 <-0; c1 <- 100;}
      if(dd[kk,"var"]%in%"Nied_unb"){c0 <-0; c1 <- 500;}
      {
        ee <-colnames(bb)[jj];
        ff <-bb[bb[,ee] < c0 & is.na(bb[,ee])==F,];
        if(nrow(ff)>0){bb[rownames(bb)%in%rownames(dd),ee] <-NA; print(paste(ee," --- ",nrow(ff)," obs > ",c0))}
        dd[kk,paste0("t_",0)]  <-c0; dd[kk,paste0("t_n_",0)] <-nrow(ff);
        ff <-bb[bb[,ee] > c1 & is.na(bb[,ee])==F,];
        if(nrow(ff)>0){bb[rownames(bb)%in%rownames(dd),ee] <-NA; print(paste(ee," --- ",nrow(ff)," obs > ",c1))}
        dd[kk,paste0("t_",1)]  <-c1; dd[kk,paste0("t_n_",1)] <-nrow(ff);
      }
      ###
    }
    STAT_in[[ll[ii]]] <-dd;
    ### save
    out <-paste(G$n_script,"STAT_in",ll[ii],".csv",sep="_");
    write.table(dd,file = file.path(G$d_temp,out),sep=";",dec=",",na="",row.names = F,col.names = T);
  }
  ### ET - constants
  {
    constants$z <-10; # hoehe windmessung
    constants$Elev <-xy_@data$altitude_m; # höhe über null
    constants$lat <-xy_@coords[,2];
    constants$lat_rad <-xy_@coords[,2]*pi/180;
  }
  ### ET input - cc
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
  ### ET - et_fao
  {
    et_fao <-ET.PenmanMonteith(clim_in,constants,
                                    ts="daily",solar="data",wind="yes",crop="short",
                                    message="yes",AdditionalStats="no",save.csv="no");
  }
  ### data.frame - ee
  {
    ### ET.Daily
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
    ## mean temp & rh
    cc$date <-paste(cc$Year,cc$Month,cc$Day,sep="-");
    dd <-tapply(cc$Temp, cc$date,function(x){mean(x,na.rm=T)});
    ff <-data.frame(date=names(dd),t_mean=dd);
    ee <-merge(ee,ff,by="date",all=T);
    dd <-tapply(cc$RH, cc$date,function(x){mean(x,na.rm=T)});
    ff <-data.frame(date=names(dd),rh_mean=dd);
    ee <-merge(ee,ff,by="date",all=T);
    ### date
    ee$date <-as.Date(ee$date);
  }
  ### summary ee - STAT_out
  {
    STAT_out[[ll[ii]]] <-NA; dd <-data.frame(NULL); 
    ee$year <-format(ee$date,"%Y");
    ff <-data.frame(year=levels(as.factor(ee$year)));
    kk <-2;
    for(kk in 2:ncol(ee))
    {
      if(kk==ncol(ee)){next}
      cc <-tapply(ee[,kk], ee$year, function(x){length(na.omit(x))});
      gg <-data.frame(year=names(cc),n=as.integer(cc))
      colnames(gg)[2] <-colnames(ee)[kk]
      ff <-merge(ff,gg,by="year");
    }
    STAT_out[[ll[ii]]] <-ff; 
    ### save - STAT_out
    # out <-paste(G$n_script,"STAT_out",ll[ii],".csv",sep="_");
    # write.table(ff,file = file.path(G$d_temp,out),sep=";",dec=",",na="",row.names = F,col.names = T);
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
      out <-file.path(G$d_temp,paste("STAT_out",ll[ii],".png",sep="_"));
      png(out, units="mm", width=160, height=y_max*10, res=300);
      par(mar=c(4,4,2,0),mgp=c(1,1,0),lab=c(5,5,7)); # par()
      ### base
      plot(c(y_min,y_max)~c(x_min,x_max),
           col="white",xlim=c(x_min,x_max+1),ylim=c(y_min-1,y_max),
           xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
           main=ll[ii])
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
          if(gg%in%c(365,366)){hh <-"blue3"}
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
G$d_temp <-file.path(G$d_out,"years"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
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
out <-paste(G$n_script,"MM.rda",sep="_");
save(MM,file = file.path(G$d_out,out));
out <-paste(G$n_script,"stat_in.rda",sep="_");
save(STAT_in,file = file.path(G$d_out,out));
out <-paste(G$n_script,"stat_out.rda",sep="_");
save(STAT_out,file = file.path(G$d_out,out));
# load(file.path(G$d_out,out))

# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")