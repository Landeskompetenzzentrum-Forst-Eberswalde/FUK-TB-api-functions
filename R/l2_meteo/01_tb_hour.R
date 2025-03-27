# 
# load TB rda from open field stations (_FF)  
# table: number of observations, time steps, missing values, thresholds
# aggregate hours
# plot year~variables: nHOUR, min, max, meansum 
# save rda
# 

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr")
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
G$d_out <-file.path(G$d_home,"output"); if(!dir.exists(G$d_out)){dir.create(G$d_out)};
### end
print(G)

# LOAD data -------------------------------------------------------------------------
list.files(G$d_in1);
load(file.path(G$d_in1,"02_save_rda_meteo_ff_.rda")); list.files(G$d_in1);

# Meteo hour - Mh  --------------------------------------------------
Mh <-list(); STAT_in <-list(); 
STAT_out_nHOUR <-list(); STAT_out_MIN <-list(); STAT_out_MAX <-list();  STAT_out_MEANSUM <-list();
G$d_temp <-file.path(G$d_out,paste(G$n_script,sep="-")); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ll <-names(TB); ll; # ll <-ll[!ll%in%c("1101_FF")]
ii <-1; 
for(ii in 1:length(ll))
{
  ### input
  aa <-TB[[ll[ii]]]; message(ll[ii]); 
  aa <-aa[!names(aa)%in%c("Datum","ts","UB","Betriebsspannung","Freier Speicher",
                          "General State","Run State","Error State","Performance Reserve")];
  aa <-aa[str_detect(names(aa),"^ ")==F]; aa <-aa[str_detect(names(aa),"^Analog")==F];
  ### data - bb
  {
    bb <-data.frame(aa[1]);  #names(aa);
    colnames(bb) <-c("date",str_sub(colnames(bb)[2],1,-7)); bb[bb[,2]%in%9999,2] <-NA;
    jj <-2;
    for(jj in 2:length(aa))
    {
      print(paste(jj," of ",length(aa)))
      cc <-data.frame(aa[jj]); 
      colnames(cc) <-c("date",str_sub(colnames(cc)[2],1,-7)); cc[cc[,2]%in%9999,2] <-NA;
      bb <-merge(bb,cc,by="date",all=T);
    }
  }
  ### STAT_in
  {
    STAT_in[[ll[ii]]] <-NA; dd <-data.frame(NULL); 
    jj <-8;
    for(jj in 2:ncol(bb))
    {
      cc <-bb[,c(1,jj)]; cc <-cc[is.na(cc[,2])==F,];
      ### table
      {
        kk <-jj-1;
        dd[kk,"var"] <-colnames(bb)[jj];
        dd[kk,"nrow"] <-nrow(cc);
        dd[kk,"start_day"] <-min(format(cc$date,"%Y-%m-%d"));
        dd[kk,"end_day"] <-max(format(cc$date,"%Y-%m-%d"));
      }
      ### time steps 
      {
        ee <-diff(as.integer(cc$date))/60; ff <-summary(as.factor(ee));
        ww <-as.integer(names(ff)); 
        timesteps <-c(1,5,10,15,30,60,999); uu <-1;
        for(uu in 1:length(timesteps))
        {
          
          dd[kk,paste0("p_",timesteps[uu],"min")] <-0; 
          hh <-round(as.integer(ff[names(ff)%in%timesteps[uu]])/sum(ff)*100,4); 
          if(timesteps[uu]%in%999){hh <-round(sum(ff[as.integer(names(ff))>timesteps[uu-1]])/sum(ff)*100,4)}
          if(length(hh)>0){dd[kk,paste0("p_",timesteps[uu],"min")] <-hh};
        };
      }
      ### number of larger time steps
      {
        dd[kk,paste0("n_miss_","1h")] <-sum(ff[as.integer(names(ff))>60 & !as.integer(names(ff))>120]); 
        dd[kk,paste0("n_miss_","2h")] <-sum(ff[as.integer(names(ff))>120 & !as.integer(names(ff))>180]); 
        dd[kk,paste0("n_miss_","3h")] <-sum(ff[as.integer(names(ff))>180 & !as.integer(names(ff))>240]); 
        dd[kk,paste0("n_miss_","999min")] <-sum(ff[as.integer(names(ff))>240]); 
      }
      ### doc & ex threshold
      {
        c0 <- 0; c1 <- 999;
        if(dd[kk,"var"]%in%"B_Temp"){c0 <- -60; c1 <- 60;}
        if(dd[kk,"var"]%in%"B_Temp_max"){c0 <- -60; c1 <- 60;}
        if(dd[kk,"var"]%in%"B_Temp_min"){c0 <- -60; c1 <- 60;}
        if(dd[kk,"var"]%in%"G_Str"){c0 <-0; c1 <- 1100;}
        if(dd[kk,"var"]%in%"G_Str_min"){c0 <-0; c1 <- 1100;}
        if(dd[kk,"var"]%in%"G_Str_max"){c0 <-0; c1 <- 1100;}
        if(dd[kk,"var"]%in%"L_Feuchte"){c0 <-0; c1 <- 100;}
        if(dd[kk,"var"]%in%"L_Feuchte_min"){c0 <-0; c1 <- 100;}
        if(dd[kk,"var"]%in%"L_Feuchte_max"){c0 <-0; c1 <- 100;}
        if(dd[kk,"var"]%in%"L_Druck"){c0 <- -60; c1 <- 1100;}
        if(dd[kk,"var"]%in%"L_Druck_max"){c0 <- -60; c1 <- 1100;}
        if(dd[kk,"var"]%in%"L_Druck_min"){c0 <- -60; c1 <- 1100;}
        if(dd[kk,"var"]%in%"L_Temp"){c0 <- -60; c1 <- 60;}
        if(dd[kk,"var"]%in%"L_Temp_max"){c0 <- -60; c1 <- 60;}
        if(dd[kk,"var"]%in%"L_Temp_min"){c0 <- -60; c1 <- 60;}
        if(dd[kk,"var"]%in%"Nied_unb"){c0 <-0; c1 <- 500;}
        if(dd[kk,"var"]%in%"Nied_Pluvio"){c0 <-0; c1 <- 500;}
        if(dd[kk,"var"]%in%"W_Gesch"){c0 <-0; c1 <- 50;}
        if(dd[kk,"var"]%in%"W_Gesch_min"){c0 <-0; c1 <- 50;}
        if(dd[kk,"var"]%in%"W_Gesch_max"){c0 <-0; c1 <- 50;}
        {
          ee <-colnames(bb)[jj];
          ff <-bb[bb[,ee] < c0 & is.na(bb[,ee])==F,];
          if(nrow(ff)>0){bb[rownames(bb)%in%rownames(ff),ee] <-NA; print(paste(ee," --- ",nrow(ff)," obs > ",c0))}
          dd[kk,"t0"]  <-c0; dd[kk,"t0n"] <-nrow(ff);
          ff <-bb[bb[,ee] > c1 & is.na(bb[,ee])==F,];
          if(nrow(ff)>0){bb[rownames(bb)%in%rownames(ff),ee] <-NA; print(paste(ee," --- ",nrow(ff)," obs > ",c1))}
          dd[kk,"t1"]  <-c1; dd[kk,"t1n"] <-nrow(ff);
        }
      }
    }
    ### save
    dd <-dd[order(dd$var),]; STAT_in[[ll[ii]]] <-dd;
    out <-paste(G$n_script,"STAT_in",ll[ii],".csv",sep="_");
    write.table(dd,file = file.path(G$d_temp,out),sep=";",dec=",",na="",row.names = F,col.names = T);
  }
  ### hour - dd
  {
    bb$hour <-format(bb$date,"%Y-%m-%d %H"); dd <-bb$hour;
    cc <-data.frame(date=dd[duplicated(dd)==F]);
    rr <-colnames(bb); rr <-rr[!rr%in%c("date","hour")]; rr <-sort(rr); jj <-3;
    for(jj in 1:length(rr))
    {
      if(str_detect(rr[jj],"^Nied")){dd <-tapply(bb[,rr[jj]],bb$hour, function(x){sum(x,na.rm=T)})};
      if(str_detect(rr[jj],"^Nied")==F){dd <-tapply(bb[,rr[jj]],bb$hour, function(x){mean(x,na.rm=T)})};
      if(str_detect(rr[jj],"max$")){dd <-tapply(bb[,rr[jj]],bb$hour, function(x){max(x,na.rm=T)})};
      if(str_detect(rr[jj],"min$")){dd <-tapply(bb[,rr[jj]],bb$hour, function(x){min(x,na.rm=T)})};
      ff <-data.frame(date=names(dd),n=as.numeric(dd)); colnames(ff)[2] <-rr[jj];
      ff[is.infinite(ff[,2]) | is.nan(ff[,2]),2] <-NA;
      cc <-merge(cc,ff,by="date",all=T);
    }
    cc$date <-as.POSIXct(paste0(cc$date,":00:00"),"%Y-%m-%d %H:%M:%S",tz="");
    bb <-bb[,!colnames(bb)%in%c("hour")];
    dd <-cc;
    # summary(cc)
  }
  ### STAT_out
  {
    STAT_out_nHOUR[[ll[ii]]] <-NA; STAT_out_MIN[[ll[ii]]] <-NA; STAT_out_MAX[[ll[ii]]] <-NA; STAT_out_MEANSUM[[ll[ii]]] <-NA;
    dd$year <-format(dd$date,"%Y");
    qq <-c("nhour","min","max","meansum"); tt <-3;
    for(tt in 1:length(qq))
    {
      ff <-data.frame(year=levels(as.factor(dd$year)));
      rr <-colnames(dd); rr <-rr[!rr%in%c("date","year")]; rr <-sort(rr); jj <-3;
      for(jj in 1:length(rr))
      {
        if(qq[tt]%in%"nhour"){cc <-tapply(dd[,rr[jj]], dd$year, function(x){length(x[is.na(x)==F])})};
        if(qq[tt]%in%"min"){cc <-tapply(dd[,rr[jj]], dd$year, function(x){min(x,na.rm=T)})};
        if(qq[tt]%in%"max"){cc <-tapply(dd[,rr[jj]], dd$year, function(x){max(x,na.rm=T)})};
        if(qq[tt]%in%"meansum")
        {
          if(str_detect(rr[jj],"^Nied")){cc <-tapply(dd[,rr[jj]], dd$year, function(x){sum(x,na.rm=T)})};
          if(str_detect(rr[jj],"^Nied")==F){cc <-tapply(dd[,rr[jj]], dd$year, function(x){mean(x,na.rm=T)})};
        };
        gg <-data.frame(year=names(cc),n=round(as.numeric(cc),1)); colnames(gg)[2] <-rr[jj];
        ff <-merge(ff,gg,by="year");
      }
      ### save
      ff <-ff[order(ff$year,decreasing = T),]; 
      if(qq[tt]%in%"nhour"){STAT_out_nHOUR[[ll[ii]]] <-ff};
      if(qq[tt]%in%"min"){STAT_out_MIN[[ll[ii]]] <-ff};
      if(qq[tt]%in%"max"){STAT_out_MAX[[ll[ii]]] <-ff};
      if(qq[tt]%in%"meansum"){STAT_out_MEANSUM[[ll[ii]]] <-ff};
      ### plot
      {
        ff <-ff[order(ff$year,decreasing = F),]; 
        ### limit - STAT_out
        x_min <-1; x_max <-ncol(ff)-1;
        y_min <-1; y_max <-nrow(ff)
        ### color
        if(qq[tt]%in%"nhour"){cc <-colorRampPalette(c("red3","orange","green4")); ccc <-cc(367*24)};
        if(qq[tt]%in%c("min")){cc <-colorRampPalette(c("gray60","blue3")); ccc <-cc(100)};
        if(qq[tt]%in%c("max")){cc <-colorRampPalette(c("gray60","red3")); ccc <-cc(100)};
        if(qq[tt]%in%c("meansum")){cc <-colorRampPalette(c("gray60","green3")); ccc <-cc(100)};
        ### window
        {
          graphics.off();
          if(qq[tt]%in%"nhour"){out <-file.path(G$d_temp,paste("STAT_out_nHOUR",ll[ii],".png",sep="_"))};
          if(qq[tt]%in%"min"){out <-file.path(G$d_temp,paste("STAT_out_MIN",ll[ii],".png",sep="_"))};
          if(qq[tt]%in%"max"){out <-file.path(G$d_temp,paste("STAT_out_MAX",ll[ii],".png",sep="_"))};
          if(qq[tt]%in%"meansum"){out <-file.path(G$d_temp,paste("STAT_out_MEANSUM",ll[ii],".png",sep="_"))};
          png(out, units="mm", width=x_max*10, height=y_max*10, res=300);
        }
        ### base
        par(mar=c(6,2,2,0),mgp=c(3,1,0),lab=c(5,5,7)); 
        plot(c(y_min,y_max)~c(x_min,x_max),
             col="white",xlim=c(x_min,x_max+1),ylim=c(y_min-1,y_max),
             xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
        ### axis
        if(qq[tt]%in%"nhour"){text(x=x_max/2,y=par("usr")[4]+0.5,labels=paste("Anzahl an Fehlstunden - ",ll[ii]),srt=0,xpd=NA,cex=1.5)};   
        if(qq[tt]%in%"min"){text(x=x_max/2,y=par("usr")[4]+0.5,labels=paste("Minumum Stundenwerte - ",ll[ii]),srt=0,xpd=NA,cex=1.5)};        
        if(qq[tt]%in%"max"){text(x=x_max/2,y=par("usr")[4]+0.5,labels=paste("Maximum Stundenwerte  - ",ll[ii]),srt=0,xpd=NA,cex=1.5)};        
        if(qq[tt]%in%"meansum"){text(x=x_max/2,y=par("usr")[4]+0.5,labels=paste("MeanSum Stundenwerte - ",ll[ii]),srt=0,xpd=NA,cex=1.5)};        
        text(x=c(x_min:x_max)+0.5,y=par("usr")[3]-1.5,labels=colnames(ff)[-1],srt=90,xpd=NA)
        text(x=par("usr")[1]-0.45,y=c(y_min:y_max)-0.5,labels=ff[,1],srt=0,xpd=NA)
        ### rect
        kk <-2;
        for(kk in 2:ncol(ff))
        {
          oo <-5;
          for(oo in 1:y_max)
          {
            gg <-ff[oo,kk]; 
            if(qq[tt]%in%"nhour")
            {
              hh <-ccc[gg+1]; 
              if(ff[oo,1]%in%seq(1900,2100,by=4)){gg <-c(366*24)-gg}
              if(!ff[oo,1]%in%seq(1900,2100,by=4)){gg <-c(365*24)-gg}
              if(gg%in%0){hh <-"blue3"}
            }
            if(qq[tt]%in%c("min","max","meansum"))
            {
              rr <-floor(rank(ff[,kk])/max(rank(ff[,kk]))*100); print(rr)
              hh <-ccc[rr[oo]];
            }
            if(is.na(gg) | is.nan(gg) | is.infinite(gg)){hh <-"white"; gg<-""}
            rect(kk-1,oo-1,kk,oo,col=hh,border=hh);
            text(kk-0.5,oo-0.5,labels=gg,col="black",cex=0.5);
            ### end oo
          }
          ### end kk
        }
        ### save
        graphics.off();
      }
      ### end qq
    }
  }
  ### save
  Mh[[ll[ii]]] <-dd;
  ### end ii
}


# SAVE rda --------------------------------------------------------
G$d_temp <-file.path(G$d_out,"rda"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};

out <-paste(G$n_script,"Mh.rda",sep="_");
save(Mh,file = file.path(G$d_temp,out));

out <-paste(G$n_script,"stat_in.rda",sep="_");
save(STAT_in,file = file.path(G$d_temp,out));

out <-paste(G$n_script,"stat_out_nHOUR.rda",sep="_");
save(STAT_out_nHOUR,file = file.path(G$d_temp,out));

out <-paste(G$n_script,"stat_out_MIN.rda",sep="_");
save(STAT_out_MIN,file = file.path(G$d_temp,out));

out <-paste(G$n_script,"stat_out_MAX.rda",sep="_");
save(STAT_out_MAX,file = file.path(G$d_temp,out));

out <-paste(G$n_script,"stat_out_MEANSUM.rda",sep="_");
save(STAT_out_MAX,file = file.path(G$d_temp,out));

# load(file.path(G$d_out,out))

# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")