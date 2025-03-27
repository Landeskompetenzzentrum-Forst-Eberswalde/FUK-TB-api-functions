# 
# read hourly rda
# aggregate days
# plot year~variables: nDAY, min, max, meansum 
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
G$d_in1 <-file.path(G$d_home,"output/rda"); list.files(G$d_in1);
G$d_out <-file.path(G$d_home,"output"); if(!dir.exists(G$d_out)){dir.create(G$d_out)};
### end
print(G)

# LOAD data -------------------------------------------------------------------------
list.files(G$d_in1);
load(file.path(G$d_in1,"01_tb_hour_Mh.rda")); list.files(G$d_in1);


# Meteo day - Md  --------------------------------------------------
Md <-list();
STAT_out_nDAY <-list(); STAT_out_MIN <-list(); STAT_out_MAX <-list();  STAT_out_MEANSUM <-list();
G$d_temp <-file.path(G$d_out,paste(G$n_script,sep="-")); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ll <-names(Mh); ll; # ll <-ll[!ll%in%c("1101_FF")]
ii <-1; 
for(ii in 1:length(ll))
{
  ### input
  bb <-Mh[[ll[ii]]]; message(ll[ii]); 
  ### day - dd
  {
    bb$day <-format(bb$date,"%Y-%m-%d"); dd <-bb$day;
    cc <-data.frame(date=dd[duplicated(dd)==F]);
    rr <-colnames(bb); rr <-rr[!rr%in%c("date","day","hour")]; rr <-sort(rr); jj <-3;
    for(jj in 1:length(rr))
    {
      dd <-tapply(bb[,rr[jj]],bb$day, function(x){mean(x,na.rm=T)});
      if(str_detect(rr[jj],"^Nied")){dd <-tapply(bb[,rr[jj]],bb$day, function(x){sum(x,na.rm=T)})};
      if(str_detect(rr[jj],"max$")){dd <-tapply(bb[,rr[jj]],bb$day, function(x){max(x,na.rm=T)})};
      if(str_detect(rr[jj],"min$")){dd <-tapply(bb[,rr[jj]],bb$day, function(x){min(x,na.rm=T)})};
      ff <-data.frame(date=names(dd),n=as.numeric(dd)); colnames(ff)[2] <-rr[jj];
      ff[is.infinite(ff[,2]) | is.nan(ff[,2]),2] <-NA;
      cc <-merge(cc,ff,by="date",all=T);
    }
    cc$date <-as.Date(cc$date,"%Y-%m-%d",tz="");
    dd <-cc;
    # summary(cc)
  }
  ### STAT_out
  {
    STAT_out_nDAY[[ll[ii]]] <-NA; STAT_out_MIN[[ll[ii]]] <-NA; STAT_out_MAX[[ll[ii]]] <-NA; STAT_out_MEANSUM[[ll[ii]]] <-NA;
    dd$year <-format(dd$date,"%Y");
    qq <-c("nday","min","max","meansum"); tt <-3;
    for(tt in 1:length(qq))
    {
      ff <-data.frame(year=levels(as.factor(dd$year)));
      rr <-colnames(dd); rr <-rr[!rr%in%c("date","year")]; rr <-sort(rr); jj <-3;
      for(jj in 1:length(rr))
      {
        if(qq[tt]%in%"nday"){cc <-tapply(dd[,rr[jj]], dd$year, function(x){length(x[is.na(x)==F])})};
        if(qq[tt]%in%"min"){cc <-tapply(dd[,rr[jj]], dd$year, function(x){min(x,na.rm=T)})};
        if(qq[tt]%in%"max"){cc <-tapply(dd[,rr[jj]], dd$year, function(x){max(x,na.rm=T)})};
        if(qq[tt]%in%"meansum")
        {
          cc <-tapply(dd[,rr[jj]], dd$year, function(x){mean(x,na.rm=T)})
          if(str_detect(rr[jj],"^Nied")){cc <-tapply(dd[,rr[jj]], dd$year, function(x){sum(x,na.rm=T)})};
          # if(str_detect(rr[jj],"^G_")){cc <-tapply(dd[,rr[jj]], dd$year, function(x){sum(x,na.rm=T)/1000})}; # Wh <- kWh
        };
        gg <-data.frame(year=names(cc),n=round(as.numeric(cc),1)); colnames(gg)[2] <-rr[jj];
        ff <-merge(ff,gg,by="year");
      }
      ### save
      ff <-ff[order(ff$year,decreasing = T),]; 
      if(qq[tt]%in%"nday"){STAT_out_nDAY[[ll[ii]]] <-ff};
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
        if(qq[tt]%in%"nday"){cc <-colorRampPalette(c("red3","orange","green4")); ccc <-cc(367)};
        if(qq[tt]%in%c("min")){cc <-colorRampPalette(c("gray60","blue3")); ccc <-cc(100)};
        if(qq[tt]%in%c("max")){cc <-colorRampPalette(c("gray60","red3")); ccc <-cc(100)};
        if(qq[tt]%in%c("meansum")){cc <-colorRampPalette(c("gray60","green3")); ccc <-cc(100)};
        ### window
        {
          graphics.off();
          if(qq[tt]%in%"nday"){out <-file.path(G$d_temp,paste("STAT_out_nDAY",ll[ii],".png",sep="_"))};
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
        if(qq[tt]%in%"nday"){text(x=x_max/2,y=par("usr")[4]+0.5,labels=paste("Anzahl an Fehltage - ",ll[ii]),srt=0,xpd=NA,cex=1.5)};   
        if(qq[tt]%in%"min"){text(x=x_max/2,y=par("usr")[4]+0.5,labels=paste("Minumum - Tageswerte ",ll[ii]),srt=0,xpd=NA,cex=1.5)};        
        if(qq[tt]%in%"max"){text(x=x_max/2,y=par("usr")[4]+0.5,labels=paste("Maximum - Tageswerte ",ll[ii]),srt=0,xpd=NA,cex=1.5)};        
        if(qq[tt]%in%"meansum"){text(x=x_max/2,y=par("usr")[4]+0.5,labels=paste("MeanSum - Tageswerte ",ll[ii]),srt=0,xpd=NA,cex=1.5)};        
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
            if(qq[tt]%in%"nday")
            {
              hh <-ccc[gg+1]; 
              if(ff[oo,1]%in%seq(1900,2100,by=4)){gg <-c(366)-gg}
              if(!ff[oo,1]%in%seq(1900,2100,by=4)){gg <-c(365)-gg}
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
  Md[[ll[ii]]] <-dd;
  ### end ii
}


# SAVE rda --------------------------------------------------------
G$d_temp <-file.path(G$d_out,"rda"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};

out <-paste(G$n_script,"Md.rda",sep="_");
save(Md,file = file.path(G$d_temp,out));

out <-paste(G$n_script,"stat_out_nDAY.rda",sep="_");
save(STAT_out_nDAY,file = file.path(G$d_temp,out));

out <-paste(G$n_script,"stat_out_MIN.rda",sep="_");
save(STAT_out_MIN,file = file.path(G$d_temp,out));

out <-paste(G$n_script,"stat_out_MAX.rda",sep="_");
save(STAT_out_MAX,file = file.path(G$d_temp,out));

out <-paste(G$n_script,"stat_out_MEANSUM.rda",sep="_");
save(STAT_out_MEANSUM,file = file.path(G$d_temp,out));


# load(file.path(G$d_out,out))

# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")