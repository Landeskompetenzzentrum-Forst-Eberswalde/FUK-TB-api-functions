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
### time
G$t_date <-Sys.Date() 
G$t_year <-as.integer(format(Sys.Date(),"%Y")); # G$year <-2023;
### dir
G$d_home <-dirname(aa); message(G$d_home);
G$d_in <-file.path(G$d_home,"input");
G$d_out <-paste(G$d_home,"output",sep="/"); if(!dir.exists(G$d_out)){dir.create(G$d_out)};
G$d_in1 <-file.path(G$d_out ,"rda");  list.files(G$d_in1)
G$d_out1 <-file.path(G$d_out ,"rda");  list.files(G$d_out1)
### end
print(G)

# set data -------------------------------------------------------------------------
list.files(G$d_in1);
load(file.path(G$d_in1,"01_get_meteo_day_.rda"));
load(file.path(G$d_in1,"02_get_meteo_regio_.rda")); 

# check, ex & fill ----------------------------------------------------------------
ll <-names(REG); ll; names(MM[[1]]);
var <-c("t_mean","precip","et_fao");
LM <-list(); MOD <-list(); XX <-list(); 
stat <-data.frame(matrix(NA,0,0)); # list.files(file.path(G$d_in2,var2[jj]));
ii <-1; 
for(ii in 1:length(ll))
{
  aa <-REG[[ll[ii]]];
  ### loop sites
  pp <-colnames(aa)[-1];
  jj <-1;
  for(jj in 1:length(pp))
  {
    stat[nrow(stat)+1,] <-NA;
    LM[[pp[jj]]][[var[ii]]] <-list();
    MOD[[pp[jj]]][[var[ii]]] <-list();
    XX[[pp[jj]]][[var[ii]]] <-list();
    ### data 
    bb <-MM[[paste0(pp[jj],"_FF")]]; bb <-bb[,c("date",var[ii])]; colnames(bb)[2] <-"obs";
    cc <-aa[,c("date",pp[jj])]; colnames(cc)[2] <-"reg";
    cc <-cc[as.Date(cc$date)>=min(as.Date(bb$date)),];
    dd <-merge(cc,bb,by="date",all=T); # plot(dd$reg~dd$obs)
    dd$date <-as.Date(dd$date); dd$reg <-as.numeric(dd$reg);
    ### ex (create clean regression training set)
    ex <-5; # limit of deviation from regio in %
    if(var[ii]%in%c("precip")){ex <-100};
    dd$diff <-c(dd$reg+100)-c(dd$obs+100); # avoid zero and negative values
    dd$proc <-round(abs(dd$diff)/abs(dd$reg+100)*100,2);
    ee <-dd[abs(dd$proc)>ex & is.na(dd$proc)==F,];
    print(paste0("plot: ",pp[jj]," - var: ",var[ii],": ",nrow(ee)," diff > ",ex,"% of regio"));
    dd <-dd[!rownames(dd)%in%rownames(ee),];
    ### stat
    stat[nrow(stat),"plot"]  <-pp[jj];
    stat[nrow(stat),"var"]  <-var[ii];
    stat[nrow(stat),"ex_lim"]  <-ex;
    stat[nrow(stat),"ex_n"]  <-nrow(ee);
    ### loop lm()
    day <-levels(as.factor(format(dd$date,"%m-%d")));
    kk <-1; yy <-data.frame(matrix(NA,0,0)); 
    for(kk in 1:length(day))
    {
      ee <-dd[format(dd$date,"%m-%d")%in%day[kk],];
      ff <-ee[is.na(ee$obs)==F,];
      gg <-lm(ee[,3]~ee[,2]); 
      hh <-summary(gg);
      ff <-ee[is.na(ee$obs),]; # if(nrow(ee)==0){next}
      if(nrow(ff)>0)
      {
        ff$pred <-gg$coefficients[1]+gg$coefficients[2]*ff[,2];
        dd[dd$date%in%ff$date,"pred"] <-ff$pred;
      }
      yy[kk,"day"] <-day[kk]; 
      yy[kk,"na"] <-nrow(ff); 
      yy[kk,"coef_1"] <-gg$coefficients[1]; 
      yy[kk,"coef_2"] <-gg$coefficients[2];
      yy[kk,"r-square"] <-hh$r.squared; 
    }
    ### result
    MOD[[pp[jj]]][[var[ii]]] <-dd
    dd[is.na(dd$obs),"obs"] <-dd[is.na(dd$obs),"pred"]
    XX[[pp[jj]]][[var[ii]]] <-dd[,c("date","obs")];
    LM[[pp[jj]]][[var[ii]]] <-yy;
    stat[nrow(stat),"r-square"]  <-mean(yy$`r-square`);
    stat[nrow(stat),"n-prediced"]  <-sum(yy$na);
  }
}
out <-paste(G$n_script,"MOD",".rda",sep="_");
save(MOD,file = file.path(G$d_out1,out));
out <-paste(G$n_script,"XX",".rda",sep="_");
save(XX,file = file.path(G$d_out1,out));
out <-paste(G$n_script,"LM",".rda",sep="_");
save(LM,file = file.path(G$d_out1,out));
out <-paste(G$n_script,"stat",".rda",sep="_");
save(stat,file = file.path(G$d_out1,out));
# load(file.path(G$d_out,out))

# STAT_out -------------------------------------------------------------------
G$d_temp <-file.path(G$d_out,paste(G$n_script,"stat",sep="-")); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ll <-names(MOD); STAT_out_DOY <-list();  STAT_out_AGG <-list();
ii <-1;
for(ii in 1:length(ll))
{
  a0 <-MOD[[ll[ii]]]; a1 <-XX[[ll[ii]]];
  b0 <-data.frame(date=a0[[1]]$date); b1 <-data.frame(date=a1[[1]]$date);
  nam <-names(a0); nam==names(a1);
  jj <-1; 
  for(jj in 1:length(nam))
  {
    cc <-a0[[nam[jj]]]; 
    b0 <-merge(b0,cc[,c("date","pred")],by="date");
    colnames(b0)[ncol(b0)] <-names(a0)[jj];
    cc <-a1[[nam[jj]]]; 
    b1 <-merge(b1,cc[,c("date","obs")],by="date");
    colnames(b1)[ncol(b1)] <-names(a1)[jj];
  }
  ### STAT_out_DOY
  {
    STAT_out_DOY[[ll[ii]]] <-NA; ee <-b0; 
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
    STAT_out_DOY[[ll[ii]]] <-ff; 
    ### plot
    {
      ### limit - STAT_out
      x_min <-1; x_max <-ncol(ff)-1;
      y_min <-1; y_max <-nrow(ff)
      ### color
      cc <-colorRampPalette(c("green4","orange","red3"));
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
           main=paste("Anzahl an modellierter Tage pro Jahr und Variable - ",ll[ii]))
      ### axis
      text(x=c(x_min:x_max)+0.5,y=par("usr")[3]-0.45,labels=colnames(ff)[-1],srt=90,xpd=NA)
      text(x=par("usr")[1]-0.1,y=c(y_min:y_max)-0.5,labels=ff[,1],srt=0,xpd=NA)
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
  ### STAT_out_AGG
  {
    STAT_out_AGG[[ll[ii]]] <-NA; ee <-b1; 
    ee$year <-format(ee$date,"%Y");
    ff <-data.frame(year=levels(as.factor(ee$year)));
    kk <-2;
    for(kk in 2:ncol(ee))
    {
      if(kk==ncol(ee)){next}
      if(colnames(ee)[kk]%in%c("precip","et_fao")){cc <-tapply(ee[,kk], ee$year, function(x){sum(x)})}
      if(!colnames(ee)[kk]%in%c("precip","et_fao")){cc <-tapply(ee[,kk], ee$year, function(x){mean(x)})}
      gg <-data.frame(year=names(cc),n=round(as.numeric(cc),1))
      colnames(gg)[2] <-colnames(ee)[kk]
      ff <-merge(ff,gg,by="year");
    }
    STAT_out_AGG[[ll[ii]]] <-ff; 
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
      out <-file.path(G$d_temp,paste("STAT_out_AGG",ll[ii],".png",sep="_"));
      png(out, units="mm", width=160, height=y_max*10, res=300);
      par(mar=c(4,4,2,0),mgp=c(1,1,0),lab=c(5,5,7)); # par()
      ### base
      plot(c(y_min,y_max)~c(x_min,x_max),
           col="white",xlim=c(x_min,x_max+1),ylim=c(y_min-1,y_max),
           xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
           main=paste("Jahreswert je Variable - ",ll[ii]))
      ### axis
      text(x=c(x_min:x_max)+0.5,y=par("usr")[3]-0.45,labels=colnames(ff)[-1],srt=90,xpd=NA)
      text(x=par("usr")[1]-0.1,y=c(y_min:y_max)-0.5,labels=ff[,1],srt=0,xpd=NA,cex=.8)
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
}

# PLOT years ------------------------------------------------------
G$d_temp <-file.path(G$d_out,paste(G$n_script,"years",sep="-")); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ll <-names(XX);
ii <-1;
for(ii in 1:length(ll))
{
  a0 <-XX[[ii]];
  ### name
  nam <-names(a0);
  kk <-1;
  for(kk in 1:length(nam))
  {
    aa <-a0[[kk]];
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
    bb <-data.frame(dat=aa$date,wert=aa$obs)
    bb$yr <-format(bb$dat,"%Y"); bb$doy <-as.integer(format(bb$dat,"%j"));
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

# CHECK -------------------------------------------------------------------------

aa <-MOD$`1201`$precip;
bb <-aa[format(aa$date,"%Y")%in%c(2016),]
sum(bb$obs)
sum(bb$reg)
bb <-aa[format(aa$date,"%Y")%in%c(2018),]
sum(bb$obs,na.rm=T)
sum(bb$reg)


aa <-MM$`1201_FF`[,c("date","precip")];  aa$date <-as.Date(aa$date,tz="");
bb <-aa[format(aa$date,"%Y")%in%c(2016),]
sum(bb[,2],na.rm=T)
bb <-aa[format(aa$date,"%Y")%in%c(2018),]
sum(bb[,2],na.rm=T)



# CLEAN ------------------------------------------------------------------------------
rm(list = ls());
cat("//014")

