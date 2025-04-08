# 
# read icp forest data
# reset histrorical data if required
# save rda
# 

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr","RPostgreSQL","dotenv")
for(ii in 1:length(ll)){aa <-ll[ii];if(!aa%in%rownames(installed.packages()))install.packages(aa, dependencies = TRUE); library(aa, character.only = TRUE)}


# GLOBALS G ------------------------------------------------------------------
G <-list() ### list workfiles and facts
aa <-getActiveDocumentContext()$path
bb <-unlist(str_split(aa,"\\/"))
### name
G$n_script <-str_sub(bb[length(bb)],1,-3) 
G$n_project <-bb[length(bb)-1]
G$n_plot <-str_sub(G$n_script,6,9)
### time
G$t_date <-Sys.Date() 
G$t_year <-as.integer(format(Sys.Date(),"%Y")); # G$year <-2023;
### dir
G$d_home <-dirname(aa); message(G$d_home);
G$d_in <-file.path(G$d_home,"input");  list.files(G$d_in)
G$d_in1 <-file.path(G$d_home,"output/rda"); list.files(G$d_in1);
G$d_out <-file.path(G$d_home,"output"); if(!dir.exists(G$d_out)){dir.create(G$d_out)};
G$d_out1 <-file.path(G$d_out,paste(G$n_script)); if(!dir.exists(G$d_out1)){dir.create(G$d_out1)};
### end
print(G)

# ENVIRONMENT -------------------------------------------------------
load_dot_env(file =file.path(G$d_home,".env"))
E <-list();
E[["sys_env"]] <-Sys.getenv(); 
E[["session"]] <-sessionInfo();
E[["options"]] <-options();


# LOAD data -------------------------------------------------------------------------
list.files(G$d_in1);
load(file.path(G$d_in1,"02_tb_day_Md.rda")); list.files(G$d_in1);

# CONNECT FUK_PG -------------------------------------------------------------

### CONNECT
aa <-E[["sys_env"]]
host <-aa[names(aa)%in%"FUK_PG_HOST"]; port <-aa[names(aa)%in%"FUK_PG_PORT"];
user <-aa[names(aa)%in%"FUK_PG_USER"]; pw <-aa[names(aa)%in%"FUK_PG_PW"]; db <-aa[names(aa)%in%"FUK_PG_DB"]; 
pg <- dbConnect(PostgreSQL(),host=host,user=user,password=pw,port=port,dbname=db);

### SCHEMA
s1 <-"icp_download";
qq <-paste("SELECT * FROM information_schema.tables WHERE table_schema ='", s1, "';", sep="");
aa <- dbSendQuery(pg, statement=qq);
bb <- fetch(aa, -1); tt <-bb$table_name; 

### TABLE
tt <-tt[order(tt)]; cbind(tt);
dbGetQuery(pg,paste("SET search_path TO",s1)); 
mm_plm <-dbReadTable(pg, "mm_plm"); 
mm_plm <-mm_plm[mm_plm$code_location%in%c("F"),];
dbGetQuery(pg,paste("SET search_path TO",s1)); 
mm_mem <-dbReadTable(pg, "mm_mem"); # this takes minutes
mm_mem <-mm_mem[paste0(mm_mem$survey_year,mm_mem$code_plot,mm_mem$code_variable,mm_mem$instrument_seq_nr)%in%
                  paste0(mm_plm$survey_year,mm_plm$code_plot,mm_plm$code_variable,mm_plm$instrument_seq_nr),];

# SET Mc --------------------------------------------------
Mc <-list(); # meteo corrections
ll <-sort(names(Md)); ll; # ll <-ll[!ll%in%c("1101_FF")]
ii <-4; 
for(ii in 1:length(ll))
{
  ### input
  aa <-Md[[ll[ii]]]; message(ll[ii]); 
  pp <-str_replace(ll[ii],"_FF",""); pp;
  plm <-mm_plm[mm_plm$code_plot%in%pp,];
  mem <-mm_mem[mm_mem$code_plot%in%pp,];
  ### table icpf
  dd <-mm_mem$date_observation; dd <-sort(dd);
  cc <-data.frame(date=dd[duplicated(dd)==F]); # duplicated days in icpf can occur due to multiple sensors
  pp <-levels(as.factor(mem$code_variable)); 
  pp <-pp[pp%in%c("AT","PR","RH","WS","SR")]
  jj <-2;
  for(jj in 1:length(pp))
  {
    print(paste(jj," of ",length(pp)));
    plm0 <-plm[plm$code_variable%in%pp[jj],];
    dd <-plm0[duplicated(plm0$survey_year),];
    if(nrow(dd)>0)
      {
        message("!!! dublicated sensor");
        qq <-levels(as.factor(plm0$survey_year));
        kk <-2; ee <-NULL;
        for(kk in 1:length(qq))
        {
          ff <-plm0[plm0$survey_year%in%qq[kk],];
          ee <-rbind(ee,ff[ff$instrument_seq_nr%in%max(ff$instrument_seq_nr),]);
        }
        plm0 <-ee;
      }
    mem0 <-mem[mem$code_variable%in%pp[jj],]; 
    dd <-mem0[paste0(mem0$survey_year,mem0$instrument_seq_nr)%in%paste0(plm0$survey_year,plm0$instrument_seq_nr),];
    ee <-dd[,c("date_observation","daily_mean")]; colnames(ee) <-c("date",pp[jj]);
    cc <-merge(cc,ee,by="date",all.x=T);
    if(pp[jj]%in%c("AT"))
    {
      ee <-dd[,c("date_observation","daily_min","daily_max")];
      colnames(ee) <-c("date",paste0(pp[jj],"_min"),paste0(pp[jj],"_max"));
      cc <-merge(cc,ee,by="date",all.x=T);
    }
  }
  ### missing days due to duplicaded sensors
  {
    dd <-seq.Date(as.Date(min(cc$date),tz=""),as.Date(max(cc$date),tz=""),by="day");
    dd <-dd[!dd%in%cc$date];
    if(length(dd)>0){message("!!! missing days due to duplicaded sensors")}
  }
  ### diff data
  {
    pp <-colnames(cc); pp <-pp[!pp%in%c("date")];
    dd <-data.frame(date=cc$date);
    jj <-1;
    for(jj in 1:length(pp))
    {
      a0 <-cc[,c("date",pp[jj])];
      a1 <-NULL;
      if(pp[jj]%in%"AT"){a1 <-aa[c("date","L_Temp")]};
      if(pp[jj]%in%"AT_min"){a1 <-aa[c("date","L_Temp_min")]};
      if(pp[jj]%in%"AT_max"){a1 <-aa[c("date","L_Temp_max")]};
      if(pp[jj]%in%"PR"){a1 <-aa[c("date","Nied_Pluvio","Nied_unb")]};
      if(pp[jj]%in%"RH"){a1 <-aa[c("date","L_Feuchte")]};
      if(pp[jj]%in%"SR"){a1 <-aa[c("date","G_Str")]};
      if(pp[jj]%in%"WS"){a1 <-aa[c("date","W_Gesch")]};
      a1[,2] <-round(a1[,2],2);
      a2 <-merge(a0,a1,by="date",all.x=T);
      a2[,paste("diff",pp[jj],sep="_")] <-a2[,2]-a2[,3];
      if(pp[jj]%in%"PR"){a2[,paste("diff",pp[jj],"unb",sep="_")] <-a2[,2]-a2[,4]};
      dd <-merge(dd,a2,by="date",all.x=T);
    }
  }
  ### save
  Mc[[ll[ii]]] <-dd;
  ### end ii
}

# PLOT nDayDiff ---------------------------------------------------------------
G$d_temp <-file.path(G$d_out1,"nDay"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ww <-c(0.1,1,5)
vv <-1;
for(vv in 1:length(ww))
{
  ll <-names(Mc); ll; 
  ii <-1;
  for(ii in 1:length(ll))
  {
    dd <-Mc[[ll[ii]]]; message(ll[ii]); 
    # dd <-dd[duplicated(dd$date)==F,]
    ### plot diff day
    dd$year <-format(dd$date,"%Y");
    ff <-data.frame(year=levels(as.factor(dd$year)));
    rr <-colnames(dd); rr <-rr[str_detect(rr,"^diff")]
    jj <-4;
    for(jj in 1:length(rr))
    {
      pp <-rr[jj]; 
      dd <-dd[is.na(dd[,pp])==F,];
      dd[abs(dd[,pp])>=ww[vv],pp] <-NA;
      cc <-tapply(dd[,pp], dd$year, function(x){length(x[is.na(x)])})
      gg <-data.frame(year=names(cc),n=round(as.numeric(cc),1)); colnames(gg)[2] <-rr[jj];
      ff <-merge(ff,gg,by="year");
    }
    ### plot 
    {
      ff <-ff[order(ff$year,decreasing = F),]; 
      ### limit - STAT_out
      x_min <-1; x_max <-ncol(ff)-1;
      y_min <-1; y_max <-nrow(ff)
      ### color
      cc <-colorRampPalette(c("green4","orange","red3")); ccc <-cc(367)
      ### window
      {
        graphics.off();
        out <-file.path(G$d_temp,paste0("nDayDiff_",ww[vv],"_",ll[ii],"_.png"));
        png(out, units="mm", width=x_max*30, height=y_max*10, res=300);
      }
      ### base
      par(mar=c(4,4,2,0),mgp=c(3,1,0),lab=c(5,5,7)); 
      plot(c(y_min,y_max)~c(x_min,x_max),
           col="white",xlim=c(x_min,x_max+1),ylim=c(y_min-1,y_max),
           xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
      ### axis
      text(x=x_max/2,y=par("usr")[4]+0.5,labels=paste0("nDayDiff_",ww[vv]," - ",ll[ii]),srt=0,xpd=NA,cex=1.5);   
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
          hh <-ccc[gg+1]; 
          if(gg%in%0){hh <-"blue3"}
          if(is.na(gg) | is.nan(gg) | is.infinite(gg)){hh <-"white"; gg<-""}
          rect(kk-1,oo-1,kk,oo,col=hh,border=hh);
          text(kk-0.5,oo-0.5,labels=gg,col="black",cex=0.8);
          ### end oo
        }
        ### end kk
      }
      ### save
      graphics.off();
    }
  }
}


# PLOT nDayTB ---------------------------------------------------------------
G$d_temp <-file.path(G$d_out1,"nDay"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ll <-names(Mc); ll; 
ii <-1;
for(ii in 1:length(ll))
{
  dd <-Mc[[ll[ii]]]; message(ll[ii]); 
  ### plot diff day
  dd$year <-format(dd$date,"%Y");
  ff <-data.frame(year=levels(as.factor(dd$year)));
  rr <-colnames(dd); rr <-rr[str_detect(rr,"^diff")]
  jj <-1;
  for(jj in 1:length(rr))
  {
    cc <-tapply(dd[,rr[jj]], dd$year, function(x){length(x[is.na(x)==F])})
    gg <-data.frame(year=names(cc),n=round(as.numeric(cc),1)); colnames(gg)[2] <-rr[jj];
    ff <-merge(ff,gg,by="year");
  }
  ### plot 
  {
    ff <-ff[order(ff$year,decreasing = F),]; 
    ### limit - STAT_out
    x_min <-1; x_max <-ncol(ff)-1;
    y_min <-1; y_max <-nrow(ff)
    ### color
    cc <-colorRampPalette(c("red3","orange","green4")); ccc <-cc(367)
    ### window
    {
      graphics.off();
      out <-file.path(G$d_temp,paste("nDayTB",ll[ii],".png",sep="_"));
      png(out, units="mm", width=x_max*20, height=y_max*10, res=300);
    }
    ### base
    par(mar=c(5,4,2,0),mgp=c(3,1,0),lab=c(5,5,7)); 
    plot(c(y_min,y_max)~c(x_min,x_max),
         col="white",xlim=c(x_min,x_max+1),ylim=c(y_min-1,y_max),
         xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
    ### axis
    text(x=x_max/2,y=par("usr")[4]+0.5,labels=paste("nDayTB - ",ll[ii]),srt=0,xpd=NA,cex=1.5);   
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
        hh <-ccc[gg+1]; 
        if(ff[oo,1]%in%seq(1900,2100,by=4)){gg <-c(366)-gg}
        if(!ff[oo,1]%in%seq(1900,2100,by=4)){gg <-c(365)-gg}
        if(gg%in%0){hh <-"blue3"}
        if(is.na(gg) | is.nan(gg) | is.infinite(gg)){hh <-"white"; gg<-""}
        rect(kk-1,oo-1,kk,oo,col=hh,border=hh);
        text(kk-0.5,oo-0.5,labels=gg,col="black",cex=0.8);
        ### end oo
      }
      ### end kk
    }
    ### save
    graphics.off();
  }
}

# PLOT nDayICP ---------------------------------------------------------------
G$d_temp <-file.path(G$d_out1,"nDay"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ll <-names(Mc); ll; 
ii <-4;
for(ii in 1:length(ll))
{
  dd <-Mc[[ll[ii]]]; message(ll[ii]); 
  ### plot diff day
  dd$year <-format(dd$date,"%Y");
  ff <-data.frame(year=levels(as.factor(dd$year)));
  rr <-colnames(dd);  rr <-rr[rr%in%c("AT","PR","RH","WS","SR")]
  jj <-1;
  for(jj in 1:length(rr))
  {
    cc <-tapply(dd[,rr[jj]], dd$year, function(x){length(x[is.na(x)==F])})
    gg <-data.frame(year=names(cc),n=round(as.numeric(cc),1)); colnames(gg)[2] <-rr[jj];
    ff <-merge(ff,gg,by="year");
  }
  ### plot 
  {
    ff <-ff[order(ff$year,decreasing = F),]; 
    ### limit - STAT_out
    x_min <-1; x_max <-ncol(ff)-1;
    y_min <-1; y_max <-nrow(ff)
    ### color
    cc <-colorRampPalette(c("red3","orange","green4")); ccc <-cc(367)
    ### window
    {
      graphics.off();
      out <-file.path(G$d_temp,paste("nDayICP",ll[ii],".png",sep="_"));
      png(out, units="mm", width=x_max*30, height=y_max*10, res=300);
    }
    ### base
    par(mar=c(4,4,2,0),mgp=c(3,1,0),lab=c(5,5,7)); 
    plot(c(y_min,y_max)~c(x_min,x_max),
         col="white",xlim=c(x_min,x_max+1),ylim=c(y_min-1,y_max),
         xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
    ### axis
    text(x=x_max/2,y=par("usr")[4]+0.5,labels=paste("nDayICP - ",ll[ii]),srt=0,xpd=NA,cex=1.5);   
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
        hh <-ccc[gg+1]; 
        if(ff[oo,1]%in%seq(1900,2100,by=4)){gg <-c(366)-gg}
        if(!ff[oo,1]%in%seq(1900,2100,by=4)){gg <-c(365)-gg}
        if(gg%in%0){hh <-"blue3"}
        if(is.na(gg) | is.nan(gg) | is.infinite(gg)){hh <-"white"; gg<-""}
        rect(kk-1,oo-1,kk,oo,col=hh,border=hh);
        text(kk-0.5,oo-0.5,labels=gg,col="black",cex=0.8);
        ### end oo
      }
      ### end kk
    }
    ### save
    graphics.off();
  }
}


# SET Mi ----------------------------------------------------
Mi <-list(); # meteo corrections
ll <-sort(names(Mc)); ll; # ll <-ll[!ll%in%c("1101_FF")]
ii <-7; 
for(ii in 1:length(ll))
{
  aa <-Mc[[ll[ii]]]; bb <-Md[[ll[ii]]]; message(ll[ii]); 
  ### ex duplictated in icpf
  aa <-aa[duplicated(aa$date)==F,];
  ### correct AT min max 
  aa[format(aa$date,"%Y")%in%c(2020:2022),"AT_min"] <-aa[format(aa$date,"%Y")%in%c(2020:2022),"L_Temp_min"];
  aa[format(aa$date,"%Y")%in%c(2020:2022),"AT_max"] <-aa[format(aa$date,"%Y")%in%c(2020:2022),"L_Temp_max"];
  ### correct 1206 RH SR
  aa[format(aa$date,"%Y")%in%c(2020:2022) & ll[ii]%in%c("1206_FF"),"RH"] <-aa[format(aa$date,"%Y")%in%c(2020:2022),"L_Feuchte"];
  aa[format(aa$date,"%Y")%in%c(2020:2022) & ll[ii]%in%c("1206_FF"),"SR"] <-aa[format(aa$date,"%Y")%in%c(2020:2022),"G_Str"];
  ### set new
  cc <-aa[,c("date","AT","AT_min","AT_max","PR","RH","WS","SR")];
  dd <-bb[bb$date>max(cc$date),];
  ee <-data.frame(
    date=dd$date,
    AT=dd$L_Temp,
    AT_min=dd$L_Temp_min,
    AT_max=dd$L_Temp_max,
    PR=dd$Nied_Pluvio,
    RH=dd$L_Feuchte,
    WS=dd$W_Gesch,
    SR=dd$G_Str
  )
  ff <-data.frame(date=seq.Date(max(cc$date)+1,as.Date(Sys.Date()),by="day"));
  ff <-merge(ff,ee,by="date",all.x=T);
  ### save
  Mi[[ll[ii]]] <-rbind(cc,ff);
}

# PLOT years ------------------------------------------------------
G$d_temp <-file.path(G$d_out1,"years"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ll <-names(Mi);
ii <-5;
for(ii in 1:length(ll))
{
  aa <-Mi[[ll[ii]]]; message(ll[ii]); 
  ### name
  nam <-colnames(aa)[!colnames(aa)%in%c("date")];
  kk <-1;
  for(kk in 1:length(nam))
  {
    ### names
    {
      ### nam_y
      nam_y <-nam[kk];
      if(nam_y%in%c("PR")){nam_y <-paste0(nam_y," [mm]")}
      if(nam_y%in%c("AT","AT_min","AT_mean")){nam_y <-paste0(nam_y," [°C]")}
      if(nam_y%in%c("RH")){nam_y <-paste0(nam_y," [%]")}
      if(nam_y%in%c("SR")){nam_y <-paste0(nam_y," [W m-2]")}
      if(nam_y%in%c("WS")){nam_y <-paste0(nam_y," [m s-1]")}
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




# SAVE pg -------------------------------------------------------------

### rbind plots
ll <-names(Mi);
ii <-1; xx <-NULL;
for(ii in 1:length(ll))
{
  aa <-Mi[[ll[ii]]]; 
  ### add code_plot
  pp <-str_replace(ll[ii],"_FF","");
  bb <-data.frame(matrix(NA,nrow(aa),0)); bb$plot <-pp;
  cc <-cbind(bb,aa);
  ### ex empty (this is ugly)
  dd <-is.na(cc[,c(3:ncol(cc))]);
  ee <-apply(dd, 1, function(x){length(x[x==F])});
  cc <-cc[ee>0,];
  ### rbind plots
  xx <-rbind(xx,cc);
}

### write pg table
s1 <-"fuk"
dbGetQuery(pg,paste("SET search_path TO",s1)); 
t1 <-paste(G$n_project,"mm_mem",sep="-")
dbWriteTable(pg, t1,xx,overwrite=T); 


# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")