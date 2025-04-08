# 
# set historical data (regio)
# model time gaps f(regio)
# save rda
# 

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr","data.table","sp","mgcv")
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
G$d_out2 <-file.path(G$d_out,G$n_script); if(!dir.exists(G$d_out2)){dir.create(G$d_out2)};
### model
G$model <-F; # set true 4 new model fit
### end
print(G)

# LOAD data ---------------------------------------------------------------------
list.files(G$d_in1);
load(file.path(G$d_in1,"05_pet_day_Mp.rda")); list.files(G$d_in1);
load(file.path(G$d_in1,"06_regio_day_.rda")); list.files(G$d_in1);

### model
out <-paste(G$n_script,"MOD.rda",sep="_");
load(file.path(G$d_out1,out)); # Me <-Mp;

# REGIO day -------------------------------------------------------------------
ll <-names(Mp); #ll <-ll[!ll%in%c("1101_FF")];
ii <-2; Mt <-list(); cc <-data.frame(date=NULL);
for(ii in 1:length(ll))
{
  aa <-data.frame(date=seq.Date(as.Date("1961-01-01"),as.Date(Sys.Date()),by="day"));
  bb <-Mp[[ll[ii]]]; message(ll[ii]);
  cc <-merge(aa,bb,by="date",all.x=T);
  qq <-unlist(str_split(ll[ii],"_FF"))[1];
  pp <-names(REG);
  ### loop variables
  mm <-colnames(cc)[-1];
  jj <-3;
  for(jj in 1:length(mm))
  {
    ### var
    var <-mm[jj]; cc[,paste0(var,"_0")] <-0;
    ### reg var
    if(var%in%c("PET")){var <-"ET"};
    if(var%in%c("SR")){var <-"RS"};
    if(!var%in%names(REG)){message("mm not found in REG"); print(mm[jj]); next;}
    dd <-REG[[var]];
    dd$date <-as.Date(dd$date,tz="");
    ### missing values
    ee <-cc[is.na(cc[,mm[jj]]),];
    ff <-dd[dd$date%in%ee$date,];
    ### fill ts
    cc[cc$date%in%ff$date,mm[jj]] <-as.numeric(ff[,qq]);
    cc[cc$date%in%ff$date,paste0(mm[jj],"_0")] <-1;
  }
  ### save
  Mt[[ll[ii]]] <-cc;
}

# MODEl variables ------------------------------------------------------------------
Mm <-Mt;
ll <-names(Mm); #ll <-ll[!ll%in%c("1101_FF")];
r2 <-data.frame(matrix(NA,0,0)); 
ii <-2; if(G$model){MOD <-list()};
for(ii in 1:length(ll))
{
  aa <-Mm[[ll[ii]]]; message(ll[ii]);
  aa <-aa[is.na(aa[,2])==F,];
  qq <-unlist(str_split(ll[ii],"_FF"))[1];
  mm <-colnames(aa)[-1]; mm <-mm[str_detect(mm,"_0")==F];
  jj <-1;
  for(jj in 1:length(mm))
  {
    var <-mm[jj]; print(paste(ll[ii]," --- ",var));
    bb <-aa[,c("date",var,paste0(var,"_0"))];
    ### reg var
    var2 <-var;
    if(var%in%c("PET")){var2 <-"ET"};
    if(var%in%c("SR")){var2 <-"RS"};
    if(!var2%in%names(REG)){message("mm not found in REG"); print(mm[jj]); next;}
    dd <-REG[[var2]]; dd$date <-as.Date(dd$date,tz="");
    ### model
    if(G$model)
    {
      cc <-merge(bb,dd,by="date");
      cc <-cc[cc[,paste0(var,"_0")]==0,];
      rr <-data.frame(res=as.numeric(cc[,var]),var1=as.numeric(cc[,4]));
      mod = gam(res ~ s(var1), data = rr); mod_sum <-summary(mod);
      MOD[[ll[ii]]][[mm[jj]]] <-mod;
      r2[ll[ii],mm[jj]] <-mod_sum$r.sq;
      message(var); message(mod_sum$r.sq);
      ### plot splines
      {
        G$d_temp <-file.path(G$d_out2,"model"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
        graphics.off();
        out <-file.path(G$d_temp,paste("model",mm[jj],ll[ii],"spline",".png",sep="_"));
        png(out, units="mm", width=100, height=100, res=300);
        plot(mod,main=paste(mm[jj],ll[ii],sep=" - ")); grid();
        graphics.off();
        out <-file.path(G$d_temp,paste("model",mm[jj],ll[ii],"data",".png",sep="_"));
        png(out, units="mm", width=100, height=100, res=300);
        plot(mod$model,main=paste(mm[jj],ll[ii],sep=" - ")); grid();
        graphics.off();
      }
    }
    ### predict
    {
      mod <-MOD[[ll[ii]]][[mm[jj]]];
      cc <-aa[aa[,paste0(var,"_0")]==1,];
      gg <-data.frame(date=dd$date,var1=dd[,qq]);
      gg <-gg[gg$date%in%cc$date,];
      hh <-predict(mod,newdata=gg,type = 'response',se = TRUE);
      gg$fit <-hh$fit;
      Mm[[ll[ii]]][Mm[[ll[ii]]]$date%in%gg$date,var] <-gg$fit;
      Mm[[ll[ii]]][Mm[[ll[ii]]]$date%in%gg$date,paste0(var,"_0")] <-2;
    }
    ### end jj
  }
  ### end ii
}

# TABLE r2 ---------------------------------------------------------------
G$d_temp <-file.path(G$d_out2); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
out <-paste(G$n_script,"r2.csv",sep="_");
write.table(r2,file.path(G$d_temp,out),sep=";",dec=",",col.names = T,row.names = F,na="")

# PLOT nDayMod ---------------------------------------------------------------
G$d_temp <-file.path(G$d_out2); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ll <-names(Mm); ll; 
ii <-1;
for(ii in 1:length(ll))
{
  dd <-Mm[[ll[ii]]]; message(ll[ii]); 
  ### plot diff day
  dd$year <-format(dd$date,"%Y"); dd <-dd[dd$year>=1991,];
  ff <-data.frame(year=levels(as.factor(dd$year)));
  rr <-colnames(dd); rr <-rr[str_detect(rr,"_0")==F]; rr <-rr[!rr%in%c("date","year")];
  jj <-1;
  for(jj in 1:length(rr))
  {
    var <-rr[jj];
    ee <-dd; ee[ee[,paste0(var,"_0")]!=0,var] <-NA;
    cc <-tapply(ee[,var], ee$year, function(x){length(x[is.na(x)])})
    gg <-data.frame(year=names(cc),n=round(as.numeric(cc),1)); colnames(gg)[2] <-var;
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
      out <-file.path(G$d_temp,paste("nDayMod",ll[ii],".png",sep="_"));
      png(out, units="mm", width=x_max*30, height=y_max*10, res=300);
    }
    ### base
    par(mar=c(5,4,2,0),mgp=c(3,1,0),lab=c(5,5,7)); 
    plot(c(y_min,y_max)~c(x_min,x_max),
         col="white",xlim=c(x_min,x_max+1),ylim=c(y_min-1,y_max),
         xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
    ### axis
    text(x=x_max/2,y=par("usr")[4]+0.5,labels=paste("nDayMod - ",ll[ii]),srt=0,xpd=NA,cex=1.5);   
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
        text(kk-0.5,oo-0.5,labels=gg,col="black",cex=1);
        ### end oo
      }
      ### end kk
    }
    ### save
    graphics.off();
  }
}


# SAVE --------------------------------------------------------------------
out <-paste(G$n_script,"Mm.rda",sep="_");
save(Mm,file = file.path(G$d_out1,out));

out <-paste(G$n_script,"MOD.rda",sep="_");
save(MOD,file = file.path(G$d_out1,out));


# load(file.path(G$d_out1,out)); Me <-Mp;

# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")
