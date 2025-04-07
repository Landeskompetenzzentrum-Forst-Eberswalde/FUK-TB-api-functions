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
load(file.path(G$d_in1,"07_ts_day_Mm.rda")); list.files(G$d_in1);


# Aggregate - AGG  --------------------------------------------------
AGG <-list();
ll <-names(Mm); ll; # ll <-ll[!ll%in%c("1101_FF")]
ii <-1; 
for(ii in 1:length(ll))
{
  ### input
  aa <-Mm[[ll[ii]]]; message(ll[ii]); 
  nam_site <-unlist(str_split(ll[ii],"_FF"))[1];
  ### variables
  bb <-colnames(aa)[-1]; bb <-bb[str_detect(bb,"_0")==F]; jj <-1;
  for(jj in 1:length(bb)) ### loop variables
  {
    ### aggregate
    nam_aggregate <-"year"; aa[,nam_aggregate] <-format(aa$date,"%Y");
    ### metrics
    dd <-c("MIN","MAX","MEANSUM"); tt <-3;
    for(tt in 1:length(dd)) ### loop metrics
    {
      nam_metric <-paste(bb[jj],dd[tt],sep="_");
      ### set table
      if(ii==1){ff <-data.frame(agg=levels(as.factor(aa[,nam_aggregate]))); colnames(ff)[1] <-nam_aggregate};
      if(nam_metric%in%names(AGG)){ff <-AGG[[nam_metric]]};
      ### tapply
      if(dd[tt]%in%"MIN"){cc <-tapply(aa[,bb[jj]],aa[,nam_aggregate], function(x){min(x,na.rm=T)})};
      if(dd[tt]%in%"MAX"){cc <-tapply(aa[,bb[jj]],aa[,nam_aggregate], function(x){max(x,na.rm=T)})};
      if(dd[tt]%in%"MEANSUM")
      {
        cc <-tapply(aa[,bb[jj]], aa[,nam_aggregate], function(x){mean(x,na.rm=T)})
        if(bb[jj]%in%c("PR","PET")){cc <-tapply(aa[,bb[jj]],aa[,nam_aggregate], function(x){sum(x,na.rm=T)})};
      };
      ### merge
      ee <-data.frame(a=names(cc),b=round(as.numeric(cc),1)); 
      colnames(ee)[1] <-nam_aggregate; colnames(ee)[2] <-nam_site;
      ff <-merge(ff,ee,by=nam_aggregate,all=T);
      ### save
      AGG[[nam_metric]] <-ff[order(ff[,nam_aggregate],decreasing = T),]; 
      ### end tt
    }
  ### end jj
  }
### end ii
}

# PLOT AGG -------------------------------------------
G$d_temp <-file.path(G$d_out,paste(G$n_script,sep="-")); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ll <-names(AGG); ll; ii <-1;
for(ii in 1:length(ll))
{
  ff <-AGG[[ll[ii]]];
  ff <-ff[order(ff[,1],decreasing = F),];
  nam_aggregate <-colnames(ff)[1];
  ### limit - STAT_out
  x_min <-1; x_max <-ncol(ff)-1;
  y_min <-1; y_max <-nrow(ff)
  ### color
  if(str_detect(ll[ii],"MIN$")){cc <-colorRampPalette(c("blue3","gray60")); ccc <-cc(100)};
  if(str_detect(ll[ii],"MAX$")){cc <-colorRampPalette(c("gray60","red3")); ccc <-cc(100)};
  if(str_detect(ll[ii],"MEANSUM$")){cc <-colorRampPalette(c("gray60","green3")); ccc <-cc(100)};
  ### window
  {
    graphics.off();
    out <-file.path(G$d_temp,paste("AGG",nam_aggregate,ll[ii],".png",sep="_"));
    png(out, units="mm", width=x_max*50, height=y_max*10, res=300);
  }
  ### base
  {
    par(mar=c(5,5,4,0),mgp=c(3,1,0),lab=c(5,5,7)); 
    plot(c(y_min,y_max)~c(x_min,x_max),
         col="white",xlim=c(x_min,x_max+1),ylim=c(y_min-1,y_max),
         xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
  }
  ### axis
  {
    text(x=(x_max/2)+1,y=par("usr")[4]+0.1,labels=paste(nam_aggregate,ll[ii],sep=" - "),srt=0,xpd=NA,cex=4)
    text(x=c(x_min:x_max)+0.5,y=par("usr")[3]-0.2,labels=colnames(ff)[-1],srt=90,xpd=NA,cex=3)
    text(x=par("usr")[1]-0.05,y=c(y_min:y_max)-0.5,labels=ff[,1],srt=0,xpd=NA,cex=2)
  }
  ### rect
  kk <-2;
  for(kk in 2:ncol(ff))
  {
    oo <-5;
    for(oo in 1:y_max)
    {
      gg <-ff[oo,kk]; rr <-floor(rank(ff[,kk])/max(rank(ff[,kk]))*100); hh <-ccc[rr[oo]];
      if(is.na(gg) | is.nan(gg) | is.infinite(gg)){hh <-"white"; gg<-""}
      rect(kk-1,oo-1,kk,oo,col=hh,border=hh);
      text(kk-0.5,oo-0.5,labels=gg,col="black",cex=2);
    ### end oo
    }
  ### end kk
  }
  ### save
  graphics.off();
### end ii
}

# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")
