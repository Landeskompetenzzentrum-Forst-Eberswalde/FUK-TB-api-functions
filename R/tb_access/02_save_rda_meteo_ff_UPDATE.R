#
# source "01_access_tb.R" to access thingsboard
# load 02_save_rda_meteo_ff.rda to set start date
# rbind update
# save 02_save_rda_meteo_ff.rda
#

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr","data.table","httr","jsonlite","dotenv")
for(ii in 1:length(ll)){if(!ll[ii]%in%rownames(installed.packages()))install.packages(ll[ii],dependencies = TRUE);library(ll[ii], character.only = TRUE)};

# SOURCE ------------------------------------------------------------
aa <-dirname(getActiveDocumentContext()$path); list.files(aa); setwd(aa);
source("01_access_tb.R");

# GLOBALES ---------------------------------------------------------------------
G <-list() ### list workfiles and facts
aa <-getActiveDocumentContext()$path
bb <-unlist(str_split(aa,"\\/"))
### name
G$n_script <-str_sub(bb[length(bb)],1,-3);
G$n_out <-str_sub(G$n_script,1,11);
### time
G$t_date <-Sys.Date() 
G$t_year <-as.integer(format(Sys.Date(),"%Y")); # G$year <-2023;
### dir
G$d_home <-dirname(aa); # aa <-unlist(str_split(G$d_home,"/proc"))
G$d_in <-G$d_home;
G$d_in1 <-file.path(G$d_home,"output/02_save_rda");
G$d_out <-file.path(G$d_home,"output"); if(!dir.exists(G$d_out)){dir.create(G$d_out)};
G$d_out1 <-file.path(G$d_out,G$n_out); if(!dir.exists(G$d_out1)){dir.create(G$d_out1)};
### end
print(G)

# LOAD ------------------------------------------------------------
list.files(G$d_in1); aa <-"02_save_rda_meteo_ff_.rda"; message(aa);
load(file.path(G$d_in1,aa));

# LOOP, READ & SAVE -------------------------------------------------------------------
aa <-tb_devices;
aa <-aa[str_detect(aa$dev_name,"Test")==F,]; 
aa <-aa[str_detect(aa$dev_plot,"_FF"),]; 
ii <-1;
for(ii in 1:nrow(aa))
{
  bb <-aa[ii,];
  ### old
  old <-TB[[bb$dev_plot]]; 
  dd <-max(old[[1]]$date,na.rm=T);
  start <-as.numeric(as.POSIXct(dd)) * 1000;
  end <-as.numeric(Sys.time()) * 1000;
  ### new
  message(bb$dev_name);message(" - - - - - "); message(bb$dev_plot);
  url <-paste("https://thingsboard.forstliche-umweltkontrolle.de/api/plugins/telemetry",bb$dev_entity,bb$dev_id,"keys/timeseries",sep="/");
  cc <- GET(url, add_headers(.headers = TOK$header))
  dd <- content(cc, as = "parsed"); if(c("status")%in%names(dd)){message(dd$status)};
  ee <-unlist(dd); ee <-ee[str_detect(ee,"CycleCounter")==F]; if(length(ee)==0){next};
  jj <-1;
  for(jj in 1:length(ee))
  {
    message(ee[jj])
    query <- list(keys = ee[jj],limit = "100000000",startTs = round(start),endTs = round(end));
    url <-paste("https://thingsboard.forstliche-umweltkontrolle.de/api/plugins/telemetry",bb$dev_entity,bb$dev_id,"values/timeseries?",sep="/");
    ff <- GET(url, query = query, add_headers(.headers = TOK$header));
    gg <- content(ff, as = "parsed"); if(c("status")%in%names(gg)){message(gg$status)}; 
    gg <-unlist(gg); if(is.null(gg)){next};
    hh <-data.frame(date=as.POSIXct(as.numeric(gg[seq(1,length(gg),2)])/1000),value=as.numeric(gg[seq(2,length(gg),2)]));
    ### rbind
    TB[[bb$dev_plot]][[ee[jj]]] <-rbind(old[[ee[jj]]],hh);
  }
}
out <-paste(G$n_script,".rda",sep="_");
save(TB,file = file.path(G$d_out1,out));

# CLEAN ----------------------------------------------------------------------------
aa <-ls(); aa <-aa[!aa%in%c("TB","tb_devices")];
rm(list = aa); cat("//014")

