#
# compare with controldata
#

# ENVIRONMENT -------------------------------------------------------
E <-list();
E[["sys_env"]] <-Sys.getenv(); 
E[["session"]] <-sessionInfo();
E[["options"]] <-options();
# load_dot_env(file = "../.env")

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr","data.table","httr","jsonlite","dotenv")
for(ii in 1:length(ll)){if(!ll[ii]%in%rownames(installed.packages()))install.packages(ll[ii],dependencies = TRUE);library(ll[ii], character.only = TRUE)}

# LOAD OR SOURCE ------------------------------------------------------------
aa <-dirname(getActiveDocumentContext()$path); list.files(file.path(aa,"tb_save_data-rda"));
bb <-"tb_save_data-rda_test_migration_.rda";
load(file.path(aa,"tb_save_data-rda",bb));

### source (set source file accordingly)
aa <-dirname(getActiveDocumentContext()$path); list.files(aa);
bb <-"tb_save_data-rda.R"; 
# source(file.path(aa,bb));

# GLOBALES ---------------------------------------------------------------------
G <-list() ### list workfiles and facts
aa <-getActiveDocumentContext()$path
bb <-unlist(str_split(aa,"\\/"))
### name
G$n_script <-str_sub(bb[length(bb)],1,-3);
G$n_out <-G$n_script; 
### time
G$t_date <-Sys.Date() 
G$t_year <-as.integer(format(Sys.Date(),"%Y")); # G$year <-2023;
### dir
G$d_home <-dirname(aa); 
G$d_in1 <-G$d_home;
G$d_out1 <-file.path(G$d_home,"output",G$n_script); if(!dir.exists(G$d_out1)){dir.create(G$d_out1)};
### end
print(G)

# CONTROLDATA -------------------------------------------------------------------
CD <-list();
aa <-unlist(str_split(G$d_in1,"/TB"));
G$d_in2 <-file.path(aa[1],"FUK/3_Themen/dauerbeobachtung/output/adlm_mssql2rda/output"); list.files(G$d_in2);
aa <-list.files(G$d_in2); aa <-aa[str_detect(aa,".rda$")]; 
load(file.path(G$d_in2,"channels.rda"));
xx <-aa; rm("aa"); xx <-xx[xx$location%in%c("Kienhorst_FF"),]; levels(as.factor(xx$location));
load(file.path(G$d_in2,"measurements.rda"));  
yy <-aa; rm("aa"); yy <-yy[yy$channelid%in%xx$id,]; gc();



# CHECK ---------------------------------------------------------------------
aa <-TB[[1]]; names(aa);
bb <-levels(as.factor(as.character(xx$name)));
cc <-bb[bb%in%names(aa)];
print(paste(length(names(aa))," variables in tb import",sep="")); print(paste(length(bb)," variables in adlm db",sep="")); print(paste("missing variables: ",bb[!bb%in%names(aa)],sep=""))
zz <-data.frame(matrix(NA,length(bb),0)); zz[,"var"] <-bb;
bb <-bb[bb%in%names(aa)];
jj <-2;
for(jj in 1:length(bb))
{
  dd <-aa[[bb[jj]]]; message(bb[jj])
  ee <-xx[xx$name%in%bb[jj],];
  ff <-yy[yy$channelid%in%ee$id,];
  ff$date <-as.POSIXct(ff$unixtime/1000,format="%Y-%m-%d %H:%M:%S",tz="")
  gg <-merge(ff,dd,by="date",all.x=T);
  zz[jj,"start"] <-min(gg$date);
  zz[jj,"end"] <-max(gg$date);
  # zz[jj,"n_soll"] <-length(seq.POSIXt(zz[jj,"start"],zz[jj,"end"],by="hour"))*2; # halbstunden werte
  zz[jj,"n_ist"] <-nrow(gg);
  # zz[jj,"n_miss"] <-zz[jj,"n_soll"]-zz[jj,"n_ist"];
  hh <-gg[,4]==gg[,5];
  zz[jj,"equal_n"] <-length(hh[hh]);
  zz[jj,"equal_p"] <-round(length(hh[hh])/zz[jj,"n_ist"]*100,1);
  zz[jj,"obs_miss"] <-nrow(gg[is.na(gg[,5]),])
}


# SAVE --------------------------------------------------------------------------
out <-paste(G$n_script,".csv",sep="_");
write.table(xx,file.path(G$d_out1,out),col.names = T, row.names = F, sep=";", dec=".",na="")


# CLEAN ----------------------------------------------------------------------------
aa <-ls(); # aa <-aa[!aa%in%c("TB")];
rm(list = aa); cat("//014")

