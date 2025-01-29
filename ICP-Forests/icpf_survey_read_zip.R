### READ icpf inspect zip files 
### SAVE icpf_survey_list.rda

# PACKAGES ----------------------------------------------------------------
if(!"stringr"%in%rownames(installed.packages()))install.packages("stringr",dependencies = TRUE);library(stringr);
if(!"rstudioapi"%in%rownames(installed.packages()))install.packages("rstudioapi",dependencies = TRUE);library(rstudioapi);
# vignette(package = "stringr");  vignette("stringr", package = "stringr")

# GLOBALS ------------------------------------------------------------
G <-list(); 
G$t_year <-format(Sys.Date(),"%Y"); 
G$plots <-c(1101:1103,1201:1209);
aa <-getActiveDocumentContext()$path;
G$d_home <-dirname(aa); bb <-unlist(str_split(aa,"\\/")); 
G$script <-str_sub(bb[length(bb)],1,-3)
aa <-unlist(str_split(G$d_home,"/proc"));
G$d_in1 <-paste(aa[1],"input",sep="/");
G$d_out <-paste(aa[1],"output",sep="/"); suppressWarnings(dir.create(G$d_out));
G$d_out1 <-paste(aa[1],"output/rmd",sep="/"); suppressWarnings(dir.create(G$d_out1));

# READ -------------------------------------------------------------------
aa <-list.files(G$d_in1);  aa <-aa[str_detect(aa,".zip")];
bb <-str_sub(aa,4,5); surveys <-bb[duplicated(bb)==F];
ii <-1; XX <-list();
for(ii in 1:length(surveys))
{
  setwd(G$d_in1); message(surveys[ii]);
  XX[[surveys[ii]]] <-list();
  bb <-aa[str_detect(aa,paste("_",surveys[ii],"_",sep=""))];
  cc <-as.integer(str_sub(bb,10,17)); bb <-bb[which(cc==max(cc))];
  cc <-unzip(bb,list=F,exdir = G$d_in1); 
  bb <-cc[str_detect(cc,".csv")]; 
  bb <-bb[str_detect(bb,"README")==F & str_detect(bb,"adds")==F];
  jj <-3;
  for(jj in 1:length(bb))
  {
    dd <-read.table(bb[jj],header = T,sep=";",dec=".");
    if(nrow(dd)==0){next};
    ee <-unlist(str_split(bb[jj],"/")); ee <-str_sub(ee[length(ee)],1,-5);
    XX[[surveys[ii]]][[ee]] <-dd;
  }
  for(jj in 1:length(cc)){file.remove(cc[jj])}; 
  unlink("adds", recursive = TRUE);
}

# SAVE ---------------------------------------------------------------------
save(XX,file=paste(G$d_out,"icpf_survey_list.rda",sep="/"));
save(XX,file=paste(G$d_out1,"icpf_survey_list.rda",sep="/"));

# CLEAN ------------------------------------------------------------------------------
rm(list = ls()); gc();
cat("//014")
