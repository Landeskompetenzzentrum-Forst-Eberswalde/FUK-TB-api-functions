# 
# load meteo day
# merge icp forests submission
# delete outliers
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

# ENVIRONMENT -------------------------------------------------------
load_dot_env(file =file.path(G$d_home,".env"))
E <-list();
E[["sys_env"]] <-Sys.getenv(); 
E[["session"]] <-sessionInfo();
E[["options"]] <-options();


# LOAD data -------------------------------------------------------------------------
list.files(G$d_in1);
load(file.path(G$d_in1,"02_meteo_day_Md.rda")); list.files(G$d_in1);

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

# MERGE variables  --------------------------------------------------
Mc <-list(); # meteo corrections
G$d_temp <-file.path(G$d_out,paste(G$n_script)); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ll <-names(Md); ll; # ll <-ll[!ll%in%c("1101_FF")]
ii <-1; 
for(ii in 1:length(ll))
{
  ### input
  aa <-Md[[ll[ii]]]; message(ll[ii]); 
  pp <-str_replace(ll[ii],"_FF",""); pp;
  plm <-mm_plm[mm_plm$code_plot%in%pp,];
  mem <-mm_mem[mm_mem$code_plot%in%pp,];
  ### table icpf
  dd <-mm_mem$date_observation; dd <-sort(dd);
  cc <-data.frame(date=dd[duplicated(dd)==F]);
  pp <-levels(as.factor(bb$code_variable)); 
  pp <-pp[pp%in%c("AT","PR","RH","WS","SR")]
  jj <-2;
  for(jj in 1:length(pp))
  {
    print(paste(jj," of ",length(pp)));
    plm0 <-plm[plm$code_variable%in%pp[jj],];
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
  ### diff raw data
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
      dd <-merge(dd,a2,by="date",all.x=T);
    }
  }
  ### save
  Mc[[ll[ii]]] <-dd;
}

# SAVE --------------------------------------------------------
G$d_temp <-file.path(G$d_out,"rda"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};

out <-paste(G$n_script,"mm_plm.rda",sep="_");
save(mm_plm,file = file.path(G$d_temp,out));
out <-paste(G$n_script,"mm_mem.rda",sep="_");
save(mm_mem,file = file.path(G$d_temp,out));

out <-paste(G$n_script,"Mc.rda",sep="_");
save(Mc,file = file.path(G$d_temp,out));

# load(file.path(G$d_out,out))

# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")