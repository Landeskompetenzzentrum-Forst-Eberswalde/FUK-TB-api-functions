# 
# ex sensor error
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
G$d_out1 <-file.path(G$d_out,paste(G$n_script)); if(!dir.exists(G$d_out1)){dir.create(G$d_out1)};
### end
print(G)

# ENVIRONMENT -------------------------------------------------------
load_dot_env(file =file.path(G$d_home,".env"))
E <-list();
E[["sys_env"]] <-Sys.getenv(); 
E[["session"]] <-sessionInfo();
E[["options"]] <-options();

# CONNECT FUK_PG -------------------------------------------------------------

### CONNECT
aa <-E[["sys_env"]]
host <-aa[names(aa)%in%"FUK_PG_HOST"]; port <-aa[names(aa)%in%"FUK_PG_PORT"];
user <-aa[names(aa)%in%"FUK_PG_USER"]; pw <-aa[names(aa)%in%"FUK_PG_PW"]; db <-aa[names(aa)%in%"FUK_PG_DB"]; 
pg <- dbConnect(PostgreSQL(),host=host,user=user,password=pw,port=port,dbname=db);

### SCHEMA
s1 <-"fuk";
qq <-paste("SELECT * FROM information_schema.tables WHERE table_schema ='", s1, "';", sep="");
aa <- dbSendQuery(pg, statement=qq);
bb <- fetch(aa, -1); tt <-bb$table_name; 

### TABLE
tt <-tt[order(tt)]; cbind(tt);
dbGetQuery(pg,paste("SET search_path TO",s1)); 
xx <-dbReadTable(pg, "l2_meteo-mm_mem"); 

### list plots
ll <-levels(as.factor(xx$plot));
ii <-1; Me <-list();
for(ii in 1:length(ll))
{
  aa <-xx[xx$plot%in%ll[ii],]; 
  Me[[paste0(ll[ii],"_FF")]] <-aa[,!colnames(aa)%in%c("plot")];
}


# CORRECT days ------------------------------------------------------

### 1101
{
  aa <-Me[["1101_FF"]]; aa <-aa[format(aa$date,"%Y")%in%c(2021:2023),]
  bb <-Me[["1101_FF"]][Me[["1101_FF"]]$date%in%as.Date("2021-12-25"),];
  Me[["1101_FF"]][rownames(Me[["1101_FF"]])%in%rownames(bb),"SR"] <-NA;
  bb <-Me[["1101_FF"]][Me[["1101_FF"]]$date%in%as.Date("2023-02-07"),];
  Me[["1101_FF"]][rownames(Me[["1101_FF"]])%in%rownames(bb),"SR"] <-NA;
  bb <-Me[["1101_FF"]][Me[["1101_FF"]]$date%in%as.Date("2023-08-23"),];
  Me[["1101_FF"]][rownames(Me[["1101_FF"]])%in%rownames(bb),"SR"] <-NA;
  
  aa <-Me[["1101_FF"]]; aa <-aa[format(aa$date,"%Y")%in%c(2023:2025),]
  bb <-Me[["1101_FF"]][Me[["1101_FF"]]$date>as.Date("2023-02-04") & Me[["1101_FF"]]$date<as.Date("2023-04-24"),];
  Me[["1101_FF"]][rownames(Me[["1101_FF"]])%in%rownames(bb),"WS"] <-NA;
  bb <-Me[["1101_FF"]][Me[["1101_FF"]]$date>as.Date("2023-05-14") & Me[["1101_FF"]]$date<as.Date("2023-08-29"),];
  Me[["1101_FF"]][rownames(Me[["1101_FF"]])%in%rownames(bb),"WS"] <-NA;
  bb <-Me[["1101_FF"]][Me[["1101_FF"]]$date>as.Date("2024-12-02") & Me[["1101_FF"]]$date<as.Date("2025-01-16"),];
  Me[["1101_FF"]][rownames(Me[["1101_FF"]])%in%rownames(bb),"WS"] <-NA;
}


### 1204
{
  aa <-Me[["1204_FF"]]; aa <-aa[format(aa$date,"%Y")%in%c(2023),]
  bb <-Me[["1204_FF"]][Me[["1204_FF"]]$date>as.Date("2023-02-28") & Me[["1204_FF"]]$date<as.Date("2023-06-14"),];
  # Me[["1204_FF"]] <-Me[["1204_FF"]][!rownames(Me[["1204_FF"]])%in%rownames(bb),];
  # Me[["1204_FF"]][rownames(Me[["1204_FF"]])%in%rownames(bb),"AT"] <-NA;
  # Me[["1204_FF"]][rownames(Me[["1204_FF"]])%in%rownames(bb),"AT_min"] <-NA;
  
  aa <-Me[["1204_FF"]]; aa <-aa[format(aa$date,"%Y")%in%c(2023,2024),]
  bb <-Me[["1204_FF"]][Me[["1204_FF"]]$date>as.Date("2023-07-24") & Me[["1204_FF"]]$date<as.Date("2024-12-31") & Me[["1204_FF"]]$AT_min%in%0,];
  Me[["1204_FF"]][rownames(Me[["1204_FF"]])%in%rownames(bb),"AT_min"] <-NA;
  
  aa <-Me[["1204_FF"]]; aa <-aa[format(aa$date,"%Y")%in%c(2022:2023),]
  bb <-Me[["1204_FF"]][Me[["1204_FF"]]$date>as.Date("2022-03-13") & Me[["1204_FF"]]$date<as.Date("2023-07-25"),];
  Me[["1204_FF"]][rownames(Me[["1204_FF"]])%in%rownames(bb),"SR"] <-NA;
  
  aa <-Me[["1204_FF"]]; aa <-aa[format(aa$date,"%Y")%in%c(2022:2023),]
  bb <-Me[["1204_FF"]][Me[["1204_FF"]]$date>as.Date("2022-03-15") & Me[["1204_FF"]]$date<as.Date("2023-07-25"),];
  Me[["1204_FF"]][rownames(Me[["1204_FF"]])%in%rownames(bb),"WS"] <-NA;
  
  aa <-Me[["1204_FF"]]; aa <-aa[format(aa$date,"%Y")%in%c(2023),]
  bb <-Me[["1204_FF"]][Me[["1204_FF"]]$date>as.Date("2023-03-01") & Me[["1204_FF"]]$date<as.Date("2023-06-13"),];
  Me[["1204_FF"]][rownames(Me[["1204_FF"]])%in%rownames(bb),"RH"] <-NA;
  
  aa <-Me[["1204_FF"]]; aa <-aa[format(aa$date,"%Y")%in%c(2022:2023),]
  bb <-Me[["1204_FF"]][Me[["1204_FF"]]$date>as.Date("2022-10-01") & Me[["1204_FF"]]$date<as.Date("2023-08-04"),];
  Me[["1204_FF"]][rownames(Me[["1204_FF"]])%in%rownames(bb),"PR"] <-NA;
}

### 1205
{
  aa <-Me[["1205_FF"]]; aa <-aa[format(aa$date,"%Y")%in%c(2024),]
  bb <-Me[["1205_FF"]][Me[["1205_FF"]]$date>as.Date("2024-05-23") & Me[["1205_FF"]]$date<as.Date("2025-03-25") & Me[["1205_FF"]]$AT_min%in%0,];
  Me[["1205_FF"]][rownames(Me[["1205_FF"]])%in%rownames(bb),"AT_min"] <-NA;
  
  aa <-Me[["1205_FF"]]; aa <-aa[format(aa$date,"%Y")%in%c(2023),]
  bb <-Me[["1205_FF"]][Me[["1205_FF"]]$date>as.Date("2023-03-14") & Me[["1205_FF"]]$date<as.Date("2023-03-21"),];
  Me[["1205_FF"]][rownames(Me[["1205_FF"]])%in%rownames(bb),"SR"] <-NA;
  
  aa <-Me[["1205_FF"]]; aa <-aa[format(aa$date,"%Y")%in%c(2025),]
  bb <-Me[["1205_FF"]][Me[["1205_FF"]]$date%in%as.Date("2025-02-08"),];
  Me[["1205_FF"]][rownames(Me[["1205_FF"]])%in%rownames(bb),"SR"] <-NA;
}

### 1206
{
  aa <-Me[["1206_FF"]]; aa <-aa[format(aa$date,"%Y")%in%c(2022:2024),]
  bb <-Me[["1206_FF"]][Me[["1206_FF"]]$date>as.Date("2023-11-12") & Me[["1206_FF"]]$date<as.Date("2025-03-25") & Me[["1206_FF"]]$AT_min%in%0,];
  Me[["1206_FF"]][rownames(Me[["1206_FF"]])%in%rownames(bb),"AT_min"] <-NA;
}

# PLOT years ------------------------------------------------------
G$d_temp <-file.path(G$d_out1,"years"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
ll <-names(Me);
ii <-5;
for(ii in 1:length(ll))
{
  aa <-Me[[ll[ii]]]; message(ll[ii]); 
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
ll <-names(Me);
ii <-1; xx <-NULL;
for(ii in 1:length(ll))
{
  aa <-Me[[ll[ii]]]; 
  ### add code_plot
  pp <-str_replace(ll[ii],"_FF","");
  bb <-data.frame(matrix(NA,nrow(aa),0)); bb$plot <-pp;
  cc <-cbind(bb,aa);
  ### rbind plots
  xx <-rbind(xx,cc);
}

### write pg table
s1 <-"fuk"
dbGetQuery(pg,paste("SET search_path TO",s1)); 
t1 <-paste(G$n_project,"mm_mem",sep="-")
dbWriteTable(pg, t1,xx,overwrite=T); 


# load(file.path(G$d_temp,out))

# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")