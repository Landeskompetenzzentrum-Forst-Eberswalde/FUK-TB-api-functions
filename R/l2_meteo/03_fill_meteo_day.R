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
G$d_in1 <-G$d_out;
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
save(MOD,file = file.path(G$d_out,out));
out <-paste(G$n_script,"XX",".rda",sep="_");
save(XX,file = file.path(G$d_out,out));
out <-paste(G$n_script,"LM",".rda",sep="_");
save(LM,file = file.path(G$d_out,out));
out <-paste(G$n_script,"stat",".rda",sep="_");
save(stat,file = file.path(G$d_out,out));
# load(file.path(G$d_out,out))


# CLEAN ------------------------------------------------------------------------------
rm(list = ls());
cat("//014")

