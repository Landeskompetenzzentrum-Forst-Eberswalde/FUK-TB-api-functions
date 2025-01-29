# 
# plot icp forests data by survey and plot
# rainer.hentschel@lfb.brandenburg.de
# 

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr" # r support
)
ii <-1; # loop package install
for(ii in 1:length(ll))
{
  aa <-ll[ii];
  if(!aa%in%rownames(installed.packages()))install.packages(aa, dependencies = TRUE); library(aa, character.only = TRUE);
}


# GLOBALS G ------------------------------------------------------------------
G <-list() ### list workfiles and facts
aa <-getActiveDocumentContext()$path
bb <-unlist(str_split(aa,"\\/"))
### name
G$n_script <-str_sub(bb[length(bb)],1,-3) 
### set time
G$t_date <-Sys.Date() 
G$t_year <-as.integer(format(Sys.Date(),"%Y")); # G$year <-2023;
### set dir
G$d_home <-dirname(aa)
aa <-unlist(str_split(G$d_home,"proc"));
G$d_in1 <-paste0(aa[1],"output")
list.files(G$d_in1); load(paste(G$d_in1,"icpf_survey_list.rda",sep="/"));
### set output dir
G$d_out1 <-file.path(G$d_in1,G$n_script)
if(!dir.exists(G$d_out1)){dir.create(G$d_out1)}
### end
print(G)

# LOOP surveys -------------------------------------------------------------------
ll <-names(XX);
ll <-ll[!ll%in%c("c1","f1","s1","si","so","tv","y1")];
aa <-c(c("survey","table","site","var"),1994:G$t_year)
xx <-data.frame(matrix(NA,0,length(aa))); colnames(xx) <-aa; 
ii <-10; nn <-0;
for(ii in 1:length(ll))
{
  survey <-ll[ii]; message(survey)
  aa <-XX[[survey]];
  ### table 1 
  bb <-unlist(lapply(aa,nrow));
  bb <-bb[bb%in%max(bb)];
  tab1 <-names(bb); 
  if(survey=="cc"){tab1 <-"cc_trc"}
  if(survey=="fo"){tab1 <-"fo_fom"}
  if(survey=="gb"){tab1 <-"gb_gbm"}
  if(survey=="mm"){tab1 <-"mm_mem"}
  cc <-data.frame(aa[tab1]); message(tab1);
  ### plots
  v1 <-paste(tab1,"code_plot",sep=".");
  pp <-levels(as.factor(cc[,v1]));
  jj <-1;
  for(jj in 1:length(pp))
  {
    site <-pp[jj]; message(site);
    dd <-cc[cc[,v1]%in%site,];
    ### set columns
    {
      if(survey=="aq")
      {
        var_col <-paste(tab1,"code_compound",sep=".")
        value <-paste(tab1,"value_aq",sep=".");
        yr <-paste(tab1,"survey_year",sep=".");
        var_list <-levels(as.factor(dd[,var_col]));
      }
      if(survey=="bd")
      {
        var_col <-paste(tab1,"code_tree_species",sep=".")
        value <-paste(tab1,"dw_dbh",sep=".");
        yr <-paste(tab1,"survey_year",sep=".");
        var_list <-levels(as.factor(dd[,var_col]));
      }
      if(survey=="cc")
      {
        dd <-dd[dd[,paste(tab1,"code_removal",sep=".")]<4,];
        var_col <-paste(tab1,"code_tree_species",sep=".")
        value <-paste(tab1,"code_defoliation",sep=".");
        yr <-paste(tab1,"survey_year",sep=".");
        var_list <-levels(as.factor(dd[,var_col]));
      }
      if(survey=="dp")
      {
        var_col <-paste(tab1,"code_sampler",sep=".")
        value <-paste(tab1,"s_so4",sep=".");
        yr <-paste(tab1,"survey_year",sep=".");
        var_list <-levels(as.factor(dd[,var_col]));
      }
      if(survey=="fo")
      {
        var_col <-paste(tab1,"code_plot",sep=".")
        value <-paste(tab1,"k",sep=".");
        value <-paste(tab1,"mg",sep=".");
        # value <-paste(tab1,"ca",sep=".");
        yr <-paste(tab1,"survey_year",sep=".");
        var_list <-levels(as.factor(dd[,var_col]));
      }
      if(survey=="gb")
      {
        var_col <-paste(tab1,"code_plot",sep=".")
        value <-paste(tab1,"ca",sep=".");
        yr <-paste(tab1,"survey_year",sep=".");
        var_list <-levels(as.factor(dd[,var_col]));
      }
      if(survey=="gr")
      {
        var_col <-paste(tab1,"code_dendrometer",sep=".")
        value <-paste(tab1,"diameter_actual",sep=".");
        yr <-paste(tab1,"survey_year",sep=".");
        var_list <-levels(as.factor(dd[,var_col]));
      }
      if(survey=="gv")
      {
        var_col <-paste(tab1,"code_species",sep=".")
        value <-paste(tab1,"species_cover",sep=".");
        yr <-paste(tab1,"survey_year",sep=".");
        var_list <-levels(as.factor(dd[,var_col]));
      }
      if(survey=="la")
      {
        var_col <-paste(tab1,"code_plot",sep=".")
        value <-paste(tab1,"gap_fraction_point",sep=".");
        value <-paste(tab1,"lai_max_point",sep=".");
        yr <-paste(tab1,"survey_year",sep=".");
        var_list <-levels(as.factor(dd[,var_col]));
      }
      if(survey=="lf")
      {
        var_col <-paste(tab1,"code_sample",sep=".")
        value <-paste(tab1,"c",sep=".");
        value <-paste(tab1,"n",sep=".");
        value <-paste(tab1,"s",sep=".");
        value <-paste(tab1,"p",sep=".");
        value <-paste(tab1,"k",sep=".");
        value <-paste(tab1,"mg",sep=".");
        value <-paste(tab1,"ca",sep=".");
        yr <-paste(tab1,"survey_year",sep=".");
        var_list <-levels(as.factor(dd[,var_col]));
      }
      if(survey=="mm")
      {
        ee <-aa$mm_plm; ee <-ee[ee$code_location=="S" & ee$code_plot%in%site,];
        dd <-dd[dd$mm_mem.instrument_seq_nr%in%ee$instrument_seq_nr,];
        if(nrow(dd)==0){next}
        var_col <-paste(tab1,"code_variable",sep=".")
        value <-paste(tab1,"daily_mean",sep=".");
        yr <-paste(tab1,"survey_year",sep=".");
        var_list <-levels(as.factor(dd[,var_col]));
      }
      if(survey=="oz")
      {
        ee <-aa$mm_plm; ee <-ee[ee$code_location=="S" & ee$code_plot%in%site,];
        dd <-dd[dd$mm_mem.instrument_seq_nr%in%ee$instrument_seq_nr,];
        if(nrow(dd)==0){next}
        var_col <-paste(tab1,"code_variable",sep=".")
        value <-paste(tab1,"ozone_symptoms",sep=".");
        yr <-paste(tab1,"survey_year",sep=".");
        var_list <-levels(as.factor(dd[,var_col]));
      }
      if(survey=="ph")
      {
        var_col <-paste(tab1,"code_event",sep=".")
        value <-paste(tab1,"date_observation",sep=".");
        dd[,value] <-as.integer(format(as.Date(dd[,value]),"%j"));
        yr <-paste(tab1,"survey_year",sep=".");
        var_list <-levels(as.factor(dd[,var_col]));
      }
      if(survey=="ss")
      {
        var_col <-paste(tab1,"code_plot",sep=".")
        value <-paste(tab1,"n_no3",sep=".");
        value <-paste(tab1,"ca",sep=".");
        value <-paste(tab1,"ph",sep=".");
        yr <-paste(tab1,"survey_year",sep=".");
        var_list <-levels(as.factor(dd[,var_col]));
      }
      if(survey=="sw")
      {
        var_col <-paste(tab1,"code_plot",sep=".")
        value <-paste(tab1,"water_content_vol",sep=".");
        value <-paste(tab1,"matric_preasure",sep=".");
        yr <-paste(tab1,"survey_year",sep=".");
        var_list <-levels(as.factor(dd[,var_col]));
      }
    }
    ### loop columns
    {
      kk <-1;
      for(kk in 1:length(var_list))
      {
        var <-var_list[kk];
        surv_var_site <-paste(tab1,var,site,sep="-")
        if(survey%in%c("fo","gb","la","ss")){surv_var_site <-paste(value,site,sep="-")}
        if(survey%in%c("lf")){surv_var_site <-paste(value,var,site,sep="-")}
        ee <-dd[dd[,var_col]%in%var,]
        ee <-ee[is.na(ee[,value])==F,];
        if(survey%in%c("ph")){ee <-ee[ee$ph_phi.code_event_score%in%max(ee$ph_phi.code_event_score,na.rm=T),]}
        if(nrow(ee)==0){next};
        ### plot
        {
          ### ylab
          {
            nam_y <-"-"; nam_main <-var_list[kk];
            if (survey%in%"aq" & var%in%c("NH3","NO2","O3","SO3")){nam_y <-expression(paste(" ",mu,"g/",m^3,sep=""))}
            if (survey%in%"cc"){nam_y <-expression(paste("%",sep="")); nam_main <-paste("NBV - bart: ",nam_main, sep="")}
            if (survey%in%"dp" & value%in%"dp_dem.s_so4"){nam_y <-expression(paste("S_SO4 (mg S/l)",sep="")); nam_main <-paste("Sampler: ",nam_main, sep="")}
            if (survey%in%"fo" & value%in%"fo_fom.k"){nam_y <-expression(paste("K (mg/g)",sep=""))}
            if (survey%in%"fo" & value%in%"fo_fom.ca"){nam_y <-expression(paste("Ca (mg/g)",sep=""))}
            if (survey%in%"fo" & value%in%"fo_fom.mg"){nam_y <-expression(paste("Mg (mg/g)",sep=""))}
            if (survey%in%"gb" & value%in%"gb_gbm.ca"){nam_y <-expression(paste("Ca (mg/g)",sep=""))}
            if (survey%in%"gr" & value%in%"gr_irp.diameter_actual"){nam_y <-expression(paste("cm",sep="")); nam_main <-paste("Dendrometer: ",nam_main, sep="")}
            if (survey%in%"gv" & value%in%"gv_vem.species_cover"){nam_y <-expression(paste("%",sep="")); nam_main <-paste("Species: ",nam_main, sep="")}
            if (survey%in%"la" & value%in%"la_lam.gap_fraction_point"){nam_y <-expression(paste("%",sep="")); nam_main <-paste("GF", sep="")}
            if (survey%in%"la" & value%in%"la_lam.lai_max_point"){nam_y <-expression(paste(m^2,"/",m^2,sep="")); nam_main <-paste("LAI", sep="")}
            if (survey%in%"lf" & value%in%"lf_lfm.k"){nam_y <-expression(paste("K (mg/g)",sep=""))}
            if (survey%in%"lf" & value%in%"lf_lfm.ca"){nam_y <-expression(paste("Ca (mg/g)",sep=""))}
            if (survey%in%"lf" & value%in%"lf_lfm.mg"){nam_y <-expression(paste("Mg (mg/g)",sep=""))}
            if (survey%in%"mm"){if(nam_main=="AP"){unit <-"hPa"};if(nam_main=="AP"){unit <-"hPa"};
              if(nam_main=="AP"){unit <-"hPa"};if(nam_main=="AT"){unit <-"°C"};if(nam_main=="MP"){unit <-"kPa"};
              if(nam_main=="PR"){unit <-"mm"};if(nam_main=="RH"){unit <-"%"};if(nam_main=="SF"){unit <-"mm"};
              if(nam_main=="SR"){unit <-expression(paste("W/",m^2,sep=""))};if(nam_main=="ST"){unit <-"°C"};
              if(nam_main=="TF"){unit <-"mm"};if(nam_main=="WC"){unit <-"Vol%"};if(nam_main=="WD"){unit <-"°"};
              if(nam_main=="WS"){unit <-"m/s"};}
            if (survey%in%"mm" & value%in%"mm_mem.daily_mean"){nam_y <-unit; nam_main <-paste("Bestand: ",nam_main, sep="")}
            if (survey%in%"ph" & value%in%"ph_phi.date_observation"){nam_y <-expression(paste("DOY",sep="")); nam_main <-paste("Event: ",nam_main, sep="")}
            if (survey%in%"ss" & value%in%"ss_ssm.n_no3"){nam_y <-expression(paste("N-N",O[3]," (mg/l)",sep=""));}
            if (survey%in%"ss" & value%in%"ss_ssm.ca"){nam_y <-expression(paste("Ca (mg/l)",sep=""));}
            if (survey%in%"ss" & value%in%"ss_ssm.ph"){nam_y <-expression(paste("pH",sep=""));}
          }
          ### limits
          ff <-range(ee[,value]); gg <-ff[2]-ff[1];
          ### window
          {
            graphics.off();
            out <-paste(surv_var_site,".png",sep="_"); 
            G$d_temp <-paste(G$d_out1,tab1,sep="/")
            if(!dir.exists(G$d_temp)){dir.create(G$d_temp)}
            out2 <-paste(G$d_temp,out,sep="/");
            png(out2, units="mm", width=160, height=100, res=300);
            par(mar=c(2,4,2,1),mgp=c(2,.5,0),cex=1,lab=c(length(pp),10,7));
          }
          ### plot
          {
            hh <-boxplot(ee[,value]~ee[,yr],main=nam_main,ylab=nam_y,xlab="",pch=16,
                         ylim=c(ff[1]-gg*0.1,ff[2]),col="lightgreen",notch=F,border=T);
            legend("topleft","Mittelwert",pch="+",col="red3");
            grid();
            boxplot(ee[,value]~ee[,yr],col="lightgrey",notch=F,border=T,pch=16,
                    ylim=c(ff[1]-gg*0.1,ff[2]),add = T); # help(bxp)
            ww <-c(1:length(hh$n));
            text(x=ww,y=ff[1]-gg*0.075,hh$n,cex=.5);
            oo <-tapply(ee[,value], ee[,yr],mean);
            points(oo,pch="+",col="red3",cex=1);
            text(x=ww+c(1/length(ww)*0.5),y=as.numeric(oo+gg*0.05),as.character(round(oo,1)),col="red3",cex=.75);
          }
          ### save
          graphics.off()
        }
        ### table
        {
          nn <-nn+1;
          xx[nn,"survey"] <-survey;
          xx[nn,"table"] <-tab1;
          xx[nn,"site"] <-site;
          xx[nn,"var"] <-var;
          yr <-colnames(ee); yr <-yr[str_detect(yr,"_year")];
          ff <-levels(as.factor(ee[,yr]));
          xx[nn,c(5:ncol(xx))] <-0;
          xx[nn,colnames(xx)%in%ff] <-1;
        }
      }
    }
  }
}
out <-file.path(G$d_out1,paste0(G$n_script,".csv"));
write.table(xx,out,row.names = F,col.names = T,sep=";",dec=",",na="")

### out
ll <-levels(as.factor(xx$survey));
aa <-c(c("survey"),1994:G$t_year)
yy <-data.frame(matrix(NA,0,length(aa))); colnames(yy) <-aa; 
ii <-1;
for(ii in 1:length(ll))
{
  aa <-xx[xx$survey%in%ll[ii],];
  bb <-colSums(aa[,c(5:ncol(xx))]);
  yy[ii,1] <-ll[ii]
  yy[ii,c(2:ncol(yy))] <-bb;
}
out <-file.path(G$d_out1,paste0(G$n_script,"_out.csv"));
write.table(yy,out,row.names = F,col.names = T,sep=";",dec=",",na="")


# CLEAN ------------------------------------------------------------------------------
rm(list = ls()); gc();
cat("//014")
