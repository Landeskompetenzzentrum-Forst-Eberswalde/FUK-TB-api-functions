#
# create token and access ThingsBoard
# list TB devices und sensors
# Available for users with 'TENANT_ADMIN' authority
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

# FUNCTION ------------------------------------------------------------

###  get_data function
post_request <- function(url, body, header = c()) {
  response <- POST(
    url = url,
    body = body,
    encode = "json",
    add_headers(.headers = header),
    verbose()
  )
  return(content(response, as = "parsed"))
}

# GLOBALES ---------------------------------------------------------------------
G <-list() ### list workfiles and facts
aa <-getActiveDocumentContext()$path
bb <-unlist(str_split(aa,"\\/"))
### name
G$n_script <-str_sub(bb[length(bb)],1,-3);
G$n_out <-G$n_script; 
G$n_user <-"rainer.hentschel@lfb.brandenburg.de"; # Sys.getenv("USERNAME")
### time
G$t_date <-Sys.Date() 
G$t_year <-as.integer(format(Sys.Date(),"%Y")); # G$year <-2023;
### dir
G$d_home <-dirname(aa); # aa <-unlist(str_split(G$d_home,"/proc"))
G$d_in1 <-G$d_home;
G$d_out1 <-G$d_home; # file.path(aa[1],"output/level2",G$n_script); if(!dir.exists(G$d_out1)){dir.create(G$d_out1)};
### end
print(G)

# TOKEN -------------------------------------------------------------
TOK <-list();
TOK$ulr <-"https://thingsboard.gruenecho.de/api/auth/login";
aa <- post_request("https://thingsboard.gruenecho.de/api/auth/login", 
                   list(username = G$n_user, password = askForPassword("Password")));
TOK$token <- aa$token; TOK$header <- c('X-Authorization' = paste("Bearer", TOK$token, sep = " "));

# DEVICES -----------------------------------------------------------
DEV <-list();
url <-"https://thingsboard.gruenecho.de/api/tenant/devices"
DEV$list_of_devices <- GET(url, query = list(pageSize = 100,page = 0), add_headers(.headers = TOK$header))
aa <-DEV$list_of_devices; bb <- content(aa, as = "parsed"); if(c("status")%in%names(bb)){message(bb$status)};

url <-"https://thingsboard.gruenecho.de/api/tenant/deviceInfos"
DEV$devices_infos <- GET(url, query = list(pageSize = 100,page = 0), add_headers(.headers = TOK$header))
aa <-DEV$devices_infos; bb <- content(aa, as = "parsed"); if(c("status")%in%names(bb)){message(bb$status)};


# TABLE devices --------------------------------------------------------------
aa <-DEV$list_of_devices; bb <- content(aa, as = "parsed");
cc <-bb$data
ii <-1; xx <-data.frame(matrix(NA,0,0));
for(ii in 1:length(cc))
{
  dd <-cc[[ii]];
  xx[ii,"dev_name"] <-dd$name;
  xx[ii,"dev_label"] <-dd$label;
  xx[ii,"dev_type"] <-dd$type;
  xx[ii,"dev_entity"] <-dd$id$entityType;
  xx[ii,"dev_id"] <-dd$id$id;
}
xx <-xx[order(xx$dev_name),]
tb_devices <-xx; 

### ADD info
aa <-DEV$devices_infos; bb <- content(aa, as = "parsed");
cc <-bb$data; dd <-tb_devices;
ii <-1;
for(ii in 1:length(cc))
{
  ee <-cc[[ii]]; ff <-dd[dd$dev_id%in%ee$id$id,];  
  tb_devices[tb_devices$dev_id%in%ff$dev_id,"tenantId"] <-ee$tenantId$id;
  tb_devices[tb_devices$dev_id%in%ff$dev_id,"customerId"] <-ee$customerId$id;
  tb_devices[tb_devices$dev_id%in%ff$dev_id,"deviceProfileId"] <-ee$deviceProfileId$id;
}


# CLEAN ----------------------------------------------------------------------------
aa <-ls(); aa <-aa[!aa%in%c("tb_devices","TOK")];
rm(list = aa); cat("//014")

