# Cleaner version of the scripts from Morten.

Separated sections of the script in different functions. Currently these:
```
source("get_localmodel_data.R")
source("get_globalmodel_data.R")
source("get_obs_data.R")
source("stations.R")
source("set_vpars_conf_int.R")
```
are called from the main script 'scorecard_2models.R'.

