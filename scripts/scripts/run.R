library(plumber)

# Run this from the folder containing plumber_api.R, config.yml, and www/
# via: Rscript run.R


setwd("C:/Users/aflac/Documents/GitHub/Machine-Learning/scripts")
plumber::plumb("plumber_api_complete.R") %>% pr_run(host = "0.0.0.0", port = 8000) # not necessary use python yb_report_server.py
#plumber::plumb("plumber_api_complete.R") %>% pr_run(host = "0.0.0.0", port = 8000)
