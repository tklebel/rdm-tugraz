source("R/00-packages.R")
source("R/01-functions.R")
source("R/02-plan.R")

make(plan)

readd(rda_fig)

config <- drake_config(plan)
vis_drake_graph(config)
