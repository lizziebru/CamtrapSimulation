## PIPELINE FOR CLUSTER - RUNNING THE SIMULATION ON PRE-GENERATED PATHS ##

rm(list = ls())

source("CamtrapSimulation.R", echo=TRUE)

iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))

filename_in <- "../paths/sp0.05_28Apr22_1724"

load(paste0(filename_in, ".RData")) # loads in a big list of paths called paths

paths <- paths1

path_input <- paths[(iter - 1)%%length(paths) + 1]

# set additional parameters:
species = 0 # currently: 0 = small, 1 = large --> ultimately: want: 1 = small herbivores, 2 = large herbivores, 3 = small carnivores, 4 = large carnivores
r = 9
th = 0.7
plot_path = TRUE
twoCTs = TRUE
connectedCTs = FALSE

filename_out <- paste0(filename_in, "_seqdat")

png(file= paste0(filename_out, ".png"),
    width=700, height=650)
seq_dats <- run_simulation(path_input, species=species, r=r, th=th, plot_path=plot_path, twoCTs=twoCTs, connectedCTs=connectedCTs)
dev.off()

save(seq_dats, file = paste0(filename_out, ".RData"))
