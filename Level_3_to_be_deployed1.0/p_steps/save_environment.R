#Script that will be used to save .Rworkspace to be loaded in the beginning of each script

save.image(file=paste0(g_intermediate, "environment.RData"))
