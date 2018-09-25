##########
# SCENES #
##########
scripts = readRDS("scripts.rds")

scenes = scripts[grepl("\\[Scene:",scripts$script),]

loc = str_locate(scenes$script,"\\[Scene:")[,1]

scenes$script = str_sub(scenes$script,loc)

for(per in per.ger){
  scenes[[per]] = grepl(per,scenes$script)
}

eos = grepl("everyone",str_to_lower(scenes$script))

for(eo in 1:length(eos)){
  if(eos[eo]){
    scenes[eo,prin] = rep(T,length(prin))
  }
}

################
# SAVE DATASET #
################

scenes.adj = scenes %>% select(-c(ep,desc.ep,script))

saveRDS(scenes.adj,"scenes.adj.RDS")
