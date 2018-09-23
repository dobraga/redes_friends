require(tidyverse)
require(rvest)
require(stringr)

###############
# PERSONAGENS #
###############
link.per = "https://pt.wikipedia.org/wiki/Lista_de_personagens_de_Friends"

# principais
prin = read_html(link.per) %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/ul/li/b/a') %>% 
  html_text()

loc = str_locate(prin," ") 

prin = str_sub(prin,end = loc) %>% 
  str_squish() %>% unique()

#secundarios
sec = read_html(link.per) %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/ul[7]/li/text()') %>% 
  html_text() %>% 
  str_remove_all(" - ")

loc = str_locate(sec," ") 

sec = str_sub(sec,end = loc) %>% str_squish()

# geral
per.ger = c(prin,sec)

###############
# TRANSCRIPTS #
###############

link = "https://fangj.github.io/friends/"

transcripts = read_html(link) %>% 
  html_nodes(xpath = '/html/body/ul/li/a') %>% 
  html_attr("href")

transcripts.title = read_html(link) %>% 
  html_nodes(xpath = '/html/body/ul/li/a') %>% 
  html_text()

# FUNÇÃO PARA BAIXAR SCRIPTS
link.scrpit = transcripts[28]
desc.script = transcripts.title[28]

download.scenes = function(link.scrpit,desc.script){
  teste = paste0(link,link.scrpit)
  
  # INFO SCRIPTS
  ep.loc = str_locate(desc.script," |-")[1]
  ep = as.numeric(str_sub(desc.script,1,ep.loc-1))
  
  ep.loc = str_locate(desc.script," ")[1]
  desc.ep = str_sub(desc.script,ep.loc) %>% str_squish()
  
  # SCRIPTS
  script = read_html(teste) %>% 
    html_text()
  
  N = str_extract_all(script,"\n")[[1]] %>% length()
  nn = str_extract_all(script,"\n\n")[[1]] %>% length()
  
  if(N>8*nn){
    script = script %>% str_split("\n") %>% 
      as.vector() %>% .[[1]]
  }else{
    script = script %>% str_split("\n\n") %>% 
      as.vector() %>% .[[1]]
  }
  
  ini.script = match(T,unlist(lapply(script,function(x) grepl("Scene",x))))
  
  if(is.na(ini.script)){
    ret = tibble(ep = ep,
                 desc.ep = desc.ep,
                 script = NA)
  }else{
  
  script = script[-c(1:ini.script-1)]
  
  script = script[!script==""]

  ret = tibble(ep = ep,
               desc.ep = desc.ep,
               script = script) %>% 
    unnest()
  }
  
  return(ret)
}

#scripts = map2_df(transcripts,transcripts.title,download.scenes)

scripts = tibble()
for(i in 1:length(transcripts)){ #28
  scripts = bind_rows(scripts,download.scenes(transcripts[i],transcripts.title[i]))
}

scripts$episode = as.numeric(str_sub(scripts$ep,-2))
scripts$season = as.numeric(str_sub(scripts$ep,1,-3))

saveRDS(scripts,"scripts.rds")


#######################
# ANALISE TRANSCRIPTS #
#######################

(qtd_lines = scripts %>% 
  group_by(ep,desc.ep) %>% 
  summarise(n=n()))

qtd_lines %>%  ggplot(aes(x=ep,y=n))+geom_histogram(stat='identity')




##########
# SCENES #
##########

for(per in per.ger){
  apos = grepl(paste0(per,"'s"),scenes)
  per.bool = grepl(per,scenes)
  
  tb_scene[[per]] = ifelse(per.bool == F,apos,ifelse(apos==T,T,F))
}


eos = grepl("everyone",str_to_lower(scenes))

for(eo in 1:length(eos)){
  if(eos[eo]){
    tb_scene[eo,prin] = rep(T,length(prin))
  }
}
