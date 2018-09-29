###############
# PERSONAGENS #
###############
link.per = "https://pt.wikipedia.org/wiki/Lista_de_personagens_de_Friends"

# ---------- #
# Principais #
# ---------- #
prin = read_html(link.per) %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/ul/li/b/a') %>% 
  html_text()

loc = str_locate(prin," ") 

prin = str_sub(prin,end = loc) %>% 
  str_squish() %>% unique()

saveRDS(prin,"prin.rds")

# ----------- #
# Secundarios #
# ----------- #
sec = read_html(link.per) %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/ul[7]/li/text()') %>% 
  html_text() %>% 
  str_remove_all(" - ")

loc = str_locate(sec," ")[,1] 

sec.prin = str_sub(sec,end = loc) %>% str_squish()
saveRDS(sec.prin,"sec.rds")

# ----- #
# Geral #
# ----- #
per.ger = c(prin,sec.prin)

# ----------------------- #
# Relação com personagens #
# ----------------------- #
aux = str_sub(sec,start = loc) %>% 
  str_squish() %>% 
  unique()

sec = tibble(sec = sec.prin,
             rel = NA)
for(per in per.ger){
  if(nrow(sec[str_detect(aux,per),2])!=0){
    if(is.na(sec[str_detect(aux,per),2])){
      sec[str_detect(aux,per),2] = per
    }
  }
}

saveRDS(sec,"rel_sec.rds")


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

scripts = map2_df(transcripts,transcripts.title,download.scenes)

scripts$episode = as.numeric(str_sub(scripts$ep,-2))
scripts$season = as.numeric(str_sub(scripts$ep,1,-3))

saveRDS(scripts,"scripts.rds")


#######################
# ANALISE TRANSCRIPTS #
#######################

(qtd_lines = scripts %>% 
  group_by(ep,season,episode,desc.ep) %>% 
  summarise(n=n()))

qtd_lines %>%  
  ggplot(aes(x=episode,y=n))+geom_histogram(stat='identity') + facet_wrap(~season) 