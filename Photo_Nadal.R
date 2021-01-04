library(rvest)
library(magick)
# 10- Photo du joueur --------------------------------------------------------
a<-stringr::str_replace(J_Prenom," ", "_") 
b<-stringr::str_replace(J_Nom," ", "_")
lien<-paste0("https://fr.wikipedia.org/wiki/",a,"_",b)
myurl<-try(read_html (lien))
#ProblÃ¨me liÃ© au prÃ©nom composÃ© FranÃ§ais
if("try-error" %in% class(myurl)) myurl<-read_html(paste0("https://fr.wikipedia.org/wiki/",
                                                          stringr::str_replace(J_Prenom," ","-"),"_",
                                                          stringr::str_replace(J_Nom," ","-")))
mynode <- myurl %>% 
  html_node(".infobox_v2 img")
link <- html_attr(mynode, "src")
link<-paste0("http:",link)
img<-image_read(link)
image_ggplot(img)
