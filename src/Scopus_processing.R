rm(list = ls())
library(tidyverse)
library(rnaturalearth)
library(viridis)
library(leaflet)

load.scopus.files <- function(filename, name=NA){
  if (is.na(name)){
    name <- sub('\\.csv$', '', basename(filename)) 
  }
  df <- read_csv( file = filename,  quote='"', na = c("null"),
                  col_names = c("junk1", "EID","junk2","Co_Inst","junk3","Co_City","junk4","Co_Country"),
                 quoted_na = F)%>%
    select(-starts_with("junk"))%>%
  mutate(college = name)
  return(df)
}#end load.scopus.files

all <- list.files(path = "data",pattern = "*.csv", full.names = TRUE) %>% 
  lapply(load.scopus.files) %>%                                        
  bind_rows   %>%
  mutate(college = recode(college,
                         "SocSci2020_Co_Authors_SQ_Cln" = "Social Science" ,
                         "ALAll_Co_Authors_SQ_Cln"      = "Arts and Letters",
                         "libfac_Co_Authors_SQ_Cln" = "Library",
                         "Sci_Co_Authors_SQ_Cln" = "Science",
                         "keough2020_Co_Authors_SQ_Cln" = "Keough",
                         "Eng_Co_Authors_SQ_Cln" = "Engineering"))

all <- bind_rows(all, all %>%
                   mutate(college = "All") %>%
                   distinct())%>%
  filter(Co_Inst !=  "University of Notre Dame")


countries <- ne_countries(returnclass = "sf", scale = "medium") %>%
  select(admin)






full.list <- all %>% 
  group_by(Co_Country, college) %>% 
  tally() %>% 
  pivot_wider(id_cols = Co_Country, names_from = college, values_from = n) %>%
  ungroup()%>%
  mutate(Co_Country = recode(Co_Country,
                             "Hong Kong" = "Hong Kong S.A.R.",
                             "Macao" = "Macao S.A.R",
                             "Russian Federation" = "Russia",
                             "United States" = "United States of America",
                             "Tanzania" ="United Republic of Tanzania"))%>%
  filter(Co_Country != "null")
 

if (nrow(right_join(countries, full.list, by = c("admin" = "Co_Country"))%>% filter(is.na(admin))) != 0){
  print ("SOME COUNTRIES DO NOT MATCH")
}


full.list.map <- left_join(countries, full.list, by = c("admin" = "Co_Country"))
         
full.list.map$popup <- paste0("<b>",full.list.map$admin,"</b><br>",
                        "Total co-authors: ",full.list.map$All,"<br>",
                        "Co-authors with Arts and Letters Faculty: ",full.list.map$`Arts and Letters`,"<br>",
                        "Co-authors with Science Faculty: ",full.list.map$Science,"<br>",
                        "Co-authors with Engineering Faculty: ",full.list.map$Engineering,"<br><hl>",
                        "Co-authors with Social Science Faculty: ",full.list.map$`Social Science`,"<br>",
                        "Co-authors with Library Faculty: ",full.list.map$Library,"<br>",
                        "Co-authors with Keough Faculty: ",full.list.map$Keough,"<br>") %>% 
  lapply(htmltools::HTML)

save(full.list.map, file = "data/FullList_all.rData")
save(full.list.map, file = "GlobalCollaboration//FullList_all.rData")
# rm(list = ls())
load("data/FullList_all.rData")


plot.generator <- function(df, var, label = NA, default.file.out = T, file.name = NA) {
  if (is.na(label)){
    label <- paste(var, "Faculty Publications")
  }
  temper <- df %>%
    mutate(active = get(var))%>%
    mutate(active =  if_else(condition = (admin == "United States of America" | active == 0), true = NA_integer_, false = as.integer(active)))
  mx <- max(temper$active, na.rm = T)
  labes <- as.integer(seq(1, mx, length =4 ))
  created.map <- ggplot(temper) + 
    geom_sf(mapping = aes(fill = active))+
    scale_fill_viridis(name = "# Global co-authors", labels = labes, breaks = labes)+
    labs(title = label,
         subtitle = "Countries with at least one Notre Dame Co-author",
         caption = "Source: Scopus") +
    theme_bw()
  if (default.file.out){
    if (is.na(file.name)){
      fnm <- paste0("figures/",var,"_Faculty.jpg")
    } else{
      fnm <- paste0("figures/",file.name)
    } 
    ggsave(filename = fnm , plot = created.map)
  }
  
  return(created.map)
}#end plot generator


plot.generator(df = full.list.map, var = "All")
plot.generator(df = full.list.map, var = "Arts and Letters")
plot.generator(df = full.list.map, var = "Engineering")
plot.generator(df = full.list.map, var = "Science")
plot.generator(df = full.list.map, var = "Social Science")
plot.generator(df = full.list.map, var = "Library")
plot.generator(df = full.list.map, var = "Keough")

   

