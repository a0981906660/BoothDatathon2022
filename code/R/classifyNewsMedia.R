library(tidyverse)
library(fs)
library(readxl)
library(feather)

# check working directory
print(getwd())

# load data
df <- read_csv("./data/ProquestNewsArticles/ProquestNewsMetaData.csv")[, -1]

# load wiki major media data
mediaTable <- read_csv("./data/ProquestNewsArticles/major_media_list.csv")

# Merge tables
df <- df %>% 
    rename(id = `GOID`,
           pub_loc = `Publication Location`,
           pub_name = `Publication Name`,
           pub_title = `Publication Title`,
           title = `Title`,
           author = `Author Name(s)`,
           date = `Date`,
           doc_type = `Document Type`

           )

# Is major media or not
majorMedia <- paste(mediaTable$Name, sep="", collapse="|")
df <- df %>% 
    mutate(MajorMedia = stringr::str_detect(pub_name, majorMedia))
# mediaTable %>% .$Name
# df$pub_name %>% unique() %>% sort() %>% View
# df$pub_title %>% unique() %>% sort() %>% View

for (media in mediaTable$Name) {
    if (media %in% df$pub_title %>% unique()) {
        print(media)
    }
}

df <- df %>% 
    mutate(ABC = stringr::str_detect(pub_name, "ABC"),
           CBS = stringr::str_detect(pub_name, "CBS"),
           CNN = stringr::str_detect(pub_name, "CNN"),
           FOX = stringr::str_detect(pub_name, "FOX"),
           MSNBC = stringr::str_detect(pub_name, "MSNBC"),
           NBC = stringr::str_detect(pub_name, "NBC"),
           NYT = stringr::str_detect(pub_name, "New York Times|NYT"), 
           USA_Today = stringr::str_detect(pub_name, "USA Today"),
           WSJ = stringr::str_detect(pub_name, "Wall Street Journal|WSJ"),
           WashingtonPost = stringr::str_detect(pub_name, "Washington Post"),
           POLITICO = stringr::str_detect(pub_name, "POLITICO"),
           Bloomberg = stringr::str_detect(pub_name, "Bloomberg"),
           Vice = stringr::str_detect(pub_name, "Vice"),
           HBO = stringr::str_detect(pub_name, "HBO"),
           HuffPost = stringr::str_detect(pub_name, "HuffPost"),
           TMZ = stringr::str_detect(pub_name, "TMZ"),
           CNET = stringr::str_detect(pub_name, "CNET"),
           NPR = stringr::str_detect(pub_name, "NPR"),
           HollywoodReporter = stringr::str_detect(pub_name, "Hollywood Reporter"),
           Newsweek = stringr::str_detect(pub_name, "Newsweek"),
           NewYorker = stringr::str_detect(pub_name, "New Yorker"),
           Time = stringr::str_detect(pub_name, "Time"),
           WorldReport = stringr::str_detect(pub_name, "U.S. News & World Report"),
           )

# df <- df %>% 
#     mutate(ABC = stringr::str_detect(pub_title, "ABC"),
#            CBS = stringr::str_detect(pub_title, "CBS"),
#            CNN = stringr::str_detect(pub_title, "CNN"),
#            FOX = stringr::str_detect(pub_title, "FOX"),
#            MSNBC = stringr::str_detect(pub_title, "MSNBC"),
#            NBC = stringr::str_detect(pub_title, "NBC"),
#            NYT = stringr::str_detect(pub_title, "New York Times|NYT"), 
#            USA_Today = stringr::str_detect(pub_title, "USA Today"),
#            WSJ = stringr::str_detect(pub_title, "Wall Street Journal|WSJ"),
#            WashingtonPost = stringr::str_detect(pub_title, "Washington Post"),
#            POLITICO = stringr::str_detect(pub_title, "POLITICO"),
#            Bloomberg = stringr::str_detect(pub_title, "Bloomberg"),
#            Vice = stringr::str_detect(pub_title, "Vice"),
#            HBO = stringr::str_detect(pub_title, "HBO"),
#            HuffPost = stringr::str_detect(pub_title, "HuffPost"),
#            TMZ = stringr::str_detect(pub_title, "TMZ"),
#            CNET = stringr::str_detect(pub_title, "CNET"),
#            NPR = stringr::str_detect(pub_title, "NPR"),
#            HollywoodReporter = stringr::str_detect(pub_title, "Hollywood Reporter"),
#            Newsweek = stringr::str_detect(pub_title, "Newsweek"),
#            NewYorker = stringr::str_detect(pub_title, "New Yorker"),
#            Time = stringr::str_detect(pub_title, "Time"),
#            WorldReport = stringr::str_detect(pub_title, "U.S. News & World Report"),
#            )
        
df %>% summary()

# save feather file
filepath <- "./data/cleaned/newsMedia.feather"
write_feather(df, filepath)

# to read
# df <- read_feather(filepath)
