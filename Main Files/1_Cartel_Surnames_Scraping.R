## NHH - BAN-master thesis ##
# Part 1 - Finding last names 

# Loading packages # 
library(pdftools)
library(tokenizers)
library(tidyverse)
library(plyr)
library(tm)
library(qdap)
library(lexicon)
library(rvest)
library(xml2)
library(parallel)
library(tictoc)
library(morestopwords)
library(purrr)
library(countrycode)

## ---------------------------------- ## 
### 1) Extracting last-names ### 
## ---------------------------------- ## 

# -------------------------------------------------------------- #  
#####               OFAC Impact-report 2007                  #####
# -------------------------------------------------------------- #  

# Saving PDF as text
impact_report <- pdf_text("narco_impact_report.pdf") 

# Tokenizing data
data <- unlist(tokenize_ptb(impact_report))

# Extracting capital-words
data <- data[grep("[A-Z][A-Z]+", data, ignore.case = FALSE)]

# Removing special characters and white-spaces
data <- str_replace_all(data, "[^[:alnum:]]", " ")
data <- str_replace_all(data, " ", "")

# Removing duplicate words and short words
data <- unique(data)
data <- data[str_length(data) > 4]

# Removing words containing letters 
data <- unlist(str_extract_all(data, "(\\b[^\\s\\d]+\\b)"))

# Formatting as a data-frame 
clean_data <- data.frame(data)

# Manually extracting rows containing last-names

# Rows containing last-names
# NB: Data is quite messy, easier to extract last-names manually. 
rows <- c(24, 25, 26, 27, 28, 29, 30, 31, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 
          51, 52, 53, 54, 56, 84, 85, 93, 99, 111, 117, 118, 120, 123, 124, 130, 131, 132, 136, 138, 139, 143, 144, 145,
          149, 153, 154, 188, 201, 202, 203, 206, 207, 208, 224, 226, 247, 248, 254, 268, 284, 286, 287, 288, 290, 
          291, 292, 293, 294, 296, 297, 298, 302, 303, 304, 305, 306, 307, 311, 312, 316, 317, 327, 329, 332, 334, 337,
          339, 340, 341, 342, 343, 344, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 356, 358, 360, 363, 364, 365, 366, 368,
          372, 376, 378, 399, 401, 406, 407, 405, 412, 414, 422, 426, 440, 445, 446, 448, 449, 452, 454, 455, 456, 457, 460, 
          461, 462, 464, 469, 474, 473, 475, 476, 478, 481, 482, 493, 498, 515, 525, 548, 553)

# Saving as new dataframe
last_names1 <- data.frame(clean_data[rows, ]) 

# -------------------------------------------------------------- #  
#####       OFAC - Colombian Cartel Network charts            #####
# -------------------------------------------------------------- #  
# Note: Colombian charts only

# Clear pattern for this data, most last-names are written in capital letters

# Creating a vector with file-paths for each Colombian chart-PDF
file_paths <- list.files(path = "OFEC_charts",  # File-paths for each i 
                         recursive = TRUE,
                         full.names = TRUE)

# Combining charts to one string
chart_data <- data.frame(Document = file_paths,
                         text = sapply(file_paths, function(x) 
                           paste0(pdf_text(x), collapse = ' ')))  


chart_data <- lapply(file_paths, pdf_text) # Reading each chart 

chart_data <- unlist(tokenize_ptb(chart_data)) # Tokenizing data

# Cleaning data
chart_data <- chart_data[grep("[A-Z][A-Z]+", chart_data, ignore.case = FALSE)] # Keeping capital words
chart_data <- str_replace_all(chart_data, "[^[:alnum:]]", " ") # Removing special characters 
chart_data <- str_replace_all(chart_data, " ", "") # Removing spaces
chart_data <- unique(chart_data) # Keeping unique words 
chart_data <- chart_data[str_length(chart_data) > 4] # Removing short words
chart_data <- unlist(str_extract_all(chart_data, "(\\b[^\\s\\d]+\\b)")) # Removing words containing numbers 

# Final cleaning # Removing most of the observations that are not last-names
words_remove <- c("\\bAS\\w*\\b", "\\bAGRO\\w*\\b", "BRITNEY", "CAMILO", "AMARILLAS", "BARBARA","\\bCHA\\w*\\b", "\\bCO\\w*\\b",
                  "EASTMAN", "\\bINT\\w*\\b", "\\bINV\\w*\\b", "ISAAC", "JONAS", "PANAMA", "\\bPRO\\w*\\b", "\\bTR\\w*\\b", "\\bUNI\\w*\\b", "HOLDING", "\\bEMP\\w*\\b"
                  )

words_remove <- paste(words_remove, collapse =  "|") 
last_names2 <- tibble(chart_data) # Saving as df
last_names2 <- last_names2[str_detect(last_names2$chart_data, words_remove) == FALSE, ] # Removing custom stopwords

# -------------------------------------------------------------- #  
#####     OFAC - Colombia Cartels Press-releases            #####
# -------------------------------------------------------------- #  

# --------------------- # 
### Finding URLs for scraping ### 
# --------------------- # 

# Base-url
url <- " https://home.treasury.gov/news/press-releases?title=colombia&publication-start-date=&publication-end-date=2017-12-15&page="

# Nr of pages 
pages <- c(0:5)

# Saving different URLs as a vector
urls <- paste(url, pages, sep = "")

# Retrieving URLs for each press-release ID
url_list <- list()

# Loop for retrieving URLs
press_release_url <- for (i in urls){
  
  url_data <- read_html(i)
  
  url_list[[i]] <- url_data %>% 
    html_elements("h3") %>% 
    html_elements("a")
}

# Formatting URLs as list 
url_df <- url_list %>%
  map(xml_attrs) %>% 
  map_df(~as.list(.))

# Retrieving only ID for each press-release
url_ids <- str_remove(url_df$href, "/news/press-releases/")

### Loop for scraping each press-release ### 
base_url <- "https://home.treasury.gov/news/press-releases/"

press_releases <- list() # List for saving 

# Loop for scraping press-releases
tic() # Tracking time 
cl <- makeCluster(6) # Using multiple cores 

for (i in url_ids){
  
  p_release_url <- paste0(base_url, i) # URL for each press-release 
  
  # Retrieving text from each URL 
  text_press_release <- read_html(p_release_url) %>% 
    html_nodes(".clearfix.text-formatted.field.field--name-field-news-body.field--type-text-long.field--label-hidden.field__item") %>% 
    html_text() 
  
  # Saving text for each press-release in list
  press_releases[i] <- text_press_release
  
}

toc() # Tracking time 
stopCluster(cl) # Stop cluster 

save(press_releases, file = "press_releases.Rdata") # Saving list 

load("press_releases.Rdata") # Loading data

# --------------------- # 
### Cleaning data ### 
# --------------------- # 

# Note: Data is quite messy, as expected. However, full names are written in a pattern. We can thus look 
# for a pattern where we extract sequences consisting of two words starting with capital letters. 

# Preparing data for cleaning
pr <- paste(press_releases, collapse = "") # Collapsing list to one single string 

# --------------------- # 
# Custom Stopwords
# --------------------- # 
library(maps)

first_names <- freq_first_names$Name # Common first-names (US)
month <- month.name # Name of Month, typically mentioned in press release
states <- state.name # Name of state (US)

cities <- us.cities$name # Name of city (US)
cities <- gsub('.{2}$', '', cities) %>%     # Removing abbreviations 
  str_squish() %>%                         # Removing extra-space 
  unique()

countries <- countryname_dict$country.name.en %>%  # Countries
  unique() 

# Vector with all stopwords to remove
customStopWords <- c(cities, countries, month, states, words)

# Removing both common and stopwords
corpus <- Corpus(VectorSource(pr)) # Transforming to corpus 
corpus_clean <- tm_map(corpus, removeWords, stopwords("english")) # Removing stopwords
corpus_clean <- tm_map(corpus_clean, removeWords, customStopWords)
pr_data <- toString(corpus_clean[[1]]) # Transforming back to string 

# --------------------- # 
# Bigrams
# --------------------- # 

# Vector of words/patterns that are mentioned a lot 
words <- c("Kingpin", "Act", "Designation", "Drug", "Department", "Treasury", "Office", "OFAC", "Director, Content", "Archived", "Cartel", "Narcotics", "Trafifickers",
           "Action", "Targets", "Agent", "Organization", "Border", "Protection", "Force", "Executive", "District", "Foreign", "Assets", "Homeland", "Security", "Court", "Grupo",
           "Lord", "Money", "International", "Transit", "Police", "Clan", "Global", "President", "Targets", "World", "Eastern", "Western","Acting","Designated", "Specially", "PUBLIC"
           , "Control", "Criminal", "FARC", "Financial", "Miami", "National", "North", "Public", "REPORTS", "SDNT", "SDNTs", "United", "York", "Colombian", "East", "Crime", 
           "\\bComer\\w*\\b", "\\bConstr\\w*\\b", "\\bIn\\w*\\b", "\\bPro\\w*\\b", "\\bServ\\w*\\b", "\\bAgro\\w*\\b", "\\bCo\\w*\\b", "\\bSec\\w*\\b", "\\bEnf\\w*\\b",
           "\\bUn\\w*\\b", "\\bDe\\w*\\b") # Patterns to remove 


pattern <- "\\b[A-Z][a-zA-Z]{4,}\\s[A-Z][a-zA-Z]{4,}\\b" # Pattern for extracting potential names

# Extracting pattern and cleaning
names <- unlist(str_extract_all(pr_data, pattern)) # Exctracting
names <- unique(names) # Keeping unique values 

names <- tibble(names) # Saving as df 

stopw <- paste(words, collapse =  "|") # Formatting stopwords

names_df <- names[str_detect(names$names, stopw) == FALSE, ] # Removing rows containing words from string above 

# Removing first-name for each observation
names_df$names <- sub("\\b\\w+\\s*", "", names_df$names)

names_df <- unique(names_df)

last_names3 <- tibble(toupper(names_df$names))

# -------------------------------------------------------------- #  
#####     Joining last-names together for final file         #####
# -------------------------------------------------------------- #  

# Changing name of row for each file 
colnames(last_names1)[1] <- "Name"
colnames(last_names2)[1] <- "Name"
colnames(last_names3)[1] <- "Name"

# Joining dfs
last_names <- unique(rbind(last_names1, last_names2, last_names3))

rownames(last_names) <- 1:nrow(last_names) # Updating row-names

# Removing most rows containing non-names
rows <- c(62, 121, 163, 174, 173, 175, 176, 179, 180, 182, 184, 185, 186, 187, 192, 193, 195, 196, 200, 201, 215,
          217, 220, 221, 222, 225, 227, 232, 233, 234, 235, 250, 254, 255, 256, 258, 283, 284, 301, 302, 318, 336, 380, 381, 
          383, 384, 392, 397, 403, 411, 412, 417, 424, 427, 428, 429, 433, 434, 448, 449, 450, 454, 459, 460, 481,
          488, 498, 503, 506, 507, 509, 514, 522, 523, 524, 525, 526, 531, 539, 544, 549, 550, 551, 552, 568, 574, 575,  581, 585, 588,
          589, 592)

last_names <- last_names %>% 
  filter(!row_number() %in% rows) # Filtering for rows not represented in vector 

# Extracting too common names (and first-names)
# Makes it a lot easier to extract interesting results when looking up in ICIJ database - and potentially do more cleaning there
common_names <- c("ALBERTO", "ADOLPHUS","ADOLFO", "ALONSO", "ALVAREZ", "ANDREA", "ANGEL", "ARTURO", "AUGUSTO", "ANTHONY", "ANTONIO", "DAVID", "FERNANDO", "FERNANDEZ", "GABRIEL", "CESAR",
                  "GOMEZ", "GONZALEZ", "HERNANDEZ", "HOLDING", "JAIME", "JAVIER", "JULIO", "LEANDRO", "LOPEZ", "MANUEL", "MANUELLA", "MARIA", "MARIO", "SANCHEZ", "GONZALO", "DARIO", 
                  "ENRIQUE", "MARTA", "MERCEDES", "McDONOUGH", "MARTINEZ", "MICHELE", "MICHAEL", "MOHAMAD", "NICHOLLS", "PATRICIA", "PUVALOWSKI", "RICHARD", "VIVIANA", "SANTO", "SANTOS", 
                  "SANTIAGO","RODRIGUEZ", "TORRES", "VICTOR", "VICTORIA",  "WERNER", "JACOBO", "FOREST", "MARINA", "SALEH", "AYMAN", "CAMILO", "RODOLFO", "SAIED", "JULIETA",  "JUNIOR",
                  "JANNET", "SILVA", "MORENO", "SEBASTIANO", "GARCIA", "ROMERO", "FELIX", "SAIEH", "ELENA", "TRADING", "LIQUIDA", "BUREAU", "HERMANOS", "MILENA", "NASSER", "LATIN",
                  "SIMAN", "FRANCO", "FRANCISCO", "SALES", "PEREZ", "VIRGINIA", "FINANCIERA", "PARTNERS", "AVENUE", "CHALLENGE", "REGISTER", "FABIO", "OPERATIONS", "EUGENIA" ,
                  "PANAMA", "NETWORKS", "ELECTRO", "IMPORT", "BASED", "FIELD", "OWNERS", "AGUSTIN", "AGRICOLA", "VIRGIN", "BARBOSA", "CHART", "NASDAQ", "DIVISION")

# Removing common names
last_names <- last_names %>% 
  filter(!last_names$Name %in% common_names)

save(last_names, file = "last_names.Rdata") # Saving 
write.csv(last_names, "last_names.csv", row.names=FALSE) # Saving results 
 
