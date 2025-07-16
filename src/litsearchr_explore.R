## litsearchr experiment
## Created by: Paul Julian (pauljulianphd@gmail.com)
## Created on: 2025-05-07

## https://elizagrames.github.io/litsearchr/
library(litsearchr)
library(ggplot2)
library(ggraph)
library(igraph)

wd <- "C:/Julian_LaCie/_GitHub/PeatlandSythesis"
paths <- paste0(wd,c("/Plots/","/export/","/Data/","/src/","/_documents/"))

plot.path <- paths[1]
export.path <- paths[2]
data.path <- paths[3]

# Experimenting with litsearchr  - search terms ---------------------------
## write and conduct naive search (after inital pull?)
# search_string <- "Argentina OR Bolivia OR Brazil OR Chile OR Columbia OR Ecuador OR Guyana OR Paraguay OR Peru OR Suriname OR Uruguay OR Venezuela"
# 
# # Split on " OR " and trim whitespace
# countries <- strsplit(search_string, " OR ")[[1]]
# countries <- trimws(countries)

geography <- c("Argentina", "Bolivia", "Brazil", "Chile", "Columbia", "Ecuador", 
               "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")
demographic <- c("peatland AND high elevation", "peatland AND low elevation","peatland NOT wetland")
intervention <- c("(Carbon OR carbon dioxide OR methane)",
                  "CO2 OR CH4",
                  "Peat* depth",
                  "Peat* subsidence",
                  "Peat* oxidation",
                  "Peat* decomposition",
                  "Peat* chemistry",
                  'peat* "greenhouse gas*" OR GHG',
                  "DOC OR dissolved organic carbon",
                  "Peat* AND (Climate Change OR global warming)")
timescale <- seq(2000,2024,1)


mysearchterms <- list(
  geography,
  demographic,
  intervention
)

my_search <-
  litsearchr::write_search(
    groupdata = mysearchterms,
    languages = "English",
    stemming = TRUE,
    closure = "none",
    exactphrase = TRUE,
    writesearch = FALSE,
    verbose = TRUE
  )
my_search

title_search <- litsearchr::write_title_search(titles=my_search)



# from Scopus output ------------------------------------------------------
# naive search - idea
papers_df <- read.csv(paste0(data.path,"2025-04-25 scopus_results_V1.csv"))

# Extract relevant text (e.g., title and abstract)
text_data <- paste(papers_df$Title, papers_df$Abstract)

# Use litsearchr functions
tokens <- extract_terms(text_data)

# 1. Identify potential keywords
rakedkeywords <-extract_terms(
    text = text_data,
    method = "fakerake",
    min_freq = 3,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )

taggedkeywords <-
  litsearchr::extract_terms(
    keywords = papers_df$Author.Keywords,
    method = "tagged",
    min_freq = 3,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )

# 2. Build the keyword co-occurrence network
all_keywords <- unique(append(taggedkeywords, rakedkeywords))

## borrowed https://luketudge.github.io/litsearchr-tutorial/litsearchr_tutorial.html

stopwords <- c(
  all_keywords[grep("result",all_keywords)],
  all_keywords[grep("rights",all_keywords)],
  all_keywords[grep("study",all_keywords)],
  all_keywords[grep("springer",all_keywords)],
  all_keywords[grep("nature",all_keywords)],
  all_keywords[grep("exclusive",all_keywords)],
  all_keywords[grep("million hectares",all_keywords)],
  all_keywords[grep("large",all_keywords)],
  all_keywords[grep("ecological society",all_keywords)],
  all_keywords[grep("british",all_keywords)],
  all_keywords[grep("samples",all_keywords)],
  all_keywords[grep("north america",all_keywords)]
)

all_stopwords <- c(get_stopwords("English"),stopwords)

all_keywords_rev <- all_keywords# unique(append(taggedkeywords, rakedkeywords))
all_keywords_rev <- all_keywords_rev[!(all_keywords_rev%in%stopwords)]


# Network Analysis in https://luketudge.github.io/litsearchr-tutorial/litsearchr_tutorial.html
docs <- text_data
dfm <- create_dfm(elements=docs, features=all_keywords_rev)

g <- create_network(dfm, min_studies=3)

ggraph(g, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE) +
  guides(edge_alpha=FALSE)

# Pruning in https://luketudge.github.io/litsearchr-tutorial/litsearchr_tutorial.html

strengths <- strength(g)

term_strengths <- data.frame(term=names(strengths), strength=(strengths))
term_strengths$rank <- rank(term_strengths$strength,ties.method = "min")|>as.numeric()
term_strengths <- term_strengths[order(term_strengths$rank),]

cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=subset(term_strengths, rank>5), hjust="right", check_overlap=TRUE,angle=-45,size=3)+
  theme_bw()

cutoff_fig 

cutoff_cum <- find_cutoff(g, method="cumulative", percent=0.8)
cutoff_cum

cutoff_fig +
  geom_hline(yintercept=cutoff_cum, linetype="dashed")

get_keywords(reduce_graph(g, cutoff_cum))
