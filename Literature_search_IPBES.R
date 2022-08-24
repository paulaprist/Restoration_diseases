#install.packages("remotes")
library(remotes)

#Install litsearchr 
#install_github("elizagrames/litsearchr", ref="main")
library(litsearchr)
library(igraph)
library(dplyr)
library(ggplot2)
library(here)
library(ggraph)

getwd()
setwd("C:/Users/prist/Documents/Repos_EHA/Restoration_diseases/data")

#------set directory
search_directory <- ("C:/Users/prist/Documents/Repos_EHA/Restoration_diseases/data")

#----Naive Search
#---From Scopus
#naive terms:Biodiversity AND "human health" AND water AND food AND climate

#----import txt file from our scopus search
naiveimport <-
  litsearchr::import_results(directory = search_directory, verbose = TRUE)

#-----remove duplicates
naiveresults <-
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")

#-----extract the keywords from title, abstract and keywords
rakedkeywords <-
  litsearchr::extract_terms(
    text = paste(naiveresults$title, naiveresults$abstract),
    method = "fakerake",min_freq = 2,
    ngrams = TRUE,min_n = 2,
    language = "English")

taggedkeywords <-
  litsearchr::extract_terms(keywords = naiveresults$keywords,
                            method = "tagged",min_freq = 2,
                            ngrams = TRUE,min_n = 2,
                            language = "English")

#----build the keyword co-occurrence network
all_keywords <- unique(append(taggedkeywords,rakedkeywords))##keywords identified without duplicates
docs <- paste(naiveresults[, "title"], naiveresults[, "abstract"])#joining title and abstract
dfm <- create_dfm(elements=docs, features=all_keywords)#creating a matrix that records which terms appear in which articles
dfm[1:3,1:4]##checking
g <- create_network(dfm, min_studies=20)

#-------network graph
memory.limit(size=30000)
ggraph(g, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE)+
  guides(edge_alpha=FALSE)

#-----ranking our search terms by importance
strengths <- strength(g)
st_kw <- data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) -> term_strengths #arrange the terms in ascending order of strength
head(term_strengths)##weak terms
tail(term_strengths)##strong terms

#----creating a graph to make a cut
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=dplyr::filter(term_strengths, rank>20), hjust="right", nudge_y=30, check_overlap=TRUE)
cutoff_fig

#----retaining 95% of the search terms
cutoff_cum <- litsearchr::find_cutoff(g, method="cumulative", percent=0.95)
cutoff_cum
cutoff_fig +geom_hline(yintercept=cutoff_cum, linetype="dashed")

#-----takes only the remaining terms from the reduced network
cut <- litsearchr::get_keywords(litsearchr::reduce_graph(g, cutoff_cum))
head(cut)
length(cut)
cut

#---adding extra terms that were not selected
extra_terms <- c("zoonotic","emerging infectious disease","zoono*")

#----final_terms to be used
final_terms <- c(cut, extra_terms)
final_terms

#----grouping the terms (we removed some non sense words)
grouped_terms <-list(restoration=final_terms[c(9,10,12,13,14,21,22,25,26)],
                     zoonoses=final_terms[c(11,30,31,32)])

grouped_terms

###doing a new search
litsearchr::write_search(grouped_terms,
                         languages="English",
                         exactphrase=TRUE,
                         stemming=FALSE,
                         closure="left",
                         writesearch=T,
                         directory = here("output/"))


 

