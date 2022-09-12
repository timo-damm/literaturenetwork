#==== visualizing citations=====
#----packages-----
list.of.packages <- c("bib2df", "dplyr", "ggplot2", "tidyr" , "network", "sna",
                      "igraph", "ggraph")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("bib2df")
library("dplyr")
library("ggplot2")
library("tidyr")
library("network")
library("sna")
library("igraph")
library("ggraph")


#----data import & cleaning----

citations_df <- bib2df("/Users/damm/downloads/wicked_problems.bib", separate_names = TRUE)
citations_df$ID_title <- seq.int(nrow(citations_df))
citations_df$TITLE <- paste(citations_df$ID_title, citations_df$TITLE)
citations_df

#----descriptives-----
#most prominent journals in citation list
citations_df %>%
  filter(!is.na(JOURNAL)) %>%
  group_by(JOURNAL) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:10)

# average age of publications
citations_df %>%
  mutate(age = 2022 - YEAR) %>%
  summarize(m = mean(age))

# When did the authors publish?
citations_df %>% 
  select(YEAR, AUTHOR) %>% 
  unnest() %>% 
  ggplot() + 
  aes(x = YEAR, y = reorder(full_name, desc(YEAR)), color = YEAR) + 
  xlab("year") +
  ylab("author") +
  theme( text = element_text(size = 12,family = "Times New Roman")) +
  geom_point()

# ----network graphics with ggraph-----
test <- unnest(citations_df, "AUTHOR") 
test$ID_author <- seq.int(nrow(test))
test$full_name <- paste(test$ID_author, test$full_name)

#with authors as nodes 
test <- select(test, "TITLE", "full_name", "ID_title" , "ID_author")
graph <- graph_from_data_frame(test)
labels1 <- as.vector(test[['TITLE']])

network <- ggraph(graph, layout = 'kk') + 
  geom_edge_link(aes(colour = factor(ID_title), label = factor(ID_title),
                     family = "Times New Roman")) + 
 geom_node_point() +
  coord_fixed() +
  theme_void() +
  scale_edge_color_discrete("Title", labels = labels1) +
  theme(legend.title= element_text(size = 12, family = "Times New Roman"),
        legend.position = "bottom") 
network

#with articles as nodes
test2 <- select(test, "full_name", "TITLE", "ID_title", "ID_author")
graph2 <- graph_from_data_frame(test2)
labels2 <- as.vector(test2[['full_name']])

network2 <- ggraph(graph2, layout = 'kk') + 
  geom_edge_link(aes(colour = factor(ID_author), label = factor(ID_author))) + 
  geom_node_point() +
  coord_fixed() +
  theme_void() +
  scale_edge_color_discrete("Author", labels = labels2) +
  theme(legend.title= element_text(size = 12, family = "Times New Roman"),
        legend.position = "bottom")
network2
