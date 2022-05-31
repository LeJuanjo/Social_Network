




#LOAD LIBRARIES\

library(igraph)
library(NetData)


#LOAD THE DATA

setwd("D:/OneDrive IESEG/OneDrive - IESEG/Documents/network_ind")

florentine <- read.table("Florentine_M.txt")
florentine

#import the data into a ""graph"" object
florentine_graph <- graph_from_adjacency_matrix(as.matrix(florentine))
#florentine_graph <- graph.data.frame(florentine) 
summary(florentine_graph)
plot(florentine_graph)
#CENTRALITY MEASURES
#Degree
deg_florentine <- igraph::degree(florentine_graph) 
deg_florentine
hist(deg_florentine)
plot(florentine_graph,vertex.size=deg_florentine*3)


#CLOSENESS

florentine_graph_close <- closeness(florentine_graph)
hist(florentine_graph_close)
florentine_graph_close
plot(deg_florentine, florentine_graph_close)
plot(florentine_graph,vertex.size=florentine_graph_close*1000)

#BETWEENESS

Florentine_bet <- betweenness(florentine_graph)
hist(Florentine_bet)
Florentine_bet
plot(deg_florentine, Florentine_bet)
plot(florentine_graph,vertex.size=Florentine_bet*0.5)
#EIGENVECTOR


Florentine_eigen <- centr_eigen(florentine_graph)
hist(Florentine_eigen$vector)
Florentine_eigen$vector
plot(deg_florentine, Florentine_eigen$vector,lty = 3, lwd = 3)
plot(florentine_graph,vertex.size=Florentine_eigen$vector*35)
#GOOGLE PAGE RANK


Florentine_page <- page_rank(florentine_graph)
hist(Florentine_page$vector)
Florentine_page$vector
plot(deg_florentine, Florentine_page$vector)
plot(florentine_graph,vertex.size=Florentine_page$vector*300)
#BONACICH ALPHA


Florentine_bona <- alpha_centrality(florentine_graph)
hist(Florentine_bona)
plot(deg_florentine, Florentine_bona)
color = ifelse(Florentine_bona>0,1,2)
plot(florentine_graph,vertex.size=abs(Florentine_bona)*3,vertex.color=color)
Florentine_bona

CEN_advise = cbind(deg_florentine, florentine_graph_close, Florentine_bet, Florentine_eigen$vector,Florentine_page$vector,Florentine_bona)

COR_advise= cor(CEN_advise)
colnames(COR_advise) = c('DEGR', 'CLOSE', 'BETW', 'EIGEN',"PAGE","BONACICH")
rownames(COR_advise) = c('DEGR', 'CLOSE', 'BETW', 'EIGEN',"PAGE","BONACICH")

COR_advise
##

names(florentine)[which.max(deg_florentine)]
names(florentine)[which.min(deg_florentine)]

names(florentine)[which.max(florentine_graph_close)]
names(florentine)[which.min(florentine_graph_close)]

names(florentine)[which.max(Florentine_bet)]
names(florentine)[which.min(Florentine_bet)]

names(florentine)[which.max(Florentine_eigen$vector)]
names(florentine)[which.min(Florentine_eigen$vector)]

names(florentine)[which.max(Florentine_page$vector)]
names(florentine)[which.min(Florentine_page$vector)]

names(florentine)[which.max(Florentine_bona)]
names(florentine)[which.min(Florentine_bona)]
