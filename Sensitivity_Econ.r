#Procedures to decompose an adjacency list into a weighted graph
packages = c("splitstackshape", "stringr", "igraph", "spdep", "magrittr", "htmlwidgets", "htmltools", "plotly")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

a<-read.csv("C:\\Users\\msgc\\Desktop\\Econscopus.csv")
table(a$Authors=="")
table(a$Authors!="[No author id available]")
a <- a[a$Authors!="",]

#Keeping only column with author relationships
authors<-as.data.frame(a[,1])#"Author.s..ID"])
colnames(authors)<-"AU"

#package for first split
# install.packages("splitstackshape")
 authors$AU <- str_remove_all(authors$AU, "[^[\\a-zA-Z ]]")

#As can be seen in the file the separator of interest is :
a1<-cSplit(authors, splitCols = "AU", sep = ";", direction = "wide", drop = TRUE) #retain the matrix form version of the adjacency list input
 # fix(a1)
class(a1)

# In case package cannot be installed uncomment and keep working using this version, alternatively you can decompose the cells into columns using excel

#read it as a matrix
mat <- as.matrix(a1)
# mat

dim(mat)# the resulting column dimension is the number of times you will have to repeat the following procedure minus 1
mat <- cbind(a$EID, mat)
edgelist1<-matrix(NA, 1, 2)#empty matrix two columns
# for (i in 1:(ncol(mat)-1)) {# for coauthors
for (i in 1:1) {
  edgelist11 <- cbind(mat[, i], c(mat[, -c(1:i)]))
  edgelist1 <- rbind(edgelist1,edgelist11)
  edgelist1<-edgelist1[!is.na(edgelist1[,2]),]
  edgelist1<-edgelist1[edgelist1[,2]!="",]
  }
dim(edgelist1)

# install.packages("igraph")

g<- graph.data.frame(edgelist1[, 2:1], directed = FALSE)
V(g)$type <- V(g)$name %in% edgelist1[ , 2]
table(V(g)$type)
i<-table(V(g)$type)[2]

#Transformations to retain actors

mat_g2_incidence <- t(get.incidence(g))
# This removes papers with only one author
# dim(mat_g2_incidence[,colSums(mat_g2_incidence)>1])
# mat_g2_incidence <- mat_g2_incidence[,colSums(mat_g2_incidence)>1]
dta <- data.frame(id=rownames(mat_g2_incidence), count=rowSums(mat_g2_incidence))
dta <- dta[order(dta$count, decreasing=T), ]


#solo authors
a1 <- as.matrix(a1)
a$author_number <- ncol(a1)-rowSums(is.na(a1))
a$author_binary <- ifelse(a$author_number>1, 0, 1)
summary(lm(Cited.by~author_binary, data=a))

#Load without selfcitation
b <- read.csv("C:\\Users\\msgc\\Desktop\\econallPHUDCFILY.csv")
a$no_self <- b$Total[match(a$ISSN, b$ISSN)]
a$no_self1 <- b$Total[match(a$Title, b$Document.Title)]
head(a[,c("no_self1","cit","author_binary")],100)

a <- a[!is.na(a$no_self1),]
table(is.na(a$no_self1), a$author_binary) 
summary(lm(Cited.by~author_binary, data=a))
summary(lm(no_self1~author_binary, data=a))

a$inflation <- a$Cited.by -  a$no_self1
a <- a[a$inflation>=0,]

summary(lm(Cited.by~author_binary+author_number, data=a))
summary(lm(no_self1~author_binary+author_number, data=a))

a$Open.Access <- ifelse(a$Open.Access=="", 0, 1)
summary(lm(Cited.by~author_binary+Open.Access, data=a))
