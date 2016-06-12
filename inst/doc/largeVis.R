## ----setup,eval=T,echo=F,warning=F,error=F,message=F---------------------
# Note to reader:  Please don't steal the semi-distinctive visual style I spent several minutes creating for myself.
require(ggplot2, 
        quietly = TRUE)
require(RColorBrewer, 
        quietly = TRUE)
require(wesanderson, 
        quietly = TRUE)
knitr::opts_chunk$set(collapse = TRUE, 
                      comment = "#>")
colors_discrete <- function(x) rep(wes_palette("Darjeeling", 
                                               n = min(x, 5)), 
                                   2)[1:x]
colors_divergent_discrete <- function(x) 
  grDevices::colorRampPalette(RColorBrewer::brewer.pal(x, "Spectral"))
colors_continuous <-  function(x) wes_palette(name = "Zissou",
                                              n = x, 
                                              type = "continuous")

nacol <- colors_discrete(4)[4]
theme_set(
  theme_bw() %+replace%
  theme(
    legend.key.size = unit(4, "mm"), 
    legend.title = element_text(size = rel(0.8),
                              face = "bold"),
    legend.margin = unit(0, "cm"),
    legend.position = "bottom",
    legend.key.size = unit(0.5, "lines"),
    legend.text=element_text(size = unit(8, "points")), 
    axis.title.y = element_text(angle = 90),
    axis.text = element_text(size = rel(0.7)),
    plot.margin = unit(c(0, 0.5, 1, 0), "lines"), 
    axis.title = element_text(size = rel(0.8),
                              face = "bold"),
    title = element_text(size = rel(0.9))
  ) 
)
rebuild <- FALSE
if (!exists("buildManifolds")) buildManifolds <- rebuild

require(largeVis,quietly = TRUE)

## ----MNIST,echo=F,message=F,warning=F,results='hide',eval=rebuild--------
#  darch::provideMNIST(download=T)
#  load("data/train.RData")
#  
#  mnistCoords <- vis(t(trainData) - 0.5,
#                     K = 40,
#                     tree_threshold = 700,
#                     n_trees = 40,
#                     max_iter = 2,
#                     verbose=F)
#  mnistCoords <- mnistCoords$coords
#  mnistCoords <- scale(t(mnistCoords))
#  mnistCoords <- data.frame(mnistCoords)
#  colnames(mnistCoords) <- c("x", "y")
#  labs <- apply(trainLabels,
#                MARGIN = 1,
#                FUN = function(x) which(x == 1))
#  mnistCoords$labels <- factor(labs - 1)

## ----drawmnist,echo=F,warning=F------------------------------------------
load(system.file("extdata", "mnistcoords.Rda", package="largeVis"))
ggplot(mnistCoords, aes(x = x, y = y, color = labels)) +
  geom_point(size = 0.1, alpha = 0.3) +
  scale_x_continuous(name = "", limits = c(-2, 2), breaks = NULL) +
  scale_y_continuous(name = "", limits = c(-2, 2), breaks = NULL) +
  scale_color_manual(values = colors_divergent_discrete(10)(10)) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  ggtitle("MNIST")

## ----ldafromldavis,echo=F,eval=rebuild-----------------------------------
#  library(LDAvis)
#  data("TwentyNewsgroups")
#  theta <- scale(t(TwentyNewsgroups$theta))
#  set.seed(1974)
#  initialcoords <- matrix(rnorm(2 * ncol(theta)), ncol = ncol(theta))
#  visObj <- vis(theta,
#                K = 100,
#                n_trees = 20,
#                tree_threshold = 100,
#                max_iter = 2)
#  
#  ngcoords <- scale(t(visObj$coords))
#  ngcoords <- data.frame(ngcoords)
#  colnames(ngcoords) <- c("x", "y")
#  library(lda)
#  data("newsgroup.train.labels")
#  ngcoords$label <- factor(newsgroup.train.labels)[-1]

## ----draw20ng,echo=F,warning=FALSE,error=FALSE,message=FALSE-------------
load(system.file("extdata", "ngcoords.Rda", package = "largeVis"))
ggplot(ngcoords, 
       aes(x = x, 
           y = y, 
           color = label)) +
  geom_point(size = 0.4, alpha = 0.5) + 
  scale_color_manual(values = colors_divergent_discrete(20)(20),
                     guide=FALSE) +
  scale_x_continuous(name = "", limits = c(-2, 2.5), breaks = NULL) +
  scale_y_continuous(name = "", limits = c(-2, 2.5), breaks = NULL) +
  ggtitle("20 Newsgroups")

## ----wikiterms,eval=F,echo=F---------------------------------------------
#  # # The data file must be obtained directly from the paper authors
#  wikiwords <- readr::read_delim("/mnt/hfsshare/DATASETS/Wiki_embedding/word_vec",delim= " ", col_names = FALSE, skip = 1)
#  wikiwords <- t(as.matrix(wikiwords[, 2:101]))
#  set.seed(1974)
#  initcoords <- matrix(rnorm(ncol(wikiwords) * 2), nrow = 2)
#  wikiVis <- vis(wikiwords,
#                 coords = initcoords,
#                 K = 100,
#                 tree_threshold = 100,
#                 n_trees = 50)
#  wikiwords <- readr::read_delim("/mnt/hfsshare/DATASETS/Wiki_embedding/word_vec",delim= " ", col_names = FALSE, skip = 1)
#  wikiwords <- wikiwords[,1]$X1
#  wikilabels <- readr::read_delim("/mnt/hfsshare/DATASETS/Wiki_embedding/wiki_word_label.txt",
#                                  col_names = FALSE, delim = "\t")
#  words <- data.frame(word = wikiwords,
#                      labelrow = match(wikiwords, wikilabels$X1))
#  words$label <- wikilabels$X2[words$labelrow]
#  words <- cbind(words,
#                 data.frame(t(wikiVis$coords)))
#  colnames(words) <- c('word', 'labelrow', 'label', 'x', 'y')

## ----drawwikiwords,echo=F,eval=T,warning=F,message=F---------------------
load(system.file("extdata", "wikiwords.Rda", package = "largeVis"))
ggplot(words, aes(x = x, y = y, color = label)) + 
  geom_point(size = 0.01, alpha = 0.5) +
  scale_color_gradientn(colors = colors_divergent_discrete(11)(11)) +
  guides(color = FALSE) +
  scale_x_continuous("", limits = c(-38, 38), breaks = NULL) + 
  scale_y_continuous("", limits = c(-38, 38), breaks = NULL) +
  ggtitle(expression(atop("Wiki Word Vectors", atop(paste(sep = " ", "K = 100,", gamma, 
                           "= 7, M = 5,", alpha, "= 1, n_trees = 50,",
                           "max_iters = 1, threshold = 100")))))

## ----wikihyperparameters,echo=F,eval=rebuild-----------------------------
#  data(wiki)
#  
#  inputs <- data.frame(
#    g = rep(c(.5,1,7,14), 4),
#    a = rep(c(.1,1,5,10), each = 4)
#  )
#  set.seed(1974)
#  initialcoords <- matrix(rnorm(ncol(wiki) * 2), nrow = 2)
#  
#  agcoords <- do.call(rbind,
#                      lapply(1:nrow(inputs),
#                             FUN = function(x) {
#    a <- inputs[x, 'a']
#    g <- inputs[x, 'g']
#    newcoords <- initialcoords
#    localcoords <- projectKNNs(wiki,
#                               alpha =  a,
#                               gamma = g,
#                               verbose = FALSE,
#                               coords = newcoords)
#    localcoords <- data.frame(scale(t(localcoords)))
#    colnames(localcoords) <- c("x", "y")
#    localcoords$a <- a
#    localcoords$g <- g
#    localcoords$activity <- log(Matrix::colSums(wiki))
#    localcoords
#  }))

## ----drawhyperparameters,echo=F,fig.width=3.5,fig.height=4,fig.align='center'----
load(system.file("extdata", "agcoords.Rda", package="largeVis"))
ggplot(agcoords,
       aes(x = x, 
           y = y, 
           color = activity)) +
  geom_point(alpha = 0.2, 
             size = 0.05) +
  facet_grid(a ~ g,
             labeller = label_bquote(alpha == .(a), 
                                     gamma == .(g)),
             scales = 'free') +
  scale_x_continuous(breaks = NULL, 
                     name = "") +
  scale_y_continuous(breaks = NULL, 
                     name = "") +
  scale_color_gradientn(colors = colors_continuous(10), 
                        guide=FALSE) +
  ggtitle(expression(paste("Effect of ", alpha, " vs. ", gamma, sep = "  ")))

## ----iris,echo=F,fig.width=5,fig.height=5,eval=rebuild-------------------
#  data(iris)
#  Ks <- c(5, 10, 20, 30)
#  Ms <- c(1, 5, 10, 20)
#  data(iris)
#  dat <- iris[,1:4]
#  dupes <- duplicated(dat)
#  dat <- dat[-dupes,]
#  labels <- iris$Species[-dupes]
#  dat <- scale(dat)
#  dat <- as.matrix(dat)
#  dat <- t(dat)
#  
#  set.seed(1974)
#  coordsinput <- matrix(rnorm(ncol(dat) * 2), nrow = 2)
#  
#  iriscoords <- do.call(rbind, lapply(Ks, FUN = function(K) {
#    neighbors <- randomProjectionTreeSearch(dat,
#                                            K = K,
#                                            verbose = FALSE)
#    neighborIndices <- neighborsToVectors(neighbors)
#    distances <- largeVis::distance(x = dat,
#                                    neighborIndices$i,
#                                    neighborIndices$j)
#    wij <- buildEdgeMatrix(i = neighborIndices$i,
#                         j = neighborIndices$j,
#                         d = distances)
#    do.call(rbind, lapply(Ms, FUN = function(M) {
#      coords <- projectKNNs(wij = wij$wij, M = M, coords = coordsinput)
#      coords <- scale(t(coords))
#      coords <- data.frame(coords)
#      colnames(coords) <- c("x", "y")
#      coords$K <- K
#      coords$M <- M
#      coords$Species <- as.integer(labels)
#      coords
#    }))
#  }))
#  
#  iriscoords$Species <- factor(iriscoords$Species)
#  levels(iriscoords$Species) <- levels(iris$Species)

## ----drawiriscoords,echo=F,fig.width=4,fig.height=4.5,fig.align='center'----
load(system.file("extdata", "iriscoords.Rda", package="largeVis"))
ggplot(iriscoords,
       aes(x = x,
           y = y,
           color = Species)) +
         geom_point(size = 0.5) +
  scale_x_continuous("", 
                     breaks = NULL) +
  scale_y_continuous("", 
                     breaks = NULL) +
  facet_grid(K ~ M, 
             scales = 'free', 
             labeller = label_bquote(K == .(K), M == .(M))) +
  scale_color_manual(values = colors_discrete(3)) +
  ggtitle("Effect of M and K on Iris Dataset")

## ----loadmnistimages,eval=rebuild,echo=F---------------------------------
#  load("data/train.RData")

## ----drawmanifoldmap,echo=T,fig.width=8,fig.height=8,message=F,warning=F,fig.align='center'----
par(mai=c(0.25, 0.25, 0.25, 0.25))
if (exists("trainData")) {
  dim(trainData) <- c(60000, 28, 28)
  set.seed(1974)
  manifoldMap(mnistCoords[,1:2],
      n = 5000,
      scale = 0.003,
      images = trainData,
      xlab = "", 
      ylab = "",
      xlim = c(-2, 2),
      ylim = c(-2, 2))
} 

## ----nomnist,eval=!buildManifolds,echo=F,results='asis'------------------
#  cat("The plot is disabled by default because it requires the MNIST dataset.  To recreate the plot, change the vignette script to point to the downloaded images.\n")
#  cat("The MNIST data may be obtained using the `darch` package, available on CRAN, with the commands `provideMNIST(folder = 'download location', download = TRUE)` followed by `readMNIST(folder = 'download location')`")

## ----lfw,echo=F,eval=rebuild---------------------------------------------
#  data("facevectors")
#  
#  faceembeddings <- t(as.matrix(facevectors[,-c(1:2)]))
#  faceVis <- vis(faceembeddings, K = 50,
#                 max_iter = 2,
#                 n_trees = 100,
#                 distance_method = 'Cosine')
#  
#  faceCoords <- data.frame(scale(t(faceVis$coords)))
#  colnames(faceCoords) <- c("x", "y")
#  faceCoords <- cbind(faceCoords, facevectors[,1:2])

## ----plotFaceVectors,echo=F,fig.width=7,fig.height=5---------------------
load(system.file("extdata", "faces.Rda", package="largeVis"))
faceCoCopy <- faceCoords
lvs <- c("Tony_Bennette", 
         "Gloria_Gaynor", 
         "Jennifer_Aniston", 
         "Kobe_Bryant", 
         "John_Nash",
         "Jack_Smith", 
         "Nancy_Kerrigan", 
         "Nora_Ephron",
         "Julianna_Margulies", 
         "Abdullah_al-Attiyah")
faceCoCopy$name[! faceCoCopy$name %in% lvs] <- "Other"
faceCoCopy$name <- factor(faceCoCopy$name)
faceCoCopy$alpha <- factor(ifelse(faceCoCopy$name == "Other", 0.05, 0.2))

ggplot(faceCoCopy, aes(x = x, 
                       y = y, 
                       color = name,
                       alpha = alpha,
                       size = alpha)) + 
  geom_point() +
  scale_alpha_manual(values = c(0.2, 0.8), guide = FALSE) + 
  scale_color_manual(values = c("grey", colors_divergent_discrete(10)(10))) +
  scale_size_manual(values = c(0.2, 0.5), guide = FALSE) +
  scale_x_continuous("", 
                     breaks = NULL) +
  scale_y_continuous("", 
                     breaks = NULL) +
  ggtitle("Positions of Embedding Vectors for Selected Individuals")

## ----faceImages,eval=buildManifolds,echo=F,fig.width=8,fig.height=8------
load(system.file("extdata", "faceLabels.Rda", package="largeVis"))
library(jpeg)
set.seed(1974)
n <- 500
facesToPlot <- sample(nrow(faceCoords), n)

faces <- apply(faceLabels[facesToPlot,], MARGIN = 1, FUN = function(x) {
  ret <- readJPEG(paste("/mnt/hfsshare/DATASETS/lfw faces/lfw",
                x[1], sub("png", "jpg", x[2]), sep = "/"))
  dim(ret) <- c(250, 250, 3)
  ret
})
dim(faces) <- c(250, 250, 3, n)
faces <- aperm(faces, c(4,1,2,3)) # n,h,w,c
par(bg = 'grey5', mai=c(0.25, 0.25, 0.25, 0.25))
ggManifoldMap(
  x = faceCoords[facesToPlot,1:2], 
  n = n, 
  images = 1 - faces, 
  scale =  1 / 1000) +
  scale_y_continuous(name = "", breaks = NULL) +
  scale_x_continuous(name = "", breaks = NULL) + 
  ggtitle("Manifold Map of OpenFace Embeddings")

## ----faceImages5000,eval=buildManifolds,echo=F,results='asis'------------
png(filename = "faceshighres.png", 
    width = 5000, height = 5000, units = 'px', 
    bg = 'grey5')
manifoldMap(x = faceCoords[facesToPlot,1:2], 
            n = n, images = 1 - faces, scale =  1 / 1000,
            xlab = NULL, ylab = NULL, col.lab = 'gray5',
            col.axis = 'gray5')
cat("A high resolution version is available [here](vignettes/faceshighres.png)")

## ----noface,eval=!buildManifolds,results='asis',echo=F-------------------
#  cat("The plot is disabled by default because it requires face images from [Labeled Faces in the Wild](http://vis-www.cs.umass.edu/lfw/). To recreate the plot, change the vignette script to point to the downloaded images.")

## ----stm,echo=F,eval=rebuild---------------------------------------------
#  library(stm)
#  data("poliblog5k")
#  p <- c(0, cumsum(as.numeric(lapply(poliblog5k.docs, function(x) ncol(x)))))
#  i <- do.call("c", lapply(poliblog5k.docs, function(x) x[1,]))
#  p[length(p)] <- length(i)
#  j <- rep(0:(length(diff(p)) - 1), diff(p))
#  v <- do.call("c", lapply(poliblog5k.docs, function(x) x[2,]))
#  poli <- Matrix::sparseMatrix(i = i + 1,
#                               j = j + 1,
#                               x = v)
#  dupes <- duplicated(slam::as.simple_triplet_matrix(Matrix::t(poli)))
#  poli <- poli[, ! dupes]
#  poli <- poli / log(Matrix::rowSums(poli > 0)) # tf-idf weight
#  set.seed(1974)
#  inputcoords <- matrix(rnorm(10000), nrow = 2)
#  policoords <- vis(poli,
#                    K = 100,
#                    n_trees = 50,
#                    tree_threshold = 100,
#                    max_iter = 1,
#                    M = 10,
#                    distance_method = 'Cosine',
#                    coords = inputcoords,
#                    verbose = FALSE)
#  
#  stmmodel <- stm(poliblog5k.docs, poliblog5k.voc, K = 20,
#                  data = poliblog5k.meta, prevalence = ~ rating + s(day),
#                  content = ~rating,
#                  max.em.its = 75, init.type="Spectral", seed = 1974)
#  
#  stmvectors <- t(scale(stmmodel$theta))
#  
#  set.seed(1974)
#  inputcoords <- matrix(rnorm(10000), nrow = 2)
#  stmVis <- vis(stmvectors,
#                    K = 100,
#                    n_trees = 50,
#                    tree_threshold = 100,
#                    max_iter = 1,
#                    M = 10,
#                    distance_method = 'Cosine',
#                    coords = inputcoords,
#                    verbose = FALSE)
#  
#  polidata <- data.frame(scale(t(policoords$coords)))
#  colnames(polidata) <- c('x', 'y')
#  polidata$rating <- poliblog5k.meta$rating[!dupes]
#  polidata$blog <- poliblog5k.meta$blog[!dupes]
#  
#  stmdata <- data.frame(scale(t(stmVis$coords)))
#  colnames(stmdata) <- c('x', 'y')
#  stmdata$rating <- poliblog5k.meta$rating
#  stmdata$blog <- poliblog5k.meta$blog
#  
#  polidata$origin <- "tf-idf term vectors"
#  stmdata$origin <- "stm topic vectors"
#  
#  combined <- rbind(polidata,
#                    stmdata)
#  combined$origin <- factor(combined$origin)
#  combined$origin <- factor(combined$origin,
#                            levels = rev(levels(combined$origin)))

## ----drawtdm,echo=F,fig.height=5.5,fig.width=7,warning=FALSE,message=FALSE,error=FALSE----
load(system.file("extdata", "poliblog.Rda", package="largeVis"))

ggplot(combined, aes(x = x, 
                     y = y, 
                     color = blog)) +
  geom_point(size = 0.2, 
             alpha = 0.8) +
  scale_color_manual(values = colors_divergent_discrete(6)(6)) +
  facet_grid(origin ~ rating, 
             scale = 'free') +
  scale_x_continuous("", 
                     breaks = NULL, 
                     limits = c(-2.5, 2.5)) +
  scale_y_continuous("", 
                     breaks = NULL,
                     limits = c(-2.5, 2.5)) +
  ggtitle("5000 Political Blog Entries")

## ----youtube,eval=F,echo=T-----------------------------------------------
#  pathToGraphFile <-
#    "/mnt/hfsshare/DATASETS/YouTubeCommunities/com-youtube.ungraph.txt"
#  pathToCommunities <-
#    "/mnt/hfsshare/DATASETS/YouTubeCommunities/com-youtube.top5000.cmty.txt"
#  
#  youtube <- readr::read_tsv(pathToGraphFile, skip=4, col_names=FALSE)
#  youtube <- as.matrix(youtube)
#  youtube <- Matrix::sparseMatrix(i = youtube[, 1],
#                                  j = youtube[, 2],
#                                  x = rep(1, nrow(youtube)),
#                                  dims = c(max(youtube), max(youtube)))
#  youtube <- youtube + t(youtube)
#  communities <- readr::read_lines(pathToCommunities)
#  communities <- lapply(communities,
#                        FUN = function(x) as.numeric(unlist(strsplit(x, "\t"))))
#  community_assignments <- rep(0,
#                               nrow(youtube))
#  for (i in 1:length(communities)) community_assignments[communities[[i]]] <- i
#  
#  youTube_coordinates <- projectKNNs(youtube)
#  youTube_coordinates <- data.frame(scale(t(youTube_coordinates)))
#  colnames(youTube_coordinates) <- c("x", "y")
#  youTube_coordinates$community <- factor(community_assignments)
#  youTube_coordinates$alpha <- factor(ifelse(youTube_coordinates$community == 0, 0.05, 0.2))
#  ggplot(youTube_coordinates, aes( x = x,
#                        y = y,
#                        color = community,
#                        alpha = alpha,
#                        size = alpha)) +
#    geom_point() +
#    scale_color_manual(values =
#                         c("black", colors_continuous(5000)),
#                       guide = FALSE) +
#    scale_alpha_manual(values = c(0.005, 0.2), guide = FALSE) +
#    scale_size_manual(values = c(0.03, 0.15), guide = FALSE) +
#    scale_x_continuous("",
#                       breaks = NULL, limits = c(-2.5,2.5)) +
#    scale_y_continuous("",
#                       breaks = NULL, limits = c(-2.5,2.5)) +
#    ggtitle("YouTube Communities")

## ----lowmemexample,eval=F,echo=T-----------------------------------------
#  neighbors <- randomProjectionTreeSearch(largeDataset)
#  neighborIndices <- neighborsToVectors(neighbors)
#  rm(neighbors)
#  gc()
#  distances <- distance(x = largeDataset,
#                        i = neighborIndices$i,
#                        j =neighborIndices$j)
#  rm(largeDataset)
#  gc()
#  wij <- buildEdgeMatrix(i = neighborIndices$i,
#                         j = neighborIndices$j,
#                         d = distances)
#  rm(distances, neighborIndices)
#  gc()
#  coords <- projectKNNs(wij$wij)

