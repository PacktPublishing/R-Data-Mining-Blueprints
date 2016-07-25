 library("recommenderlab")
 data(Jester5k)
 Jester5k@data@Dimnames[2]
 data<-sample(Jester5k,2000)
 hist(getRatings(data),breaks=100,col="blue")
 hist(getRatings(normalize(data)),breaks=100,col="blue4")
 recommenderRegistry$get_entries(dataType = "realRatingMatrix")
 rc <- Recommender(Jester5k, method = "POPULAR")
 rc
 names(getModel(rc))
 getModel(rc)$topN
recom <- predict(rc, Jester5k, n=5)
recom
 head(as(recom,"list"))
 rc <- Recommender(Jester5k, method = "IBCF")
 rc
 recom <- predict(rc, Jester5k, n=5)
 recom
 head(as(recom,"list"))
 rc <- Recommender(Jester5k, method = "RANDOM")
 rc
 recom <- predict(rc, Jester5k, n=5)
 recom
 head(as(recom,"list"))
 rc <- Recommender(Jester5k, method = "SVD")
 rc
 recom <- predict(rc, Jester5k, n=5)
 recom
 head(as(recom,"list"))
 rc <- Recommender(Jester5k, method = "UBCF")
 rc
 recom <- predict(rc, Jester5k, n=5)
 recom
 head(as(recom,"list"))
 e <- evaluationScheme(Jester5k, method="split",
                         train=0.9,given=15, goodRating=5)
 e
 #User based collaborative filtering
   r1 <- Recommender(getData(e, "train"), "UBCF")
 #Item based collaborative filtering
   r2 <- Recommender(getData(e, "train"), "IBCF")
 #PCA based collaborative filtering
   #r3 <- Recommender(getData(e, "train"), "PCA")
   #POPULAR based collaborative filtering
   r4 <- Recommender(getData(e, "train"), "POPULAR")
 #RANDOM based collaborative filtering
   r5 <- Recommender(getData(e, "train"), "RANDOM")
 #SVD based collaborative filtering
   r6 <- Recommender(getData(e, "train"), "SVD")
 #Predicted Ratings
   p1 <- predict(r1, getData(e, "known"), type="ratings")
 p2 <- predict(r2, getData(e, "known"), type="ratings")
 #p3 <- predict(r3, getData(e, "known"), type="ratings")
   p4 <- predict(r4, getData(e, "known"), type="ratings")
 p5 <- predict(r5, getData(e, "known"), type="ratings")
 p6 <- predict(r6, getData(e, "known"), type="ratings")
 #calculate the error between the prediction and 
   #the unknown part of the test data
   error <- rbind(
      calcPredictionAccuracy(p1, getData(e, "unknown")),
      calcPredictionAccuracy(p2, getData(e, "unknown")),
      #calcPredictionAccuracy(p3, getData(e, "unknown")),
        calcPredictionAccuracy(p4, getData(e, "unknown")),
      calcPredictionAccuracy(p5, getData(e, "unknown")),
      calcPredictionAccuracy(p6, getData(e, "unknown"))
      )
 rownames(error) <- c("UBCF","IBCF","POPULAR","RANDOM","SVD")
 error
 #Evaluation of a top-N recommender algorithm
   scheme <- evaluationScheme(Jester5k, method="cross", k=4,
                                given=3,goodRating=5)
 scheme
 results <- evaluate(scheme, method="POPULAR", n=c(1,3,5,10,15,20))
 results <- evaluate(scheme, method="IBCF", n=c(1,3,5,10,15,20))
 results <- evaluate(scheme, method="UBCF", n=c(1,3,5,10,15,20))
 results <- evaluate(scheme, method="RANDOM", n=c(1,3,5,10,15,20))
 results <- evaluate(scheme, method="SVD", n=c(1,3,5,10,15,20))
 getConfusionMatrix(results)[[1]]
