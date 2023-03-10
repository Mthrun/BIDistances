Dist2All <- function (X, Data, SelectFeatures, method = "euclidean",p=2,knn=1) {
# Liefert die Distanzen eines Vektors X zu allen Zeilen einer Matrix Data
  # V <- Dist2All(X,A)
  # Input:
  # X		Vector [1:n]; numeric;
  # Data		Matrix [1:n, 1:d]; numeric;
  # SelectFeatures [1:d] element equals 1 if feature should be considered, otherwise zero
  # method
  # p               scalar, The ppth root of the sum of the ppth powers of the differences of the components.
  # knn             scalar, which are the k nearest neighbors in Data
  # Output:
  #List V with
  # dist2All [1:n]; numeric;  Distance of X to every row of Data
  # KNN  [1:knn]    knn indices of the k nearest neighbors in Data

  #Author MCT, 2023
  if(!is.matrix(Data)){
    Data=as.matrix(Data)
    warning('dist2All: Data has to be matrix. applying as.matrix')
  }
  SelectFeatures = rep(1, ncol(Data))
  if(mode(Data)!="numeric"){
    mode(Data)=="numeric"
    warning('dist2All: Data has to be numeric matrix. applying numerical mode.')
    }
  if(length(X)!=ncol(Data)){stop('dist2All: X has to be of same length as cols of A, i.e. number of colums == length X. Function stops!')}
  if(mode(X)!=mode(Data)){
    mode(X)=="numeric"
    warning('dist2All: X and Data have to be numeric vector and matrix respectively. Function stops!')

    }
  if(sum(is.finite(X))!=length(X)){
    stop('dist2All: X has values not are not finite. Function stops!')
  }
  xData <- rbind(X, Data)
  xData <- xData[, which( SelectFeatures == 1)]
  #hier spaeter GPU version
  if( isTRUE(requireNamespace('parallelDist'))){
    distToAll <- as.vector(parallelDist::parallelDist(xData, method = method,p=p))[1:nrow(Data)]

  }else{
    warning("dist2All: parallelDist not installed, faling back to minkowski distances.")
    dist2x <- apply(Data[, which( SelectFeatures == 1)], 1, function(row,X){sum((row-X)^p)},X,p) # Summe der quadrierte Differenzen  zu  X
    distToAll = (dist2x)^(1/p)         # p=2, Euclid-sche Distanzen der Daten    zu X


  }

  return(list("distToAll" = distToAll,
              "KNN"       = order(distToAll,decreasing = F)[1:knn]))
}
