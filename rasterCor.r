library(spatialEco)
library(raster)
# library(spgwr)

################
### 関数の定義箇所
################
makeCo<- function(dimx, dimy) {
    #x座標の作成
    coX1 <- data.frame(LON = rep(1, each = dimy))
    for(i in 2:dimx){
        coX2 <- data.frame(LON = rep(i, each = dimy))
        coX1 <- rbind(coX1, coX2)
    }

    #y座標の作成
    coY1 <- data.frame(LAT = dimy:1)
    coY2 <- data.frame(LAT = dimy:1)
    for(i in 2:dimx){
        coY1 <- rbind(coY1, coY2)
    }

    #点座標の作成
    co = cbind(coX1, coY1)
    return(co)
}

gridcorts <- function(rasterstack, method, type=c("corel","pval","both")){
  # Values for (layers, ncell, ncol, nrow, method, crs, extent) come straight from the input raster stack
  # e.g. nlayers(rasterstack), ncell(rasterstack)... etc.
  print(paste("Start Gridcorts:",Sys.time()))
  print("Loading parameters")
  layers=nlayers(rasterstack);ncell=ncell(rasterstack);
  ncol=ncol(rasterstack);nrow=nrow(rasterstack);crs=crs(rasterstack);
  extent=extent(rasterstack);pb = txtProgressBar(min = 0, max = ncell, initial = 0)
  print("Done loading parameters")
  mtrx <- as.matrix(rasterstack,ncol=layers)
  empt <- matrix(nrow=ncell, ncol=2)
  print("Initiating loop operation")
  if (type == "corel"){
    for (i in 1:ncell){
      setTxtProgressBar(pb,i)
      if (all(is.na(mtrx[i,1:(layers/2)])) | all(is.na(mtrx[i,((layers/2)+1):layers]))){ 
        empt[i,1] <- NA 
      } else 
        if (sum(!is.na(mtrx[i,1:(layers/2)]/mtrx[i,((layers/2)+1):layers])) < 4 ){
          empt[i,1] <- NA 
        } else 
          empt[i,1] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$estimate)
    }
    print("Creating empty raster")
    corel <- raster(nrows=nrow,ncols=ncol,crs=crs)
    extent(corel) <- extent
    print("Populating correlation raster")
    values(corel) <- empt[,1]
    print(paste("Ending Gridcorts on",Sys.time()))
    corel
  } 
  else
    if (type == "pval"){
      for (i in 1:ncell){
        setTxtProgressBar(pb,i)
        if (all(is.na(mtrx[i,1:(layers/2)])) | all(is.na(mtrx[i,((layers/2)+1):layers]))){ 
          empt[i,2] <- NA 
        } else 
          if (sum(!is.na(mtrx[i,1:(layers/2)]/mtrx[i,((layers/2)+1):layers])) < 4 ){
            empt[i,2] <- NA 
          } else 
            empt[i,2] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$p.value)
      }
      pval <- raster(nrows=nrow,ncols=ncol,crs=crs)
      extent(pval) <- extent
      print("Populating significance raster")
      values(pval) <- empt[,2]
      print(paste("Ending Gridcorts on",Sys.time()))
      pval
    }
  else
    if (type == "both"){
      for (i in 1:ncell){
        setTxtProgressBar(pb,i)
        if (all(is.na(mtrx[i,1:(layers/2)])) | all(is.na(mtrx[i,((layers/2)+1):layers]))){ 
          empt[i,] <- NA 
        } else 
          if (sum(!is.na(mtrx[i,1:(layers/2)]/mtrx[i,((layers/2)+1):layers])) < 4 ){
            empt[i,] <- NA 
          } else {
            empt[i,1] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$estimate) 
            empt[i,2] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$p.value)
          }
      }
      c <- raster(nrows=nrow,ncols=ncol,crs=crs)
      p <- raster(nrows=nrow,ncols=ncol,crs=crs)
      print("Populating raster brick")
      values(c) <- empt[,1]
      values(p) <- empt[,2]
      brk <- brick(c,p)
      extent(brk) <- extent
      names(brk) <- c("Correlation","Pvalue")
      print(paste("Ending Gridcorts on",Sys.time()))
      brk
    }
}

#ラスタ画像の読み取り
# A <- raster(file.path("Unity.grd"))
# B <- raster(file.path("Dep.grd"))

A <- raster(file.path("Depthmap_Foot.tif"))
B <- raster(file.path("Unity_Foot.tif"))



a <- rasterCorrelation(A, B, s = 5, type = "spearman")

plot(a)
# UNITY <- values(A)
# AA <- values(B)
# AA <- replace(AA, AA<0, 0)


# id <- 1:length(AA)

# UNITY <- cbind(id, UNITY)
# AA <- cbind(id, AA)

# #大きさを取得
# dimA <- dim(A)
# dimB <- dim(B)



# #座標の作成
# co <- makeCo(dimA[1], dimA[2])
# co <- cbind(id, co)



# #欠損値を削除（建物部分）
# AA <- na.omit(AA)

# # length(co)
# # head(co)
# # tail(co)
# # length(AA)
# # head(AA)
# # tail(AA)
# # length(UNITY)
# # head(UNITY)
# # tail(UNITY)

# #データの結合
# pt1 <- merge(co, AA)
# pt <- merge(pt1, UNITY)
# #UNITYデータの欠損値の置換
# pt$UNITY[is.na(pt$UNITY)]<-0

# head(pt)

# print("Preprocessing is finished")

# #バンド幅
# # bw <- gwr.sel(AA ~ UNITY , data=pt, coords=cbind(pt$LON, pt$LAT))
# # bw

# gauss <- gwr(AA ~ UNITY, data=pt,coords=cbind(pt$LON, pt$LAT), bandwidth=5)
# gauss
# print("fin")