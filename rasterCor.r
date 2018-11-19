library(spatialEco)
library(raster)

x <- raster(file.path("Unity_Foot.tif"))
y <- raster(file.path("Depthmap_Foot.tif"))
dim(x)
dim(y)

r.cor <- rasterCorrelation(x, y, s = 5, type = "spearman")
s <- raster(nrow=1027, ncol=646)
s <- resample(r.cor, s, method='bilinear')

corValue <- values(s)
length(corValue)

write.csv(corValue, file = "corValue.csv")

# cor(values(x),
#     values(y),
#     use = "na.or.complete")