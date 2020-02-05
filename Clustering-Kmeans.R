#### Table 15.2

utilities.df <- read.csv("Utilities.csv")

# set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]

# remove the utility column
utilities.df <- utilities.df[,-1]

# compute Euclidean distance
# (to compute other distance measures, change the value in method = )
d <- dist(utilities.df, method = "euclidean")

#### Table 15.4

# normalize input variables
utilities.df.norm <- sapply(utilities.df, scale)

# add row names: utilities
row.names(utilities.df.norm) <- row.names(utilities.df) 

# compute normalized distance based on variables Sales and FuelCost
d.norm <- dist(utilities.df.norm[,c(6,8)], method = "euclidean")



#### Figure 15.3
# compute normalized distance based on all 8 variables
d.norm <- dist(utilities.df.norm, method = "euclidean")

# in hclust() set argument method =  
# to "ward.D", "single", "complete", "average", "median", or "centroid"
#"ward.D" is distsance measure based on "loss of information" when records/cluster join together.
#"hc" is hendorgram
hc1 <- hclust(d.norm, method = "single")
plot(hc1,hang = -1,  ann = FALSE)
#"-1"The fraction of the plot height by which label should hang below the rest of the plot.
#A negative # will cause the label to hang down from 0
#ann = FALSE) --> do you want dedault automation like title y-axix/x-axis labels.
hc2 <- hclust(d.norm, method = "average")
plot(hc2, hang = -1, ann = FALSE)




#### Table 15.6

memb <- cutree(hc1, k = 3)
memb
memb <- cutree(hc2, k = 6)
memb




#### Figure 15.4

# set labels as cluster membership and utility name
row.names(utilities.df.norm) <- paste(memb, ": ", row.names(utilities.df), sep = "")
#": " ----> cluster #
#sep = ""------>no extra space between memb and ": " needed
#row.names(utilities.df)----> utility company name 


#### Table 15.9

# load and preprocess data 
utilities.df <- read.csv("Utilities.csv")
row.names(utilities.df) <- utilities.df[,1]
utilities.df <- utilities.df[,-1]

# normalized distance:
utilities.df.norm <- sapply(utilities.df, scale)
row.names(utilities.df.norm) <- row.names(utilities.df) 

# run kmeans algorithm 
set.seed(2)
km <- kmeans(utilities.df.norm, 6)

# show cluster membership
km$cluster




#### Table 15.10
# centroids
km$centers



#### Figure 15.5

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))

#c(0)--> need to plot something even though this will show nothing
#xaxt---> a character which specifies the x-axis
#type: a vale of "n" surpresses plotting of axis

# label x-axes
axis(1, at = c(1:8), labels = names(utilities.df))
##1 -------> 1.below, 2.left 3. above  4. right



# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                       "black", "dark grey"))
#lty: 0: blank, 1:solid 2: dash 3: dotted 4:dot dash 5 long dash 6 two dash
#lwd: line width
#ifelse(i %in% c(1, 3, 5),"black", "dark grey") ----->
###if i is in c(1,3,5) , then black, else dar grey

# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))
#add text to graph 
#y = km$centers[, 1] ----> at this height
# paste("Cluste ----> what text

#### Table 15.11
test<-as.data.frame(km$centers)
dist(km$centers, method = "manhattan")
