#Kmeans clustering to make an educated decision 
#about what cities to customize new marketing campaigns in
names(df) # variables 
indvard = 'user_location_city' 
#select 8 features that distinguish one city from another
depvars = c('duration', 
            'notice',
            'orig_destination_distance',
            'is_mobile',
            'is_package',
            'srch_adults_cnt',
            'srch_children_cnt',
            'srch_rm_cnt')
allvars=c(indvard,depvars)
k_data = df[,allvars]
k_data = aggregate(.~user_location_city, data=k_data, mean, na.action = na.omit)
k_data[, depvars] = apply(k_data[,depvars], 2, scale)
row.names(k_data) = k_data[,indvard]
k_data = k_data[,depvars]
head(k_data)

#now lets look at some clusters
k2 <- kmeans(k_data, centers = 2, nstart = 25)
k3 <- kmeans(k_data, centers = 3, nstart = 25)
k4 <- kmeans(k_data, centers = 4, nstart = 25)
k5 <- kmeans(k_data, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = k_data) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = k_data) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = k_data) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = k_data) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)

print(fviz_nbclust(k_data, kmeans, method = "wss",k.max = 12)) #Elbow method - find optimal K
print(fviz_nbclust(k_data, kmeans, method = "silhouette")) #Elbow method - find optimal K

final <- kmeans(k_data, 4)
print(final)
#lets start look at the final
print(fviz_cluster(final, data = k_data))#this performs pca and plots clusters

map = as.data.frame(cbind(names(final$cluster), final$cluster))
names(map) = c('user_location_city', 'cluster')
  
total <- merge(df,map,by="user_location_city")
  
final_c = as.data.frame(na.omit(total) %>%
  group_by(cluster) %>%
  summarise_all("mean"))

final_c = as.data.frame(cbind(final_c[,depvars], cluster = final_c$cluster))
head(final_c)
 
table(total$cluster)