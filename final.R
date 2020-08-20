 # This code assumes edx and validation sets are already gnerated
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(dplyr)

#function to calc the rmse for ratings

RMSE<-function(actual,predicted){
  sqrt(mean((actual-predicted)^2))
}
# Check to see if there are any NA in our data that me might need to fill in 

sapply(edx,anyNA)



#use a range of lambdas for crosstraining

lamb<-seq(0,1,.1)

results<-sapply(lamb,function(lmb){
  
  #take the training set average movie rating

  mu<-mean(edx$rating)
  
  #now take the average of differences and divide by lambda
  
  #reg_movie_bias is the average difference of the movie rating minus the calculated mean with the additon of a 
  #small penalty factor for movies with lower reviews.
  reg_movie_bias<-edx%>%group_by(movieId)%>%summarize(movie_bias=sum(rating-mu)/(n()+lmb))
  
  ##reg_user_bias is simple the averaged residual left when the average bias per movie is subtracted from the average of all movies and the average
  ##rating per movie, associated by user
  
  reg_user_bias<-edx%>%left_join(reg_movie_bias,by="movieId")%>%group_by(userId)%>%summarize(user_bias=sum(rating-mu-movie_bias)/(n()+lmb))
  
  ##reg_genre_bias is the regularized residual left when we have subtracted the effects of a given movie reviewed by a given user
  reg_genre_bias<-edx%>%left_join(reg_movie_bias,by="movieId")%>%left_join(reg_user_bias,by="userId")%>%group_by(genres)%>%summarize(gbias=sum(rating-mu-movie_bias-user_bias)/(n()+lmb))
  
  ##now we construct a table of all the average residuals of a given movie reviewed by given user in a specific genre
  ##and use this table to make predictions on our training data.
  predicted_ratings<-edx%>%left_join(reg_movie_bias,by="movieId")%>%left_join(reg_user_bias,by="userId")%>%left_join(reg_genre_bias,by="genres")%>%mutate(pred=mu+movie_bias+user_bias+gbias)%>%pull(pred)
 
   #derive an RMSE
  RMSE(edx$rating,predicted_ratings)

})

#now that we have chosen our lambda parameter (lowest RMSE) we run the process again

lambda<-lamb[which.min(results)]

mu<-mean(edx$rating)
reg_movie_bias<-edx%>%group_by(movieId)%>%summarize(movie_bias=sum(rating-mu)/(n()+lambda))
reg_user_bias<-edx%>%left_join(reg_movie_bias,by="movieId")%>%group_by(userId)%>%summarize(user_bias=sum(rating-mu-movie_bias)/(n()+lambda))
reg_genre_bias<-edx%>%left_join(reg_movie_bias,by="movieId")%>%left_join(reg_user_bias,by="userId")%>%group_by(genres)%>%summarize(gbias=sum(rating-mu-movie_bias-user_bias)/(n()+lambda))

#we constuct a table containing all the effects of varius movies reviewed by a given user in a given genre
predicted_ratings_training<-edx%>%left_join(reg_movie_bias,by="movieId")%>%left_join(reg_user_bias,by="userId")%>%left_join(reg_genre_bias,by="genres")%>%mutate(pred=mu+movie_bias+user_bias+gbias)%>%pull(pred)
edx<-edx%>%mutate(predicted=predicted_ratings_training,error=rating-predicted_ratings_training)

##now we use this table to make predictions on our test data
predicted_ratings<-validation%>%left_join(reg_movie_bias,by="movieId")%>%left_join(reg_user_bias,by="userId")%>%left_join(reg_genre_bias,by="genres")%>%mutate(pred=mu+movie_bias+user_bias+gbias)%>%pull(pred)

##now we will see how much difference we have
##RMSE for final result on our validation set
RMSE(validation$rating,predicted_ratings)
#   
