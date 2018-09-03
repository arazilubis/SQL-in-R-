#Arazi Lubis 
#hw 9 
library(RSQLite)
library(DBI)

#setting directory 
#setwd("/Users/arazilubis/Downloads")
#getwd()

#testing if things work 
drv <- dbDriver('SQLite')
con <- dbConnect(drv, dbname = 'lean_imdbpy_2010.db')
dbListTables(con)
dbListTables(con, "cast_info2")


dbGetQuery(con, "select * from kind_type limit 100")
dbListTables(con)
dbGetQuery(con, "select * from aka_name2 limit 100")

#1a tom cruise id 
nameInfo = dbGetQuery(con, "SELECT id FROM name2 WHERE name = 'Cruise, Tom';")
nameInfo #Tom Cruise ID 432197 -- he may also have a japanese name 

#1b
dbGetQuery(con, "select * from role_type")
actorid = dbGetQuery(con, "select * from role_type where id = 1")
actorid 
#actors have a role id 1

#1c 

numactors = dbGetQuery(con, "select count(distinct person_id) from cast_info2 where role_id = 1 OR role_id = 2")
numactors 
#there are 1251907 actors in this database 

movies = dbGetQuery(con, "select count(*) from movie_info2;")
movies 
#are there 3528314 movies 

#1d 

typework = dbGetQuery(con, "select * from kind_type where kind = 'movie';")
typework


#1e final answer is called cruisemovies
dbGetQuery(con, "select * from cast_info2 where person_id = 432197 and person_role_id = 1")
dbGetQuery(con, "select * from title2 limit 5")
dbGetQuery(con, "select * from aka_title2 limit 5")
#join tbles movie id, under cast_info2 where actor id is equal to t cruise  

#want the movies that have tom cruise as an actor, return a database with movie name and release 
# FINAL ANSWER # 


cruisemovies = dbGetQuery(con, "SELECT title, production_year FROM title2 JOIN cast_info2 
                          ON title2.id = cast_info2.movie_id 
                          WHERE cast_info2.person_id = 432197 
                          AND cast_info2.role_id = 1 
                          AND title2.kind_id = 1")

cruisemovies

#2a like before I split each part into 2 parts each: 1 for easy ham and one for spam. i realize that maybe 
#doing if statements could save some lines but I think that would require extra work saving the respective 
#files into other lists. 

#easyham -- 

deleteHTML = function(email = "00001.7c53336b37003a9286aba55d2945844c"){
  
    bigfile = "http://personal.psu.edu/muh10/380/data/spam/easy_ham/"
    filemail = paste(bigfile, email, sep = "")
    message = readLines(filemail)
    for(i in 1:length(message)) {
      if(message[i] == "") {
        cleanmessage = message[i:length(message)]
        break
      }
    }
    
    veryclean = gsub("<.*?>", "", cleanmessage)
    return(veryclean)
  }

#example 
deleteHTML("00001.7c53336b37003a9286aba55d2945844c")  
deleteHTML("00003.860e3c3cee1b42ead714c5c874fe25f7")
deleteHTML("00002.9c4069e25e1ef370c078db7ee85ff9ac") 


#spam---

deleteHTML2 = function(email = "00001.7c53336b37003a9286aba55d2945844c") {
  bigfile = "http://personal.psu.edu/muh10/380/data/spam/spam/"
  filemail = paste(bigfile, email, sep = "")
  message = readLines(filemail)
  for(i in 1:length(message)) {
    if(message[i] == "") {
      cleanmessage = message[i:length(message)]
      break
    }
  }
  together = paste(cleanmessage, collapse = " ")
  return(together = gsub("<.*?>", "", together))
}

deleteHTML2('00001.7848dde101aa985090474a91ec93fcf0')
deleteHTML2('00002.d94f1b97e48ed3b553b3508d116e6a09')
deleteHTML2('00003.2ee33bc6eacdb11f38d052c44819ba6c')
deleteHTML2('00004.eac8de8d759b7e74154f142194282724')

#2b counting words #stringsplit, unlist, remove "" try the if 

countwords = function(email = "00001.7c53336b37003a9286aba55d2945844c"){
  
  bigfile = "http://personal.psu.edu/muh10/380/data/spam/easy_ham/"
  filemail = paste(bigfile, email, sep = "")
  message = readLines(filemail)
  for(i in 1:length(message)) {
    if(message[i] == "") {
      cleanmessage = message[i:length(message)]
      break
    }
  }
  
  veryclean = gsub("<.*?>", "", cleanmessage)
  together = strsplit(veryclean, split = " ")
  together1 = unlist(together)
  return(length(together1[together1 != " "]))
}

#easy ham examples 
countwords("00001.7c53336b37003a9286aba55d2945844c")
countwords("00003.860e3c3cee1b42ead714c5c874fe25f7")
countwords("00002.9c4069e25e1ef370c078db7ee85ff9ac")



countwords2 = function(email = "00001.7c53336b37003a9286aba55d2945844c") {
  bigfile = "http://personal.psu.edu/muh10/380/data/spam/spam/"
  filemail = paste(bigfile, email, sep = "")
  message = readLines(filemail)
  for(i in 1:length(message)) {
    if(message[i] == "") {
      cleanmessage = message[i:length(message)]
      break
    }
  }
  together = paste(cleanmessage, collapse = " ")
  together1 = gsub("<.*?>", "", together)
  together2 = strsplit(together1, split = " ")
  together3 = unlist(together2)
  together4 = together3[together3 != ""]
  return(length(together4))
}

#spam examples 
countwords2('00001.7848dde101aa985090474a91ec93fcf0')
countwords2('00002.d94f1b97e48ed3b553b3508d116e6a09')
countwords2('00003.2ee33bc6eacdb11f38d052c44819ba6c')
countwords2('00004.eac8de8d759b7e74154f142194282724')

#3 bootstrap 

bulbs=scan('http://personal.psu.edu/muh10/540/data/bulbs.dat')
bulbs
length(bulbs)
hist(bulbs)
B = 100 #number of bootstrap samples how many times you want to repeat 
bulbBS = function()
{
  bulbsamp = sample(bulbs, 75, replace = TRUE)
  return(median(bulbsamp))
}
bulbBS
bulbBS()
bulbMedians = replicate(B, bulbBS())
bulbMedians
#histrogram of the sample median of lightbulbs 
hist(bulbMedians)
var(bulbMedians)
#95% confidence interval 
mean(bulbMedians) + 1.96*sd(bulbMedians)
mean(bulbMedians) - 1.96*sd(bulbMedians)
bulbs
#error
sd(bulbMedians)/sqrt(75)

