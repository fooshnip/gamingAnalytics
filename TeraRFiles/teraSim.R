library(RPostgreSQL)
db <- dbConnect(PostgreSQL(), 
                user = "postgres", #This needs to be adjusted for your login details
                host = "localhost",
                dbname = "gaming", 
                password = "postgres") #This needs to be adjusted for your login details
#Some example pulls
dbListTables(db)

char <- dbGetQuery(db,"select * from characters")
#JUST ADDING INT CODE-USED FOR VISUAL
char$code <- ifelse(char$class=="Archer",1,
              ifelse(char$class=="Berserker",2,
                ifelse(char$class=="Lancer",3,
                  ifelse(char$class=="Mystic",4,
                    ifelse(char$class=="Priest",5,
                      ifelse(char$class=="Slayer",6,
                        ifelse(char$class=="Sorcerer",7,
                          ifelse(char$class=="Warrior",8,
                                 0))))))))

## Simulation Engine
## takes in a class type and number of simulations
terasim <- function(classsim,num,...){
  require(plyr)
  require(triangle)
  pb<- txtProgressBar(min=1,max=num,style=3) #can mess with style of this
  l=1
  i=1
  samp = 0
  cube1 <- array(1:(32*41*num),dim=c(32,41,num))
  for (i in 1:num){
    setTxtProgressBar(pb,i)
    for (l in 1:max(char$level)){
      if (l==1){
        char2 <- char[char$level==l & char$class==classsim,]
        samp <-  char2[sample(1:dim(char2)[1], size=1, replace=TRUE),];samp$prob=0;samp$probdif=0;samp$qtile=0
      } else {
        char2 <- char[char$level==l & 
                      char$class==classsim & 
                      char$kills_monsters >= samp[l-1,"kills_monsters"],] #for now, just constraint on monsters killed
       if (nrow(char2)==0 | samp$level[l-1]==0){
          samp <- rbind(samp,0)
        }
        else {
          ##TRIANGLE
          char2$prob <- ifelse(quantile(char2$kills_monsters)[[2]]<char2$kills_monsters,
                               2*((max(char2$kills_monsters)+1)-char2$kills_monsters)/((max(char2$kills_monsters)+1)-(min(char2$kills_monsters)-1))*((max(char2$kills_monsters)+1)-quantile(char2$kills_monsters)[[2]]),
                               2*(char2$kills_monsters-(min(char2$kills_monsters)-1))/((max(char2$kills_monsters)+1)-(min(char2$kills_monsters)-1))*(quantile(char2$kills_monsters)[[2]]-(min(char2$kills_monsters)-1))
          )
          char2$probdif <- char2$prob/max(char2$prob)
          char2$qtile <- (char2$kills_monsters-min(char2$kills_monsters))/(max(char2$kills_monsters)-min(char2$kills_monsters))
          samp <- rbind(samp,char2[sample(1:dim(char2)[1], size=1, replace=TRUE, prob=char2$prob),])
        }
      }
      l<-l+1
    } #end the level looping mechanic
    cube1[,,i] <- as.matrix(samp)
    samp = 0
  } #end the player looping mechanic
  return(cube1)
  close(pb)#end progress bar
}

## Simulation loop
## Will run through all class types given a number of simulations per class to run
numsim <- 1000
archer100 <- terasim("Archer",numsim)
berserker100 <- terasim("Berserker",numsim)
lancer100 <- terasim("Lancer",numsim)
mystic100 <- terasim("Mystic",numsim)
priest100 <- terasim("Priest",numsim)
slayer100 <- terasim("Slayer",numsim)
sorcerer100 <- terasim("Sorcerer",numsim)
warrior100 <- terasim("Warrior",numsim)

simData <- function(numsim, metricCode){
  warri=mat.or.vec(numsim,35)
  for(i in 1:numsim){
    warri[i,] <- c(warrior100[,,i][1,3],warrior100[,,i][1,38],metricCode,as.numeric(t(warrior100[,,i][,metricCode])))
  }
  mysti=mat.or.vec(numsim,35)
  for(i in 1:numsim){
    mysti[i,] <- c(mystic100[,,i][1,3],mystic100[,,i][1,38],metricCode,as.numeric(t(mystic100[,,i][,metricCode])))
  }
  pries=mat.or.vec(numsim,35)
  for(i in 1:numsim){
    pries[i,] <- c(priest100[,,i][1,3],priest100[,,i][1,38],metricCode,as.numeric(t(priest100[,,i][,metricCode])))
  }
  arche=mat.or.vec(numsim,35)
  for(i in 1:numsim){
    arche[i,] <- c(archer100[,,i][1,3],archer100[,,i][1,38],metricCode,as.numeric(t(archer100[,,i][,metricCode])))
  }
  lance=mat.or.vec(numsim,35)
  for(i in 1:numsim){
    lance[i,] <- c(lancer100[,,i][1,3],lancer100[,,i][1,38],metricCode,as.numeric(t(lancer100[,,i][,metricCode])))
  }
  slaye=mat.or.vec(numsim,35)
  for(i in 1:numsim){
    slaye[i,] <- c(slayer100[,,i][1,3],slayer100[,,i][1,38],metricCode,as.numeric(t(slayer100[,,i][,metricCode])))
  }
  sorce=mat.or.vec(numsim,35)
  for(i in 1:numsim){
    sorce[i,] <- c(sorcerer100[,,i][1,3],sorcerer100[,,i][1,38],metricCode,as.numeric(t(sorcerer100[,,i][,metricCode])))
  }
  berse=mat.or.vec(numsim,35)
  for(i in 1:numsim){
    berse[i,] <- c(berserker100[,,i][1,3],berserker100[,,i][1,38],metricCode,as.numeric(t(berserker100[,,i][,metricCode])))
  }
  data = rbind(warri,mysti,pries,arche,lance,slaye,sorce,berse)
  fixins <- c('Class','TagCode','metricCode','1','2','3','4','5','6','7','8','9','10','11','12','13',
                      '14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29',
                      '30','31','32')
  colnames(data) <- fixins
  return(data)
}

## Metric code goes from 7 to 37
##[7]  "friends"               "quests_completed"      "achievements"         
##[10] "attack"                "defense"               "knockdown"            
##[13] "knockdown_resist"      "crit_ratio"            "crit_resist"          
##[16] "strength"              "endurance"             "damage"               
##[19] "balance"               "speed_attack"          "speed_movement"       
##[22] "mining"                "plants"                "energy"               
##[25] "kills_monsters"        "loot_total_items"      "kills_monsters_small" 
##[28] "kills_monsters_medium" "kills_monsters_large"  "auctions_created"     
##[31] "auctions_expired"      "auctions_sold"         "auctions_purchased"   
##[34] "deaths_monsters"       "enchant_success"       "enchant_attempt"      
##[37] "craft_success"   
pb<- txtProgressBar(min=7,max=37,style=3) #can mess with style of this
for (k in 7:37){
  setTxtProgressBar(pb,k)
  testing <- simData(1000,k)
  write.csv(testing,paste('C:/Users/Riles/Documents/Northwestern/Projects/PLAIT Lab/TeraData/simData/sim',numsim,'metricCode',k,'.csv',sep=""),
          quote=FALSE,
          row.names=FALSE)
  testing <- 0
  close(pb)#end progress bar
}

##warrior100[,,1][,37]