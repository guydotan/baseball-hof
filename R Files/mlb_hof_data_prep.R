## Exploratory Data Analysis
## MLB Dataset - HOF
## 12/03/2019

setwd("~/Documents/UCLA MAS/2019 Fall/Final")

# libraries
library("RSQLite")
library(dplyr)
library(reshape)
library(sqldf)

# connect to the sqlite file
con = dbConnect(drv=RSQLite::SQLite(), dbname="the-history-of-baseball/database.sqlite")

# get a list of all tables
alltables = dbListTables(con)

# get the hall of fame data as a data.frame
hof <- dbGetQuery( con,' select player_id,
                                count(1) years,
                                max(inducted) as inducted
                        from hall_of_fame hof 
                        where category = "Player"
                        group by player_id')
                        #hof.inducted = "Y" ' )

# aggregate player career regular season stats
career_pit <- dbGetQuery( con,' select
                          player_id,
                          min(year) as min_year,
                          max(year) as max_year,
                          sum(w) as w,
                          sum(l) as l,
                          sum(g) as g,
                          sum(gs) as gs,
                          sum(cg) as cg,
                          sum(sho) as sho,
                          sum(sv) as sv,
                          sum(ipouts) as ipouts,
                          sum(h) as h,
                          sum(er) as er,
                          sum(hr) as hr,
                          sum(bb) as bb,
                          sum(so) as so,
                          NULL as baopp,
                          NULL as era,
                          sum(ibb) as ibb,
                          sum(wp) as wp,
                          sum(hbp) as hbp,
                          sum(bk) as bk,
                          sum(bfp) as bfp,
                          sum(gf) as gf,
                          sum(r) as r,
                          sum(sh) as sh,
                          sum(sf) as sf,
                          sum(g_idp) as g_idp
                        from pitching 
                        where 1=1
                        group by player_id ')

# aggregate player postseason stats
post_pit <- dbGetQuery( con,' select
                          player_id,
                          min(year) as min_year,
                          max(year) as max_year,
                          sum(w) as w,
                          sum(l) as l,
                          sum(g) as g,
                          sum(gs) as gs,
                          sum(cg) as cg,
                          sum(sho) as sho,
                          sum(sv) as sv,
                          sum(ipouts) as ipouts,
                          sum(h) as h,
                          sum(er) as er,
                          sum(hr) as hr,
                          sum(bb) as bb,
                          sum(so) as so,
                          NULL as baopp,
                          NULL as era,
                          sum(ibb) as ibb,
                          sum(wp) as wp,
                          sum(hbp) as hbp,
                          sum(bk) as bk,
                          sum(bfp) as bfp,
                          sum(gf) as gf,
                          sum(r) as r,
                          sum(sh) as sh,
                          sum(sf) as sf,
                          sum(g_idp) as g_idp
                        from pitching_postseason 
                        where 1=1
                        group by player_id ')

# aggregate player seasons ranking in top of wins and saves
seasons_w_sv_so <- dbGetQuery( con,' select player_id,
                                       sum(case when w_rk <= 10 then 1 else 0 end) w_top10,
                                       sum(case when w_rk <= 5 then 1 else 0 end) w_top5,
                                       sum(case when sv_rk <= 10 then 1 else 0 end) sv_top10,
                                       sum(case when sv_rk <= 5 then 1 else 0 end) sv_top5,
                                       sum(case when so_rk <= 10 then 1 else 0 end) so_top10,
                                       sum(case when so_rk <= 5 then 1 else 0 end) so_top5
                                     from (select
                                      player_id,    
                                      league_id,
                                      year,
                                      rank() over(partition by year, league_id order by w desc) as w_rk,
                                      rank() over(partition by year, league_id order by sv desc) as sv_rk,
                                      rank() over(partition by year, league_id order by so desc) as so_rk
                                   from pitching)
                                      where 1=1
                                      group by player_id ')

# aggregate player seasons ranking in top of era
seasons_era <- dbGetQuery( con,' select player_id,
                                       sum(case when era_rk <= 10 then 1 else 0 end) era_top10,
                                       sum(case when era_rk <= 5 then 1 else 0 end) era_top5
                                     from (select
                                       player_id,    
                                       league_id,
                                       year,
                                       rank() over(partition by year, league_id order by er*1.0/ipouts asc) as era_rk
                                from pitching
                                     where ipouts >= 150*3.0)
                                     group by player_id ' )

# aggregate player all-star game appearances
allstar <- dbGetQuery( con,' select player_id,
                                    count(1) as asg
                             from all_star        
                             group by player_id' )

# aggregate player award wins
award <- dbGetQuery( con,' select player_id,
                                    award_id,
                                    count(1) as wins
                             from player_award
                             where award_id in ("Gold Glove","Rookie of the Year",
                                          "Most Valuable Player","Cy Young Award",
                                          "ALCS MVP", "NLCS MVP", "World Series MVP")
                             group by award_id, player_id' )

# all player names
plyr <- dbGetQuery( con,' select player_id, name_first, name_last
                          from player ' )

# pitching war dataset
war <- read.csv("the-history-of-baseball/war_daily_pitch.csv", stringsAsFactors = F, na.strings = "NULL")

# aggregate player career war
career_war <- war %>% group_by(player_id = player_ID) %>% 
                  summarise(careerWAR = sum(WAR))

career_war[career_war$player_id == 'sabatc.01',]$player_id <- "sabatcc01"
career_war[career_war$player_id == 'burnea.01',]$player_id <- "burneaj01"

# aggregate player seasons ranking in top of war
seasons_war <- sqldf( ' select player_id,
                                sum(case when war_rk <= 10 then 1 else 0 end) war_top10,
                                sum(case when war_rk <= 5 then 1 else 0 end) war_top5
                             from (select
                                   player_id,    
                                   lg_ID,
                                   year_ID,
                                   rank() over(partition by year_ID, lg_ID order by WAR desc) as war_rk
                                   from "war")
                             group by player_id ')

# define era
career_pit$era <- career_pit$er / career_pit$ipouts * 27
post_pit$era <- post_pit$er / post_pit$ipouts * 27

# minimum 1000 innings pitched
career_pit_min1k <-  career_pit[career_pit$ipouts >= 1000*3.0,]

# add in career regular season stats
full_plyr_list <- inner_join(plyr, career_pit_min1k, by = "player_id")

# add in allstar apps
full_plyr_list <- left_join(full_plyr_list, allstar, by = "player_id")

# add in awards
award_piv <- cast(award, player_id ~ award_id, value = "wins", fill = 0)
award_piv$Post_MVPs <- award_piv$`ALCS MVP` + award_piv$`NLCS MVP` + award_piv$`World Series MVP`
names(award_piv) <- c("player_id", "alcs-mvp", "cy-young", "gold-glove", "mvp", "nlcs-mvp", "roy", "ws-mvp", "post-mvp")
full_plyr_list <- left_join(full_plyr_list, award_piv[,c("player_id","cy-young", "mvp",
                                                         "gold-glove", "post-mvp")], by = "player_id")

full_plyr_list[is.na(full_plyr_list$`cy-young`),]$`cy-young` <- 0
full_plyr_list[is.na(full_plyr_list$`gold-glove`),]$`gold-glove` <- 0
full_plyr_list[is.na(full_plyr_list$`mvp`),]$`mvp` <- 0
full_plyr_list[is.na(full_plyr_list$`post-mvp`),]$`post-mvp` <- 0

# add in postseason stats
full_plyr_list <- left_join(full_plyr_list , post_pit[,-c(2,3,17)], by = "player_id", suffix = c(".reg", ".post"))

# add in career war
full_plyr_list <- left_join(full_plyr_list , career_war, by = "player_id")

# add in season ranks
full_plyr_list <- left_join(full_plyr_list, seasons_era, by = "player_id")
full_plyr_list <- left_join(full_plyr_list, seasons_w_sv_so, by = "player_id")
full_plyr_list <- left_join(full_plyr_list, seasons_war, by = "player_id")

# add in HOF
full_plyr_list <- left_join(full_plyr_list , hof[,-2], by = "player_id")
full_plyr_list[is.na(full_plyr_list$inducted),]$inducted <- "N"

# only players who's career was after 1900
modern_list <- full_plyr_list[full_plyr_list$max_year >= 1900,]

# induct all pitchers who got in after 2015
modern_list[modern_list$player_id == 'hallaro01',]$inducted <- "Y"
modern_list[modern_list$player_id == 'mussimi01',]$inducted <- "Y"
modern_list[modern_list$player_id == 'riverma01',]$inducted <- "Y"
modern_list[modern_list$player_id == 'hoffmtr01',]$inducted <- "Y"
modern_list[modern_list$player_id == 'morrija02',]$inducted <- "Y"
modern_list[modern_list$player_id == 'smithle02',]$inducted <- "Y"

# export to CSV
write.csv(modern_list , "pitcher_list_v3.csv", row.names = F, na = "")

