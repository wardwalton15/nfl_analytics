# Rscript rushing_vs_passing.r [cached nflverse PBP .rds]
library(dplyr)
library(grid)
args <- commandArgs(trailingOnly=TRUE)
p <- if(length(args)) readRDS(args[1]) else nflreadr::load_pbp(2025)
p <- p |> filter(season==2025, season_type=='REG', !coalesce(play_deleted==1,FALSE))
out <- 'outputs/rushing_vs_passing_2025'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
games <- p |> distinct(game_id,home_team,away_team,home_score,away_score)
stopifnot(nrow(games)==272, !anyNA(games))
yards <- p |> filter(!is.na(posteam)) |> group_by(game_id,posteam) |>
 summarise(rush_yards=sum(rushing_yards,na.rm=TRUE),net_pass_yards=sum(if_else(play_type=='pass',yards_gained,0),na.rm=TRUE),.groups='drop')
games <- games |> left_join(yards,by=c('game_id','home_team'='posteam')) |> rename(home_rush=rush_yards,home_pass=net_pass_yards) |>
 left_join(yards,by=c('game_id','away_team'='posteam')) |> rename(away_rush=rush_yards,away_pass=net_pass_yards) |>
 mutate(result=sign(home_score-away_score),rush_edge=sign(home_rush-away_rush),pass_edge=sign(home_pass-away_pass))
stopifnot(!anyNA(games))
write.csv(games,file.path(out,'game_audit.csv'),row.names=FALSE)
plays <- p |> filter(play_type %in% c('run','pass'), !is.na(epa),coalesce(qb_kneel,0)==0,coalesce(qb_spike,0)==0,coalesce(two_point_attempt,0)==0) |>
 mutate(type=if_else(qb_dropback==1,'Dropback','Designed run'),state=case_when(score_differential>0~'Leading',score_differential<0~'Trailing',TRUE~'Tied'))
epa <- plays |> group_by(type) |> summarise(plays=n(),epa=mean(epa),.groups='drop')
rates <- plays |> group_by(state) |> summarise(plays=n(),runs=sum(type=='Designed run'),run_rate=runs/plays,.groups='drop')
quarters <- plays |> group_by(qtr,state) |> summarise(plays=n(),run_rate=mean(type=='Designed run'),.groups='drop')
win <- data.frame(metric=c('Rushing yards','Net passing yards'))
win$wins <- c(sum(games$rush_edge==games$result & games$result!=0 & games$rush_edge!=0),sum(games$pass_edge==games$result & games$result!=0 & games$pass_edge!=0))
win$games_with_yardage_edge <- c(sum(games$rush_edge!=0),sum(games$pass_edge!=0))
win$game_ties_with_edge <- c(sum(games$result==0 & games$rush_edge!=0),sum(games$result==0 & games$pass_edge!=0))
win$win_pct <- 100*win$wins/win$games_with_yardage_edge
for(n in c('epa','rates','quarters','win')) write.csv(get(n),file.path(out,paste0(n,'.csv')),row.names=FALSE)
print(win); print(epa); print(rates); print(quarters)
bg <- '#F5F0E6'; ink <- '#242320'; muted <- '#686158'; orange <- '#B44A16'; blue <- '#24596A'
txt <- function(label,x,y,size=24,col=ink,font=1,just='left') grid.text(label,x,y,just=just,gp=gpar(fontfamily='Arial',fontsize=size,col=col,fontface=font))
rect <- function(x,y,w,h,fill) grid.rect(x,y,w,h,just=c('left','bottom'),gp=gpar(fill=fill,col=NA))
pass <- epa$epa[epa$type=='Dropback']; run <- epa$epa[epa$type=='Designed run']
lead <- rates$run_rate[rates$state=='Leading']; trail <- rates$run_rate[rates$state=='Trailing']
draw <- function(){
 grid.newpage(); rect(0,0,1,1,bg)
 txt('2025 NFL  /  REGULAR SEASON',.075,.935,17,orange,2)
 txt('DOES RUNNING\nCAUSE WINNING?',.075,.865,36,font=2)
 txt(sprintf('%.1f%%',win$win_pct[1]),.075,.748,65,orange,2)
 txt('of teams with more rushing yards won.',.075,.691,20,font=2)
 txt(sprintf('%d wins in %d games with a rushing-yard edge',win$wins[1],win$games_with_yardage_edge[1]),.075,.663,15,muted)
 rect(.075,.624,.85,.0015,'#D8CEBE')
 txt('THE SCORE CHANGES THE PLAY CALL',.075,.592,18,font=2)
 txt('Designed runs as a share of offensive plays',.075,.565,15,muted)
 for(i in 1:2){ y<-c(.501,.448)[i]; v<-c(trail,lead)[i]; txt(c('Trailing','Leading')[i],.075,y+.017,18); rect(.27,y,.48*v/.6,.032,c(blue,orange)[i]); txt(sprintf('%.1f%%',v*100),.27+.48*v/.6+.02,y+.016,19,font=2) }
 txt('Teams with the lead run more often.',.075,.41,17,muted)
 rect(.075,.376,.85,.0015,'#D8CEBE')
 txt('DROPBACKS ADDED MORE VALUE',.075,.344,18,font=2)
 txt('Average expected points added (EPA) per play',.075,.317,15,muted)
 grid.lines(c(.52,.52),c(.20,.285),gp=gpar(col=muted,lwd=1))
 for(i in 1:2){ y<-c(.253,.202)[i]; v<-c(pass,run)[i]; txt(c('Dropbacks','Designed runs')[i],.075,y+.016,17); rect(.52+min(v,0)*3,y,abs(v)*3,.03,c(blue,orange)[i]); txt(sprintf('%+.3f',v),max(.52,.52+v*3)+.02,y+.015,19,font=2) }
 txt('Rushing totals reflect game script, too.',.075,.159,21,font=2)
 txt('Correlation alone cannot tell us what caused the win.',.075,.131,15,muted)
 txt('Dropbacks include sacks + scrambles. EPA / run rates exclude\nkneels, spikes, no-play penalties and two-point attempts.\nRushing totals include scrambles + kneels. Source: nflverse.\nYardage ties excluded; a tied game is not counted as a win.',.075,.069,11,muted)
}
png(file.path(out,'rushing_vs_passing_2025.png'),width=1080,height=1920,res=144); draw(); dev.off()
