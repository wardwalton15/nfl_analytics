# Run after generating outputs/starting_field_position_2025/drive_audit.csv
.libPaths(c('/private/tmp/nfl-gt-library', .libPaths()))
library(dplyr)
library(ggplot2)
output_dir <- 'outputs/starting_field_position_2025'
drives <- read.csv(file.path(output_dir, 'drive_audit.csv'))
teams <- drives |> group_by(posteam) |> summarise(drives=n(), scores=sum(scored),
  avg_start=mean(100-start), scoring_rate=mean(scored), .groups='drop')
stopifnot(nrow(teams)==32, sum(teams$drives)==5700, sum(teams$scores)==2254)
write.csv(teams,file.path(output_dir,'team_summary.csv'),row.names=FALSE)
league_x <- mean(100-drives$start)
league_y <- mean(drives$scored)
bg <- '#F5F0E6'
ink <- '#242320'
accent <- '#B44A16'
# Small visual offsets separate nearby logos; leader lines preserve actual coordinates.
teams <- teams |> mutate(lx=avg_start, ly=scoring_rate)
offsets <- data.frame(posteam=c('CHI','DET','KC','LAC','ARI','ATL','MIA','WAS','NE','SEA','NYG','PIT'),
 dx=c(-.03,.03,-.13,.13,-.12,.00,-.02,.12,-.06,.06,-.06,.06),
 dy=c(-.005,.005,-.003,.003,-.004,.009,-.007,.008,.004,-.004,-.005,.005))
teams <- teams |> left_join(offsets,by='posteam') |> mutate(lx=lx+coalesce(dx,0),ly=ly+coalesce(dy,0))
p <- ggplot(teams,aes(avg_start,scoring_rate)) +
 geom_vline(xintercept=league_x,linetype='dashed',color='#A99D8C',linewidth=.5) +
 geom_hline(yintercept=league_y,linetype='dashed',color='#A99D8C',linewidth=.5) +
 geom_segment(aes(xend=lx,yend=ly),color='#7F776B',linewidth=.4) +
 scale_x_continuous(limits=c(28.2,33.25),breaks=29:33,labels=function(x) paste('Own',x),expand=c(0,0)) +
 scale_y_continuous(limits=c(.235,.53),breaks=seq(.25,.5,.05),labels=scales::label_percent(accuracy=1),expand=c(0,0)) +
 labs(title='WHERE DRIVES START. HOW OFTEN TEAMS SCORE.',
 subtitle='2025 NFL regular season | Each logo represents one team',
 x='Average starting field position',y='Drives ending in an offensive TD or made FG',
 caption=sprintf('Dashed lines: league averages (own %.1f | %.1f%% scoring rate).\nAll drives included, including kneel-only and end-of-half possessions.\nNearby logos are offset slightly and connected to their true values.\nSource: nflverse / nflreadr | Ward Walton',league_x,100*league_y)) +
 annotate('text',x=28.3,y=.525,label='INSIDE OWN 20: 27.7% SCORE  /  LEAGUE: 39.5%',hjust=0,vjust=1,
 color=accent,size=4.4,fontface='bold') +
 theme_minimal(base_size=13,base_family='Arial') +
 theme(plot.background=element_rect(fill=bg,color=NA),panel.background=element_rect(fill=bg,color=NA),
 panel.grid.minor=element_blank(),panel.grid.major=element_line(color='#E1D8C9',linewidth=.35),
 plot.title=element_text(size=20,face='bold',color=ink,margin=margin(b=8)),
 plot.subtitle=element_text(size=13,color=accent,margin=margin(b=15)),
 axis.title=element_text(size=12,color=ink),axis.title.x=element_text(margin=margin(t=12)),
 axis.title.y=element_text(margin=margin(r=12)),axis.text=element_text(color=ink),
 plot.caption=element_text(hjust=0,size=10,lineheight=1.3,margin=margin(t=15)),
 plot.margin=margin(22,25,18,20))
for(i in seq_len(nrow(teams))) {
 logo <- png::readPNG(file.path('outputs/first_down_drives_2025/logos',paste0(teams$posteam[i],'.png')))
 p <- p + annotation_custom(grid::rasterGrob(logo,interpolate=TRUE),
 xmin=teams$lx[i]-.105,xmax=teams$lx[i]+.105,ymin=teams$ly[i]-.0067,ymax=teams$ly[i]+.0067)
}
ggsave(file.path(output_dir,'team_field_position_scoring.png'),p,width=12,height=10,dpi=200,device=ragg::agg_png)
