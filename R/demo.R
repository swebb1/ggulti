
## Create a pitch
pitch = ggpitch(type="half_attack",endzone_fill = "#AA4422")

## Set players and positions
players <- list(
  player("H1", show=T, x=c(5),y=c(75), alpha = 0.8, frame = 1:3),
  player("H2", show=T, x=c(14), y=c(75), alpha = 0.5, frame = 1:3),
  player("C1", show=T, x=c(18,18,6), y=c(82,98,98), alpha = 0.8,frame = 1:3),
  player("COACH", show=T,object = "Coach",x = c(38),y=c(70),alpha = 1,frame = 1:3)
)

## Add defenders to players
defenders <- list(
  defender(players[[1]],"D1",show=T,xjust=c(2),yjust=c(2)),
  defender(players[[2]],"D2",show=T,xjust=c(-3),yjust=c(4)),
  defender(players[[3]],"D3",show=T,xjust=c(-3,-3,3),yjust=c(-1,-1,-5))
)
## Join players and defenders
player_list <- c(players,defenders)
plot_play(pitch,object_list = player_list,animate=T)

## Add throws between players
throws <- list(
  throw(player_list,"T1",F,"H1","C1",frame=1:2,throw_frame = 3,arrow_shape = "bhrc")
)

## Create arrow list
arrow_list = list(
  player_paths(player_list), ## Add arrows in front of running players
  throws
)

## Add a disc
disc_list = list(
  disc(throws[[1]],"D1",F,frame = 1:3,release_frame = 3)
)

## Combine players and disc
object_list <- c(player_list,disc_list)

## Plot the play
plot_play(pitch, arrow_list, object_list,static_frame = 3,keep_arrows = T)
plot_play(pitch,arrow_list,object_list,animate=T)

gp = plot_play(pitch, arrow_list = arrow_list, object_list,animate = T,static_frame = 2,keep_arrows = T)

ga=gp +
  transition_states(states = factor(frame,levels=1:3),transition_length = 1,state_length = 1,wrap=T)

animate(ga, renderer = gifski_renderer())
