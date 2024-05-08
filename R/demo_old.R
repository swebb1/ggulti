players <- list(
  player("A", show=T, x=c(20,20,10),y=c(30,75,50), frame = 1:3, label_pos = "right"),
  player("J-DOG", show=T, x=c(10), y=c(20), alpha = 1, frame = 1:3),
  player("D1",object="Defense",x=c(12), y=c(22), frame = 1:3)
)

defenders <- list(
  defender(players[[1]],"DA",show=T,xjust=c(-3,-3,3),yjust=c(0,0,5),label_pos = "left")
)
player_list <- c(players,defenders)

throws <- list(
  throw(player_list,"T1",F,"J-DOG","A",frame=2,throw_frame = 3),
  throw(player_list,"T2",F,"A","space",frame=3,throw_frame = 3,space_y = 90,arrow_shape = "bhrc")
)

arrow_list = list(
  player_paths(player_list),
  throws,
  pitch_arrow("Roll Curve", object = "Offense", show = T, x = 5, y = 70, xend = 8, yend =  75, type = "Label", frame = 3,label_pos = "start_down")
)

disc_list = list(
  disc(throws[[1]],"D1",xjust = c(-1,-1),yjust = c(1,1))
)
object_list <- c(player_list,disc_list)

pitch = ggpitch(type="full",endzone_fill = "#AA4422")

plot_play(pitch, arrow_list, object_list,static_frame = 3)
gp = plot_play(pitch, arrow_list = arrow_list, object_list,animate = T,static_frame = 2)

ga=gp +
  transition_states(states = factor(frame,levels=1:4),transition_length = 1,state_length = 1,wrap=F)

animate(ga, renderer = gifski_renderer())
