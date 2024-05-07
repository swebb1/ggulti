library(ggsoccer)
library(ggplot2)
library(dplyr)
library(purrr)
library(gganimate)
library(gifski)

ggpitch <- function (colour = "dimgray", fill = "white", type = "full", endzone_fill = "blue",
                     linewidth = 0.5, alpha = 1, endzone_alpha=0.2, linetype = "solid",
                     xmin = 0, xmax = 37, ymin = 0, ymax = 100, zone_depth = 18)
{
  p = ggplot()
  if(type=="full"){
    p = p +
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=fill,colour=colour,linewidth=linewidth,alpha=alpha,linetype=linetype) +
      geom_rect(aes(xmin=0,xmax=37,ymin=0,ymax=18),fill=endzone_fill,colour=colour,linewidth=linewidth,alpha=endzone_alpha,linetype=linetype) +
      geom_rect(aes(xmin=0,xmax=37,ymin=82,ymax=100),fill=endzone_fill,colour=colour,linewidth=linewidth,alpha=endzone_alpha,linetype=linetype)
  }
  if(type=="half_attack"){
    p = p +
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymax/2,ymax=ymax),fill=fill,colour=colour,linewidth=linewidth,alpha=alpha,linetype=linetype) +
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymax-zone_depth,ymax=ymax),fill=endzone_fill,colour=colour,linewidth=linewidth,alpha=endzone_alpha,linetype=linetype)+
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymax/2,ymax=ymax/2),fill=fill,colour=fill,alpha=alpha)
  }
  if(type=="half_defend"){
    p = p +
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax/2),fill=fill,colour=colour,linewidth=linewidth,alpha=alpha,linetype=linetype) +
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymin+zone_depth),fill=endzone_fill,colour=colour,linewidth=linewidth,alpha=endzone_alpha,linetype=linetype)+
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymax/2,ymax=ymax/2),fill=fill,colour=fill,alpha=alpha)
  }
  if(type=="blank"){
   p = p +
     geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=fill,alpha=alpha)
  }
  p
}

player <- function(label = "", show = F, team = "Offense", x, y, frame = 1, alpha = 0.5, label_pos = "down"){
  lshow = ""
  if(show == T){
    lshow = label
  }
  data.frame(label = label, show = lshow, team = team, x = x, y = y, frame = frame, alpha = alpha, label_pos = label_pos)
}

defender <-function(player,label = "", show = NA, team = "Defense", xjust, yjust, frame = NA, alpha = NA, label_pos = NA){
  if(is.na(frame)){
    frame = player$frame
  }
  if(is.na(alpha)){
    alpha = player$alpha
  }
  if(is.na(label_pos)){
    label_pos = player$label_pos
  }
  if(is.na(show)){
    show = player$show
  }
  lshow = ""
  if(show == T){
    lshow = label
  }
  data.frame(label = label, show = lshow, team = team, x = player$x + xjust, y = player$y + yjust, frame = frame, alpha = alpha, label_pos = label_pos)
}

pitch_arrow <- function(label = "", show = F, team = "Offense", type = "Cut", x, y, xend, yend, frame = 1, alpha = 1, arrow_shape = "straight", label_pos = "start_down"){
  lshow = ""
  if(show == T){
    lshow = label
  }
  data.frame(label = label, show = lshow, team = team, type = type, x = x, y = y, xend = xend, yend = yend, frame = frame, alpha = alpha, arrow_shape = arrow_shape, label_pos = label_pos)
}

player_paths <- function(player_list, teams = c("Offense")){
  arrows <- player_list |> map(~if(!is.null(.x)){
                      .x |>
                       mutate(xend = x, yend = y,
                              x = lag(x), y = lag(y),
                              type = "Cut", arrow_shape = "straight", show="", label_pos="start_down") |>
                       filter(!is.na(x), !(x == xend & y == yend)) |>
                       select(label,show,team,type,x,y,xend,yend,frame,alpha,arrow_shape,label_pos)
  })

  bind_rows(arrows) |> filter(team %in% teams)
}

throw <- function(player_list,label,show=F,from,to,frame=1,throw_frame,alpha=1,arrow_shape="straight",space_x=NA,space_y=NA,label_pos="start_down"){
  lshow = ""
  if(show == T){
    lshow = label
  }

  p <- player_list |> bind_rows()

  team = p |> filter(label == from, frame == throw_frame) |> pull(team)
  x = p |> filter(label == from, frame == throw_frame) |> pull(x)
  y = p |> filter(label == from, frame == throw_frame) |> pull(y)

  ## Throw to a defined space
  if(to == "space"){
    if(is.na(space_x)){
      xend = x
    }
    else{
      xend = space_x
    }
    if(is.na(space_y)){
      yend = y
    }
    else{
      yend = space_y
    }
  }
  else{
    xend = p |> filter(label == to, frame == throw_frame) |> pull(x)
    yend = p |> filter(label == to, frame == throw_frame) |> pull(y)
  }

  data.frame(label = label, show = lshow, team = team, type = "Throw", x = x, y = y, xend = xend, yend = yend, frame = frame, alpha = alpha, arrow_shape = arrow_shape, label_pos = label_pos)
}

arrowtypes = c("Cut" = "solid", "Throw" = "dashed", "Label" = "dotted")

plot_play <- function(pitch,arrow_list=NULL,player_list=NULL,static_frame=1,animate=F){
  arrows = bind_rows(arrow_list)
  players = bind_rows(player_list)

  p = pitch
  if(length(arrows) > 0){
    p = p +
    geom_curve(data=arrows |> filter(arrow_shape %in% c("fhrc","bhio")),
               aes(x = x, y = y, xend = xend, yend = yend,
                   colour=team, linetype=type, group = label),
               curvature = 0.2,
               arrow = arrow(length = unit(0.25, "cm"),
                             type = "closed")) +
    geom_curve(data=arrows |> filter(arrow_shape %in% c("bhrc","fhio")),
               aes(x = x, y = y, xend = xend, yend = yend,
                   colour=team, linetype=type, group = label),
               curvature = -0.2,
               arrow = arrow(length = unit(0.25, "cm"),
                             type = "closed")) +
    geom_segment(data=arrows |> filter(arrow_shape == "straight"),
                 aes(x = x, y = y, xend = xend, yend = yend,
                     colour=team, linetype=type, group = label),
                 arrow = arrow(length = unit(0.25, "cm"),
                               type = "closed")) +
    scale_linetype_manual(values=arrowtypes) +
    geom_label(data=arrows |> filter(!show == "" & label_pos=="start_up"),aes(x=x,y=y,colour=team,alpha=alpha,label=show),vjust=-0.5,show.legend = FALSE) +
    geom_label(data=arrows |> filter(!show == "" & label_pos=="end_up"),aes(x=xend,y=yend,colour=team,alpha=alpha,label=show),vjust=-0.5,show.legend = FALSE) +
    geom_label(data=arrows |> filter(!show == "" & label_pos=="start_down"),aes(x=x,y=y,colour=team,alpha=alpha,label=show),vjust=1.5,show.legend = FALSE) +
    geom_label(data=arrows |> filter(!show == "" & label_pos=="end_down"),aes(x=xend,y=yend,colour=team,alpha=alpha,label=show),vjust=1.5,show.legend = FALSE)
  }
  if(length(players) > 0){
    if(animate==F){
      players = players |> filter(frame==static_frame)
    }
    p = p +
    geom_point(data=players,aes(x=x,y=y,colour=team,alpha=alpha,group=label),size=8) +
    geom_text(data=players |> filter(!show == "" & label_pos=="down"), aes(x=x,y=y,colour=team,alpha=alpha,label=show),vjust=3,show.legend = FALSE)
    #geom_text(data=players |> filter(!show == "" & label_pos=="down"),aes(x=x,y=y,colour=team,alpha=alpha,label=show),vjust=3,show.legend = FALSE) +
    #geom_text(data=players |> filter(!show == "" & label_pos=="up"),aes(x=x,y=y,colour=team,alpha=alpha,label=show),vjust=-2,show.legend = FALSE) +
    #geom_text(data=players |> filter(!show == "" & label_pos=="right"),aes(x=x,y=y,colour=team,alpha=alpha,label=show),hjust=-2,show.legend = FALSE) +
    #geom_text(data=players |> filter(!show == "" & label_pos=="left"),aes(x=x,y=y,colour=team,alpha=alpha,label=show),hjust=3,show.legend = FALSE)
  }
  p +
    labs(x=NULL,y=NULL,colour="Team",linetype="Arrow")+
    scale_alpha_identity() +
    guides(alpha="none") +
    theme_minimal()
}
plot_play(pitch, arrow_list, player_list)

players <- list(
  player("A", show=T, x=c(20,20,10),y=c(30,75,30), frame = 0:2, label_pos = "right"),
  player("J-DOG", show=T, x=c(10), y=c(20), alpha = 1, frame = 1:2),
  player("D1",team="Defense",x=c(12), y=c(22), frame = 1:2)
)

defenders <- list(
  defender(player_list[[1]],"DA",show=T,xjust=-3,yjust=c(0,0,3),label_pos = "up")
)
player_list <- c(players,defenders)

throws <- list(
  throw(player_list,"T1",F,"J-DOG","A",frame=1:2,throw_frame = 2),
  throw(player_list,"T2",F,"A","space",frame=1:2,throw_frame = 2,space_y = 90,arrow_shape = "bhrc")
)

arrow_list = list(
  player_paths(player_list),
  throws,
  pitch_arrow("GOAL", team = "Offense", show = T, x = 5, y = 70, xend = 8, yend =  75, type = "Label", frame = 2,label_pos = "start_down")
)

pitch = ggpitch(type="full",endzone_fill = "#AA4422")

plot_play(pitch, arrow_list, player_list)
gp = plot_play(pitch, arrow_list = NULL, player_list,animate = T)

ga=gp +
    transition_states(states = frame) #,transition_length = 1,state_length = 1,)

animate(ga, renderer = gifski_renderer())

arrow_list
