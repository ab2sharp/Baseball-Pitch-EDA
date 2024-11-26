#library(car)
#library(rgl)
#library(baseballr)
#library(dbplyr)
#library(knitr)
#library(DBI)
#library(pracma)
#library(tidyverse)
#library(magrittr)

#library(devtools)
devtools::install_github("BillPetti/baseballr")
library(baseballr)

#test the scraping functions
#Call the Statcast data from April 6th 2018 and compare resulting tables
scrape_statcast_savant("2018-04-06", "2018-04-06", type = "pitcher")%>%
  select(pitch_type:pitcher)%>%
  kable()
 
scrape_statcast_savant_pitcher_all("2018-04-06", "2018-04-06")%>%
  select(pitch_type:pitcher)%>%
  kable()


#Initialize the database

#Create a new sqlite database
db <- src_sqlite("statcastDB.sqlite3", create = TRUE)

#Grab data from first day of the 2018 regular seaons
initial_data <- scrape_statcast_savant_pitcher_all("2018-03-29", "2018-03-29")

#Write the data to the database you created
dbWriteTable(db$con, "statcast", initial_data)

#Generate the dates to be used in the baseballr pitcher_all function. 

#Set the first interval to be 9 days, including the inital date
dateInitial <- as.Date("2018-03-30")
endFirstDateInterval <- dateInitial + 8
#The last day of the season
dateFinal <- as.Date("2018-10-01")

#Generate the end points for each date interval. Append the last day to finish_dates 
#because the final interval is less than 9 days
begin_dates <- seq(dateInitial, dateFinal, "9 days")
finish_dates <- seq(endFirstDateInterval, dateFinal, "9 days") %>%
  append(dateFinal)

mapply(writeStatcastToDB, 
       date_init = begin_dates, 
       date_fin = finish_dates, 
       table = "statcast")

#Moody_Mudskippers table writing function
# Writes a new table in the database "data", with the name "name".
create <- function(data,name){
  DBI::dbSendQuery(data$src$con,
                   paste("CREATE TABLE", name,"AS", dbplyr::sql_render(data)))
  dplyr::tbl(data$src$con,name)
}

#Use this code to create a connection to the database
db <-  src_sqlite("statcastDB.sqlite3", create = FALSE)

#Choose columns to remove. These columns are know to be empty or deprecated
remove <- c("spin_dir", 
            "spin_rate_deprecated", 
            "break_angle_deprecated", 
            "break_length_deprecated", 
            "game_type", 
            "tfs_deprecated", 
            "tfs_zulu_deprecated", 
            "umpire", 
            "sv_id")

#Remove the columns and write data as a new table in the database
db %>%
  tbl("statcast")%>%
  select(-remove)%>%
  create("cleancast")

#Unique values of the description variable
db%>%
  tbl("cleancast")%>%
  distinct(description)%>%
  kable()

#Number of unique games played in 2018 season
db%>%
  tbl("cleancast")%>%
  distinct(game_pk)%>%
  count()%>%
  kable()

#A primary key for the data
db%>%
  tbl("cleancast")%>%
  select(game_pk, at_bat_number, pitch_number)%>%
  collect()%>% 
  count(game_pk, at_bat_number, pitch_number)%>%
  filter(n>1)%>%
  nrow() == 0

# ----------------------------------------
#  Begin Analyzing Erroneous Measurements
# ----------------------------------------

#Count the number of observations
db%>%
  tbl("cleancast")%>%
  tally()

#Check pitch_type values
#Vector with mlbam defined pitch types
pitch_types <- c("FF", "CH", "FC", "FT", "FS", "SL", "CU", 
                 "KC", "SI", "FO", "PO", "EP", "SC", "KN" )
db%>%
  tbl("cleancast")%>%
  filter(!pitch_type %in% pitch_types)%>%
  select(pitch_type, player_name, events, release_speed, vx0, ax)%>%
  collect()%>%
  head()

#Check ball and strike values
acceptaballs <- c(0,1,2,3)
allowable_strikes <- c(0,1,2)
db%>%
  tbl("cleancast")%>%
  filter(!balls %in% acceptaballs |
           !strikes %in% allowable_strikes)%>%
  select(game_pk, player_name, events, balls, strikes)%>%
  collect()%>%
  kable()

#4 balls, BlueJays game
db%>%
  tbl("cleancast")%>%
  filter(game_pk == 530969,
         player_name == "Hector Santiago",
         inning == 8,
         outs_when_up ==2)%>%
  select(pitch_type, player_name, balls, strikes, release_speed, zone)%>%
  collect()%>%
  kable()

#4 balls, Orioles game
db%>%
  tbl("cleancast")%>%
  filter(game_pk == 529872,
         player_name == "Miguel Castro",
         inning == 3,
         outs_when_up ==1)%>%
  select(pitch_type, player_name, balls, strikes, plate_x, plate_z, des)%>%
  collect()%>%
  kable()

#Time to reach given y position function
getTimeToReachYPosition <- function(vy0, ay, y0, y_p){
  return((-vy0 - sqrt(vy0^2 - 2*ay*(y0-y_p)))/ay)
}

#Velocity at given time in trajectory
velocityAtTimet <- Vectorize(function(v0, a, t){
  vt <-  v0  + a*t
  return(vt)
})

#3D vector length function
vectorNorm <- Vectorize(function (x,y,z){
  return(sqrt(x^2 + y^2 + z^2))
} )
```

#check release speeds
db%>%
  tbl("cleancast")%>%
  collect()%>%
  #The value of t in the trajectory model which corresponds to release time
  mutate(time_release = getTimeToReachYPosition(vy0, ay, y0=50, y_p = release_pos_y),
         time_home = getTimeToReachYPosition(vy0, ay, y0=50, y_p = 17/12 ), #time at home plate
         flight_time = time_home - time_release #The total flight time of the ball in seconds
  )%>%
  mutate(vxR = velocityAtTimet(vx0, ax, t = time_release), 
         vyR= velocityAtTimet(vy0, ay, t = time_release), 
         vzR = velocityAtTimet(vz0, az, t = time_release)
  )%>%
  mutate(calc_speed_release = vectorNorm(vxR, vyR, vzR)*(15/22), #15/22 converts ft/sec to mph
         diff_speed = release_speed - calc_speed_release)%>%
  filter(abs(diff_speed) > 1)%>%
  select(game_pk, player_name, release_speed, calc_speed_release, diff_speed)%>%
  kable()

#Check games and PAs with odd speed values
db%>%
  tbl("cleancast")%>%
  filter(game_pk == 530204,
         at_bat_number %in% c(39,40,41,42))%>%
  select(pitch_type, player_name, release_speed, des, inning)%>%
  collect()%>%
  View()

#Average splitter speed
db%>%
  tbl("cleancast")%>%
  filter(pitch_type == "FS")%>%
  pull(release_speed)%>% #grabs the specified table column
  mean()

#Position at time t function 
position_at_t <- function(p_initial, velo_initial, acceleration, t, t_initial){
  return(p_initial + velo_initial*(t-t_initial)+(0.5)*acceleration*(t-t_initial)^2)
}

#Testing p_z and p_x values
db%>%
  tbl("cleancast")%>%
  collect()%>%
  #time _release gives the value of t that goes into the trajectory equation that 
  #corresponds to the position of the ball at release. time_home is the value corresponding 
  #to crossing home plate flight_time gives the total flight in seconds
  mutate(time_release = getTimeToReachYPosition(vy0, ay, y0=50, y_p = release_pos_y), 
         time_home = getTimeToReachYPosition(vy0, ay, y0=50, y_p = 17/12 ),
         flight_time = time_home - time_release
  )%>%
  mutate(vxR = velocityAtTimet(vx0, ax, t = time_release), #The release velocities
         vyR= velocityAtTimet(vy0, ay, t = time_release), 
         vzR = velocityAtTimet(vz0, az, t = time_release)
  )%>%
  #Get plate positions according to the given trajectories
  mutate(p_z = position_at_t(release_pos_z, vzR, az, time_home, time_release), 
         p_x = position_at_t(release_pos_x, vxR, ax, time_home, time_release)
  )%>%
  #Filtering by values for which the difference is unusual
  filter(abs(plate_x - p_x) > 1 | abs(plate_z - p_z) > 1)%>%
  select(game_pk, pitch_type, player_name, plate_x, p_x, plate_z, p_z, zone)%>%
  head()%>%
  kable()

#Amend all errors that were found and write into new table
db%>%
  tbl("cleancast")%>%
  collect()->
  fix_cleancast
fix_cleancast%>%
  #Remove pitches with uniterpretable pitch type / no Statcast measurments
  filter(pitch_type %in% pitch_types)%>%  
  mutate(balls = replace(balls, balls==4, 3))%>%
  #time _release: gives the value of t that returns position at release from trajetory eqn 
  #time_home: value of t that returns the position the ball at home plate from trajectory eqn
  #flight_time: total time from release to home plate, in seconds
  mutate(time_release = getTimeToReachYPosition(vy0, ay, y0=50, y_p = release_pos_y),
         time_home = getTimeToReachYPosition(vy0, ay, y0=50, y_p = 17/12 ), 
         flight_time = time_home - time_release 
  )%>%
  mutate(vxR = velocityAtTimet(vx0, ax, t = time_release),  #The release velocities
         vyR = velocityAtTimet(vy0, ay, t = time_release), 
         vzR = velocityAtTimet(vz0, az, t = time_release)
  )%>%
  #Fix incorrect release speeds
  mutate(calc_speed_release = vectorNorm(vxR, vyR, vzR)*(15/22), #15/22 converts ft/sec to mph
         diff_speed = release_speed - calc_speed_release,
         release_speed = if_else(abs(diff_speed) > 1, round(calc_speed_release,1), release_speed )
  )%>%
  #Get plate positions according to the given trajectories.
  mutate(p_z = position_at_t(release_pos_z, vzR, az, time_home, time_release), 
         p_x = position_at_t(release_pos_x, vxR, ax, time_home, time_release),
         zone = if_else(abs(plate_x - p_x) >1,    #Mirror the zones to be correct
                        case_when(zone == 1 ~3,
                                  zone == 3 ~1,
                                  zone == 4 ~6,
                                  zone == 6 ~4,
                                  zone == 7 ~9,
                                  zone == 9 ~7,
                                  zone == 11 ~12,
                                  zone == 12 ~11,
                                  zone == 13 ~14,
                                  zone == 14 ~13
                        ), 
                        zone)
  )%>%
  #Drop old plate values to keep plate values consistent with trajectories.
  select(-c(plate_x, plate_z))%>%
  dbWriteTable(db$con, "calccast", ., overwrite = TRUE)

rm(fix_cleancast)

# -----------------------------
#  Begin Analyzing Pitch Types
# -----------------------------

#Number of observations of each pitch type
db%>%
  tbl("calccast")%>%
  pull(pitch_type)%>%
  table()

#Graph of release speed distributions by type
db%>%
  tbl("calccast")%>%
  filter(!is.na(release_speed),
         !pitch_type %in% c("FO", "PO", "SC", "KN", "EP"),
         #Remove poorly thrown pitches, which may skew the data
         !description %in% c("blocked_ball", "hit_by_pitch")
  )%>%
  select(pitch_type, release_speed)%>%
  ggplot(., aes(pitch_type, release_speed, fill = pitch_type)) +
  geom_violin(scale = "width",
              adjust = 0.7,
              draw_quantiles = c(0.25, 0.5, 0.75)
  )+ 
  coord_cartesian(ylim = c(67,105)) +
  theme_dark() 

#Graph release spin rates by pitch type
db%>%
  tbl("calccast")%>%
  filter(!is.na(release_spin_rate),
         !pitch_type %in% c("FO", "PO", "SC", "KN", "EP"),
         !description %in% c("blocked_ball", "hit_by_pitch"),
         release_spin_rate < 3400
  )%>%
  select(pitch_type, release_spin_rate)%>%
  ggplot(., aes(pitch_type, release_spin_rate, fill = pitch_type))+
  geom_violin(scale = "width",
              draw_quantiles = c(0.25, 0.5, 0.75)
  )+ 
  coord_cartesian(ylim = c(600,3400)) +
  theme_dark()

#Function for getting arc length of each trajectory
#library(pracma)
arc_length <- Vectorize(function(x, y, z, vx0, vy0, vz0, ax, ay, az, tr, tf){
  f <- function(t){
    c(x + (t-tr)*vx0 + 0.5*ax*(t-tr)^2, 
      y + (t-tr)*vy0 + 0.5*ay*(t-tr)^2, 
      z + (t-tr)*vz0 + 0.5*az*(t-tr)^2)
  }
  arc <- arclength(f, tr, tf)
  return(as.numeric(arc$length))
})

#Add arclength values to calccast so that they do not need to be recalculated in the future
db%>%
  tbl("calccast")%>%
  collect()%>%
  mutate(arcLength = arc_length(release_pos_x, release_pos_y, release_pos_z, 
                                vxR, vyR, vzR, 
                                ax, ay, az, 
                                tr = time_release, 
                                tf = time_home),
         Pitch_line = vectorNorm(p_x - release_pos_x, 
                                 (17/12)-release_pos_y, 
                                 p_z - release_pos_z),
         arcLength_diff = arcLength - Pitch_line
  )%>%
  dbWriteTable(db$con, "calccast", . , overwrite = TRUE)

#Graph arc length diffs vs pitch type
db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("FO", "PO", "SC", "KN", "EP"),
         !description %in% c("blocked_ball", "hit_by_pitch")
  )%>%
  select(pitch_type,
         arcLength_diff
  )%>%
  collect()%>%
  # Plot the difference in arclength, with units measure in inches
  ggplot(aes(pitch_type, 12*arcLength_diff, fill = pitch_type)) +
  geom_violin(scale = "width",
              draw_quantiles = c(0.25, 0.5, 0.75)
  ) +
  coord_flip(ylim = c(0,2)) +
  ggtitle("Arc_length Difference distributions by Pitch Type, by Zone") +
  labs(y = "Difference in arc length, in inches") +
  theme_dark()

#Get the table of mean value projections
db%>%
  tbl("calccast")%>%
  filter(p_throws == "R",
         !pitch_type %in% c("FO", "PO", "KN", "EP"),
         !description %in% c("blocked_ball", "hit_by_pitch"),
         !is.na(vxR)
  )%>%
  select(pitch_type, 
         release_pos_x, 
         release_pos_y,
         release_pos_z,
         vxR,
         vyR,
         vzR,
         ax,
         ay,
         az)%>%
  collect()%>%
  group_by(pitch_type, keep = FALSE)%>%
  summarise_all(mean) ->
  means_tbl

#Remove redundant keep column and add the time at home plate
means_tbl%>%
  select(-keep)%>%
  ungroup()%>%
  mutate(time_home = getTimeToReachYPosition(vyR, ay, release_pos_y, 17/12))->
  means_tbl

# ================================================================== #
#  A function that will generate 1400 points in the projection of a  #
#  pitch trajectory, for the purpose of plotting                     #
# ================================================================== #
plotProj <- function(xr, 
                     yr, 
                     vxr, 
                     vyr, 
                     ax, 
                     ay,
                     tf,
                     pitch_type){
  interval <- seq(0, tf, length.out = 1400)
  tibble( pitch_type, 
          x= xr + vxr*interval + (0.5)*ax*interval^2,
          y= yr + vyr*interval + (0.5)*ay*interval^2
  )
}

#Graph the top-down projection plots

# Create a table called projection_plots, containing the points to be plotted 
#for each  of the mean trajectories 
means_tbl%$%
  do.call(rbind, mapply( plotProj, xr = release_pos_x, 
                         yr = release_pos_y, 
                         vxr = vxR, 
                         vyr = vyR, 
                         ax = ax, 
                         ay = ay, 
                         tf = time_home,
                         pitch_type = pitch_type,
                         SIMPLIFY = FALSE)
  )->
  projection_plots

# Add a column called "repeat_type" which is a carbon copy of the pitch_type column
projection_plots%>%
  mutate(repeat_type = pitch_type)->
  projection_plots

#Here we plot the trajectories. 
projection_plots%>%
  ggplot(aes(x=-y, y=x, group = pitch_type, color = pitch_type)) +
  #Creates the swing bar and the accompanying text
  annotate("rect", xmin=-30, xmax=-25, 
           ymin=-Inf, ymax=Inf, 
           alpha=0.15, fill="black"
  ) +
  annotate("text", 
           x = -27.5, 
           y = -1.9, 
           label = "decision \n to swing", color = "white", alpha = 0.7
  ) +
  #Creates the background plot which includes all of the trajectories
  geom_line(data = transform(projection_plots, pitch_type = NULL), 
            aes(group = repeat_type), 
            color = "ivory", alpha = 0.25
  ) +
  #Graphs the colored trajectories
  geom_line(lwd = 1.4) +
  scale_color_hue(l=85, c=100) +
  #Graphs the vertical bar representing the strike zone
  annotate("segment", 
           y = (-17/24), 
           yend = Inf, 
           x =-17/12, 
           xend = -17/12, 
           color = "black", alpha = 0.7, lwd = 1.1
  ) +
  #Title and theme options
  theme_dark() +
  ggtitle("Top View of Average Pitch Trajectories") +
  xlab("Distance from back of Home Plate") +
  ylab("Lateral Distance from middle of Home Plate") +
  theme(legend.position = "none") +
  facet_wrap(~pitch_type, nrow = 3)
rm(projection_plots)


#Graph the side view projections
means_tbl%$%
  do.call(rbind, mapply(
    plotProj, xr = release_pos_y, 
    yr = release_pos_z, 
    vxr = vyR, 
    vyr = vzR, 
    ax = ay, 
    ay = az, 
    tf = time_home,
    pitch_type = pitch_type,
    SIMPLIFY = FALSE)) ->
  projection_plots

projection_plots%>%
  mutate(type_repeat = pitch_type)->
  projection_plots
projection_plots%>%
  ggplot(aes(x=-x, y=y, group = pitch_type, color = pitch_type)) +
  annotate("rect", xmin=-30, xmax=-25, 
           ymin=-Inf, ymax=Inf, 
           alpha=0.15, fill="black"
  ) +
  annotate("text", x = -27.5, 
           y = 2, 
           label = "decision \n to swing", color = "white", alpha = 0.7
  ) +
  geom_line(data = transform(projection_plots, pitch_type = NULL), 
            aes(group = type_repeat), 
            color = "ivory", alpha = 0.2
  ) +
  geom_line(lwd = 1.4) +
  scale_color_hue(l=85, c=100) +
  annotate("segment", y = -Inf, 
           yend = 3.5, 
           x =-17/12, 
           xend = -17/12, 
           color = "black", alpha = 0.7, lwd = 1.1
  ) +
  theme_dark() +
  ggtitle("Side View of Average Pitch Trajectories") +
  xlab("Distance from back of home plate, in negative feet") +
  ylab("Height off the ground, in feet") +
  theme(legend.position = "none") +
  facet_wrap(~pitch_type, nrow = 3)

#Function to plot the 3D trajectory
#Requires rgl package

#plot and label a pitch 
plotPitch <- function(xr=2.329, 
                      yr, 
                      zr=0, 
                      vxr=0, 
                      vyr, 
                      vzr=0, 
                      ax=0, 
                      ay, 
                      az=0,
                      tr=0, 
                      tf, 
                      label = "a pitch", 
                      color = "red",
                      lwd = 4.75,
                      alpha = 1){
  interval <- seq(tr, tf, length.out = 500)
  pos <- runif(1)
  lines3d( x= xr + vxr*(interval-tr) + (0.5)*ax*(interval-tr)^2,
           y= yr + vyr*(interval-tr) + (0.5)*ay*(interval-tr)^2,
           z= zr + vzr*(interval-tr) + (0.5)*az*(interval-tr)^2,
           color = color,
           lwd = lwd,
           alpha = alpha
  )
  #text3d( x= xr + vxr*(pos*tf-tr) + (0.5)*ax*(pos*tf-tr)^2 + 0.05,
  #        y= yr + vyr*(pos*tf-tr) + (0.5)*ay*(pos*tf-tr)^2,
  #        z= zr + vzr*(pos*tf-tr) + (0.5)*az*(pos*tf-tr)^2,
  #        color = color,
  #        text = label,
  #        font = 2
  #)
}

#Plots axes in rgl space
#Not my function, but I made a couple of adjustments
# x, y, z : numeric vectors corresponding to
#  the coordinates of points
# axis.col : axis colors
# xlab, ylab, zlab: axis labels
# show.plane : add axis planes
# show.bbox : add the bounding box decoration
# bbox.col: the bounding box colors. The first color is the
# the background color; the second color is the color of tick marks
rgl_add_axes <- function(x, y, z, axis.col = "grey",
                         xlab = "", ylab="", zlab="", show.plane = TRUE, 
                         show.bbox = FALSE, bbox.col = c("#333377","black"))
{ 
  
  lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}
  # Add axes
  xlim <- lim(x); ylim <- c(17/12 - 0.5, 60.5); zlim <- c(0, 7)
  rgl.lines(xlim, c(0, 0), c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), ylim, c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), c(0, 0), zlim, color = axis.col)
  
  # Add a point at the end of each axes to specify the direction
  axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0), 
                c(0, 0, zlim[2]))
  rgl.points(axes, color = axis.col, size = 3)
  
  # Add axis labels
  rgl.texts(axes, text = c(xlab, ylab, zlab), color = axis.col,
            adj = c(0.5, -0.8), size = 2)
  
  # xy plane
  if(show.plane) 
    rgl.quads( x = c(xlim[1], 2.33, 2.33, xlim[1]), 
               z = c(0, 0, 0, 0),
               y = c(ylim[1], ylim[1], ylim[2], ylim[2]),
               color = "#90EE90",
               lit          = FALSE, 
               ambient      = "white",
               specular     = "white", 
               emission     = "white", 
               shininess    = 0, 
               smooth       = TRUE)
  
  #yz plane
  if(show.plane) 
    zlim <- zlim/1.1; ylim <- ylim /1.1
  rgl.quads( x = c(2.33, 2.33, 2.33, 2.33), 
             z = c(zlim[1], zlim[2], zlim[2], zlim[1]),
             y = c(ylim[1], ylim[1], 60.5, 60.5),
             color = "#B0E0E6",
             lit          = FALSE, 
             ambient      = "white",
             specular     = "white", 
             emission     = "white", 
             shininess    = 0, 
             smooth       = TRUE)
  
  # Add bounding box decoration
  if(show.bbox){
    rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5, 
             emission=bbox.col[1], specular=bbox.col[1], shininess=5, 
             xlen = 3, ylen = 3, zlen = 3) 
  }
}

#Graph the 3D trajectories
db%>%
  tbl("arccast")%>%
  filter(p_throws == "R",
         !(pitch_type %in% c("FO", "PO", "KN")))%>%
  select(pitch_type, 
         release_pos_x, 
         release_pos_y,
         release_pos_z,
         vxR,
         vyR,
         vzR,
         ax,
         ay,
         az)%>%
  collect()%>%
  group_by(pitch_type, keep = FALSE)%>%
  summarise_all(mean) ->
  tbl_of_means

#Remove redundant keep column
tbl_of_means%>%
  select(-keep)%>%
  ungroup() ->
  tbl_of_means

#Produce the color palette
palette <- colorRampPalette(c("darkviolet", "dodgerblue", "gold", "indianred"))



#Add time at home plate
tbl_of_means%>%
  mutate(time_at_home = getTimeToReachYPosition(vyR, ay, release_pos_y, 17/12)) ->
  tbl_of_means

#Get color based on relative speed
tbl_of_means%>%
  mutate(speed = sqrt(vxR^2 + vyR^2 + vzR^2))%>%
  arrange(speed)%>%
  add_column(color = palette(10)) ->
  tbl_of_means

# produce a 3D plot
open3d()

#3D trajectories
tbl_of_means%$%
  mapply(plotPitch, xr = release_pos_x,
         yr = release_pos_y,
         zr = release_pos_z,
         vxr = vxR,
         vyr = vyR,
         vzr = vzR,
         ax = ax,
         ay = ay,
         az = az,
         tf = time_at_home,
         label = pitch_type,
         color = color,
         lwd = 3.5,
         alpha= 0.75)

#YZ projections
tbl_of_means%$%
  mapply(plotPitch, yr = release_pos_y,
         zr = release_pos_z,
         vyr = vyR,
         vzr = vzR,
         ay = ay,
         az = az,
         tf = time_at_home,
         label = pitch_type,
         color = color,
         lwd = 2)

#XY projections 
tbl_of_means%$%
  mapply(plotPitch, xr = release_pos_x,
         yr = release_pos_y,
         vxr = vxR,
         vyr = vyR,
         ax = ax,
         ay = ay,
         tf = time_at_home,
         label = pitch_type,
         color = color,
         lwd = 2)



#Add strikezone
rgl.lines(c(-17/24, 17/24), c(17/12, 17/12), c(1.5, 1.5), color = "black")
rgl.lines(c(-17/24, -17/24), c(17/12, 17/12), c(1.5, 3.5), color = "black")
rgl.lines(c(-17/24, 17/24), c(17/12, 17/12), c(3.5, 3.5), color = "black")
rgl.lines(c(17/24, 17/24), c(17/12, 17/12) ,c(1.5, 3.5), color = "black")


#Add axes
tbl_of_means%$%
  rgl_add_axes(x = release_pos_x, 
               y = release_pos_y, 
               z = release_pos_z)
aspect3d(1,0.9,1) # zoom


#bgplot3d({
#  plot.new()
#  title(main = "Average Pitch Trajectories", line = 3)
#})

#add a legend
tbl_of_means%$%
  legend3d("topright", legend = pitch_type, pch = 16, col = color, cex=1, inset=c(0.02))

#movie3d(spin3d(axis=c(0,0,1), rpm=4), dir=getwd(), duration=11, fps=30, movie="Pitches Plot")


# ------------------------- #
#  Analysis of Pitch Roles  #
# ------------------------- #

#All pitchers
db%>%
  tbl("calccast")%>%
  filter(description =="called_strike",
         !pitch_type %in% c("PO", "FO", "EP", "SC", "KN"),
         (abs(p_x) > 17/24) | 
           p_z > 3.5 | 
           p_z < 1.5)%>%
  # Notice we do not call the collect() function here before
  # sending to ggplot. This was initially a mistake. I do not know
  # how it retrieves the data when it is left this way. Possibly
  # the pass to ggplot implicitly calls the collect() function
  select(pitch_type, description, p_x, p_z)%>%
  ggplot(., aes(x= p_x, y = p_z)) + 
  geom_point( color = "mistyrose", shape = 19, alpha = 0.1) +
  geom_rect(aes(xmin = -17/24,
                xmax =  17/24,
                ymin =  1.5,
                ymax =  3.5), 
            fill = NA, color = "black", size = 1) +
  facet_wrap(~pitch_type, nrow = 3) +
  ggtitle("Strikzone by Pitch") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_dark()

#Called Zones, lefty and right hitters
#Right handed batters
db%>%
  tbl("calccast")%>%
  filter(description =="called_strike",
         !pitch_type %in% c("PO", "FO", "EP", "SC", "KN"),
         stand == "R",
         (abs(p_x) > 17/24) | 
           p_z > 3.5 | 
           p_z < 1.5)%>%
  select(pitch_type, description, p_x, p_z)%>%
  ggplot(., aes(x= p_x, y = p_z)) + 
  geom_point( color = "mistyrose", shape = 19, alpha = 0.1) +
  geom_rect(aes(xmin = -17/24,
                xmax =  17/24,
                ymin =  1.5,
                ymax =  3.5), 
            fill = NA, color = "black", size = 1
  )+
  facet_wrap(~pitch_type, nrow = 3) +
  ggtitle("Strikzone by Pitch, Righty Batter") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_dark()


#Left handed batters
db%>%
  tbl("calccast")%>%
  filter(description =="called_strike",
         !pitch_type %in% c("PO", "FO", "EP", "SC", "KN"),
         stand == "L",
         (abs(p_x) > 17/24) | 
           p_z > 3.5 | 
           p_z < 1.5)%>%
  select(pitch_type, description, p_x, p_z)%>%
  ggplot(., aes(x= p_x, y = p_z)) + 
  geom_point( color = "mistyrose", shape = 19, alpha = 0.1) +
  geom_rect(aes(xmin = -17/24,
                xmax =  17/24,
                ymin =  1.5,
                ymax =  3.5), 
            fill = NA, color = "black", size = 1
  )+
  facet_wrap(~pitch_type, nrow = 3) +
  ggtitle("Strikzone by Pitch, Lefty Batter") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_dark()

#Swing percentage plots

# Vectors used to specify text locations inside the zones
# They are used in the mutate/case_when below
zones_x <- c(1,4,7)
zones_y <- c(1,2,3)
corners <- c(11,12,13,14)

# Used to plot the actual zones in the Statcast coordinates
# using ggplot
test_zones <- tibble(
  x_lower = rep(seq(-17/24, 17/72, length.out =3), each = 3),
  x_upper = rep(seq(-17/72,17/24, length.out =3), each = 3),
  y_lower = rep( seq(3/2,17/6, length.out = 3), 3),
  y_upper = rep( seq(13/6,7/2, length.out = 3), 3),
)

#All descriptions that correspond to a batter swinging their bat at the pitch
swing_events <- c("foul",
                  "foul_tip",
                  "hit_into_play",
                  "hit_into_play_no_out",
                  "hit_into_play_score",
                  "swinging_strike",
                  "swinging_strike_blocked")
db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("PO", "FO", "EP", "SC", "KN"),
         p_throws == "R",
         stand == "R",
         !is.na(zone)
  )%>%
  select(pitch_type, 
         description, 
         p_x,
         p_z, 
         zone
  )%>%
  collect()%>%
  group_by(pitch_type, zone)%>%
  #Calculate whiff percent for each pitch type, in each zone
  summarise(swing_prop = 
              sum(description %in% swing_events
              )/n()
  )%>%
  #Defines the coordinates for plotting the whiff percentage 
  #in the center of zone to which it corresponds
  mutate( zone_x = case_when(
    zone %in% zones_x      ~ -17/36,
    zone %in% (zones_x+1)  ~  0,
    zone %in% (zones_x+2)  ~  17/36,
    zone == 11           ~ -34/36,                 
    zone == 12           ~  34/36, 
    zone == 13           ~ -34/36,
    zone == 14           ~  34/36,
  ),
  zone_y = case_when( 
    zone %in% zones_y    ~  19/6,
    zone %in% (zones_y+3)  ~  15/6,
    zone %in% (zones_y+6)  ~  11/6,
    zone == 11           ~  23/6,                 
    zone == 12           ~  23/6, 
    zone == 13           ~  7/6,
    zone == 14           ~  7/6,
  )
  ) ->
  summarized_swings_R

db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("PO", "FO", "EP", "SC", "KN"),
         p_throws == "R",
         stand == "R",
         !is.na(zone)
  )%>%
  select(pitch_type, 
         description, 
         p_x,
         p_z, 
         zone
  )%>%
  collect()%>%
  group_by(pitch_type, zone)%>%
  ggplot() +
  #A heatmap of the pitch locations
  stat_density_2d( aes( x = p_x, y = p_z, fill = ..density..), 
                   geom = "raster",
                   contour = FALSE,
                   show.legend = FALSE
  ) + 
  scale_fill_distiller(palette="OrRd", direction=1) +
  coord_cartesian(xlim = c(-2, 2),
                  ylim = c(0.75,4)
  ) + 
  #This plots the percentages
  geom_text(data = summarized_swings_R,
            aes(x=zone_x, y=zone_y, label = paste0(round(100*swing_prop), "%"),
                fontface = "bold")
  ) +
  geom_rect(data = test_zones, 
            aes( xmin = x_lower,
                 xmax = x_upper, 
                 ymin = y_lower,
                 ymax = y_upper), 
            fill = NA, color = "grey20",lwd = 1
  ) +
  # Plots the exterior lines that define the zones outside of
  # the strike zone
  geom_segment(aes(x = 0, y = 3.5 , xend =  0, yend = 3.5 + (2/3))) +
  geom_segment(aes(x = 0, xend = 0, y = 0.8333, yend = 1.5)) +
  geom_segment(aes(x = 17/24, xend = (17/24)+(17/36), y = 2.5, yend = 2.5)) +
  geom_segment(aes(x = (-17/24 - 17/36), xend = -17/24, y = 2.5, yend = 2.5)) +
  ggtitle("Swing Percentages per Pitch Type, by Zone, Right-Handed Hitters (Catcher's Perspective)") +
  theme(legend.position = "none") +
  theme_dark() + 
  facet_wrap(~pitch_type, nrow = 3)

rm(summarized_swings_R)

#Same plot, lefties
db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("PO", "FO", "EP", "SC", "KN"),
         p_throws == "R",
         stand == "L",
         !is.na(zone)
  )%>%
  select(pitch_type, 
         description, 
         p_x,
         p_z, 
         zone
  )%>%
  collect()%>%
  group_by(pitch_type, zone)%>%
  #Calculate whiff percent for each pitch type, in each zone
  summarise(swing_prop = 
              sum(description %in% swing_events
              )/n()
  )%>%
  #Defines the coordinates for plotting the whiff percentage 
  #in the center of zone to which it corresponds
  mutate( zone_x = case_when(
    zone %in% zones_x      ~ -17/36,
    zone %in% (zones_x+1)  ~  0,
    zone %in% (zones_x+2)  ~  17/36,
    zone == 11           ~ -34/36,                 
    zone == 12           ~  34/36, 
    zone == 13           ~ -34/36,
    zone == 14           ~  34/36,
  ),
  zone_y = case_when( 
    zone %in% zones_y    ~  19/6,
    zone %in% (zones_y+3)  ~  15/6,
    zone %in% (zones_y+6)  ~  11/6,
    zone == 11           ~  23/6,                 
    zone == 12           ~  23/6, 
    zone == 13           ~  7/6,
    zone == 14           ~  7/6,
  )
  ) ->
  summarized_swings_L

db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("PO", "FO", "EP", "SC", "KN"),
         p_throws == "R",
         stand == "L",
         !is.na(zone)
  )%>%
  select(pitch_type, 
         description, 
         p_x,
         p_z, 
         zone
  )%>%
  collect()%>%
  group_by(pitch_type, zone)%>%
  ggplot() +
  #A heatmap of the pitch locations
  stat_density_2d( aes( x = p_x, 
                        y = p_z,
                        fill = ..density..), 
                   geom = "raster",
                   contour = FALSE,
                   show.legend = FALSE
  ) + 
  scale_fill_distiller(palette="OrRd", direction=1) +
  coord_cartesian(xlim = c(-2, 2), 
                  ylim = c(0.75,4)
  ) + 
  #This plots the percentages
  geom_text(data = summarized_swings_L, 
            aes(x=zone_x, 
                y=zone_y, 
                label = paste0(round(100*swing_prop), "%"),
                fontface = "bold")
  ) +
  geom_rect(data = test_zones,
            aes( xmin = x_lower,
                 xmax = x_upper, 
                 ymin = y_lower,
                 ymax = y_upper), 
            fill = NA, 
            color = "grey20",
            lwd = 1
  ) +
  geom_segment(aes(x = 0, y = 3.5 , xend =  0, yend = 3.5 + (2/3))) +
  geom_segment(aes(x = 0, xend = 0, y = 0.8333, yend = 1.5)) +
  geom_segment(aes(x = 17/24, xend = (17/24)+(17/36), y = 2.5, yend = 2.5)) +
  geom_segment(aes(x = (-17/24 - 17/36), xend = -17/24, y = 2.5, yend = 2.5)) +
  ggtitle("Swing Percentages per Pitch Type, by Zone, Left-Handed Hitters (Catcher's Perspective)") +
  theme(legend.position = "none") +
  theme_dark() + 
  facet_wrap(~pitch_type, nrow = 3)

rm(summarized_swings_L)

# Function that prints a decimal value without the leading 0
numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.2f", val)) }

#Summarized outcomes plots
#Righty batters
hit_types <- c("single", "double", "triple", "home_run") #List of batted ball types that are hits
weak_values <- c(1,2,3)
db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("FO", "PO", "KN", "SC", "EP"),
         p_throws == "R",
         stand == "R",
         !is.na(zone),
         description %in% swing_events
  )%>%
  select(pitch_type, 
         description,
         zone,
         launch_speed_angle)%>%
  collect()%>%
  group_by(pitch_type, zone)%>%
  summarise(whiff_prop = sum(description %in% c("swinging_strike_blocked",
                                                "swinging_strike")
  )/n(),
  weak_prop = sum(launch_speed_angle %in% weak_values)/n()
  )%>%
  mutate( zone_x = case_when(
    zone %in% zones_x      ~ -17/36,
    zone %in% (zones_x+1)  ~  0,
    zone %in% (zones_x+2)  ~  17/36,
    zone == 11           ~ -34/36,                 
    zone == 12           ~  34/36, 
    zone == 13           ~ -34/36,
    zone == 14           ~  34/36,
  ),
  zone_y = case_when( 
    zone %in% zones_y    ~  19/6,
    zone %in% (zones_y+3)  ~  15/6,
    zone %in% (zones_y+6)  ~  11/6,
    zone == 11           ~  23/6,                 
    zone == 12           ~  23/6, 
    zone == 13           ~  7/6,
    zone == 14           ~  7/6,
  )
  ) ->
  summarized_outcomes_R

db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("PO", "FO", "KN", "SC", "EP"),
         p_throws == "R",
         stand == "R",
         !is.na(zone),
         description %in% swing_events
  )%>%
  select(pitch_type, p_x, p_z, zone)%>%
  collect()%>%
  ggplot() +
  stat_density_2d( aes( x = p_x, 
                        y = p_z,
                        fill = ..density..), 
                   geom = "raster",
                   contour = FALSE,
                   show.legend = FALSE
  )+ 
  scale_fill_distiller(palette="OrRd", direction=1) +
  coord_cartesian(xlim = c(-1.9, 1.9), 
                  ylim = c(0.75,4)
  )+ 
  geom_text( data = summarized_outcomes_R,
             aes(x=zone_x, 
                 y=zone_y, 
                 label = paste0(numformat(weak_prop),"\n",
                                numformat(whiff_prop)),
                 fontface = "bold")
  )+
  geom_rect(data = test_zones,
            aes( xmin = x_lower,
                 xmax = x_upper, 
                 ymin = y_lower,
                 ymax = y_upper), 
            fill = NA, 
            color = "grey20",
            lwd = 1
  )+
  geom_segment(aes(x = 0, y = 3.5 , xend =  0, yend = 3.5 + (2/3))) +
  geom_segment(aes(x = 0, xend = 0, y = 0.8333, yend = 1.5)) +
  geom_segment(aes(x = 17/24, xend = (17/24)+(17/36), y = 2.5, yend = 2.5)) +
  geom_segment(aes(x = (-17/24 - 17/36), xend = -17/24, y = 2.5, yend = 2.5)) +
  ggtitle("Weak Contact/Whiff Proportions per Pitch Type, by Zone, Right-Handed Hitters (Catcher's Perspective)") +
  theme_dark() + 
  facet_wrap(~pitch_type, nrow = 3)
rm(summarized_outcomes_R)

#lefties
db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("FO", "PO", "KN", "SC", "EP"),
         p_throws == "R",
         stand == "L",
         !is.na(zone),
         description %in% swing_events
  )%>%
  select(pitch_type, 
         description,
         zone,
         launch_speed_angle)%>%
  collect()%>%
  group_by(pitch_type, zone)%>%
  summarise(whiff_prop = sum(description %in% c("swinging_strike_blocked",
                                                "swinging_strike")
  )/n(),
  weak_prop = sum(launch_speed_angle %in% weak_values)/n()
  )%>%
  mutate( zone_x = case_when(
    zone %in% zones_x      ~ -17/36,
    zone %in% (zones_x+1)  ~  0,
    zone %in% (zones_x+2)  ~  17/36,
    zone == 11           ~ -34/36,                 
    zone == 12           ~  34/36, 
    zone == 13           ~ -34/36,
    zone == 14           ~  34/36,
  ),
  zone_y = case_when( 
    zone %in% zones_y    ~  19/6,
    zone %in% (zones_y+3)  ~  15/6,
    zone %in% (zones_y+6)  ~  11/6,
    zone == 11           ~  23/6,                 
    zone == 12           ~  23/6, 
    zone == 13           ~  7/6,
    zone == 14           ~  7/6,
  )
  ) ->
  summarized_outcomes_L

db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("PO", "FO", "KN", "SC", "EP"),
         p_throws == "R",
         stand == "L",
         !is.na(zone),
         description %in% swing_events
  )%>%
  select(pitch_type, p_x, p_z, zone)%>%
  collect()%>%
  ggplot() +
  stat_density_2d( aes( x = p_x, 
                        y = p_z,
                        fill = ..density..), 
                   geom = "raster",
                   contour = FALSE,
                   show.legend = FALSE
  )+ 
  scale_fill_distiller(palette="OrRd", direction=1) +
  coord_cartesian(xlim = c(-1.9, 1.9), 
                  ylim = c(0.75,4)
  )+ 
  geom_text( data = summarized_outcomes_L,
             aes(x=zone_x, 
                 y=zone_y, 
                 label = paste0(numformat(weak_prop),"\n",
                                numformat(whiff_prop)),
                 fontface = "bold")
  )+
  geom_rect(data = test_zones,
            aes( xmin = x_lower,
                 xmax = x_upper, 
                 ymin = y_lower,
                 ymax = y_upper), 
            fill = NA, 
            color = "grey20",
            lwd = 1
  )+
  geom_segment(aes(x = 0, y = 3.5 , xend =  0, yend = 3.5 + (2/3))) +
  geom_segment(aes(x = 0, xend = 0, y = 0.8333, yend = 1.5)) +
  geom_segment(aes(x = 17/24, xend = (17/24)+(17/36), y = 2.5, yend = 2.5)) +
  geom_segment(aes(x = (-17/24 - 17/36), xend = -17/24, y = 2.5, yend = 2.5)) +
  ggtitle("Weak Contact/Whiff Proportions per Pitch Type, by Zone, Left-Handed Hitters (Catcher's Perspective)") +
  theme_dark() + 
  facet_wrap(~pitch_type, nrow = 3)
rm(summarized_outcomes_L)

#Whiff percentage by type with speed bins
order <- c("< 70", "70s", "80s", "90s", "100+")

db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("PO", "FO", "EP", "SC", "KN"),
         !is.na(release_speed),
         description %in% swing_events
  )%>%
  select(pitch_type, 
         description,
         release_speed
  )%>%
  collect()%>%
  mutate(speed_bins = case_when(
    release_speed < 70                        ~ "< 70",
    release_speed >= 70 & release_speed < 80  ~ "70s",
    release_speed >= 80 & release_speed < 90  ~ "80s",
    release_speed >= 90 & release_speed < 100 ~ "90s",
    release_speed >=100                       ~ "100+"
  ))%>%
  group_by(pitch_type, speed_bins)%>%
  #Calculate whiff percent for each pitch type, in each zone
  summarise(whiff_prop = 
              sum(description %in% c("swinging_strike_blocked",
                                     "swinging_strike")
              )/n(),
            total = n()
  )%>%
  ggplot(aes( x = factor(speed_bins, levels = order), y = whiff_prop)) +
  geom_bar(aes(fill = pitch_type), 
           stat = "identity", 
           show.legend = FALSE
  ) +
  # Report the total number of swing events in each category 
  # to help recognize cases with low observations that 
  # may be skewing the graphs
  geom_text(aes(label = total, y = -Inf),
            position = position_dodge(0.9),
            vjust = -1.1
  ) +
  ggtitle("Whiff Percentages per Pitch by Speed") +
  xlab("Speeds in MPH") +
  ylab("Whiff %") +
  theme(legend.position = "none") +
  theme_dark() + 
  facet_wrap(~pitch_type, nrow = 3, scales = "free")

#Whiff percent by type with spin bins
order <- c("< 1000", "< 1500", "< 2000", "< 2500", "< 3000", "3000+")

db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("PO", "FO", "EP", "SC", "KN"),
         !is.na(release_spin_rate),
         description %in% swing_events
  )%>%
  select(pitch_type, 
         description,
         release_spin_rate
  )%>%
  collect()%>%
  mutate(spin_bins = case_when(
    release_spin_rate < 1000                              ~"< 1000",
    release_spin_rate >= 1000 & release_spin_rate < 1500  ~"< 1500",
    release_spin_rate >= 1500 & release_spin_rate < 2000  ~"< 2000",
    release_spin_rate >= 2000 & release_spin_rate < 2500  ~"< 2500",
    release_spin_rate >= 2500 & release_spin_rate < 3000  ~"< 3000",
    release_spin_rate >= 3000                             ~"3000+"
  ))%>%
  group_by(pitch_type, spin_bins)%>%
  #Calculate whiff percent for each pitch type, in each zone
  summarise(whiff_prop = 
              sum(description %in% c("swinging_strike_blocked",
                                     "swinging_strike")
              )/n(),
            total = n()
  )%>%
  ggplot(aes( x = factor(spin_bins, levels = order), y = whiff_prop)) +
  geom_bar(aes(fill = pitch_type), 
           stat = "identity", 
           show.legend = FALSE
  ) +
  # Report the total number of swing events in each category 
  # to help recognize cases with low observations that 
  # may be skewing the graphs
  geom_text(aes(label = total, y = -Inf),
            position = position_dodge(0.9),
            vjust = -1.1
  ) +
  ggtitle("Whiff Percentages per Pitch by Spin Rate") +
  xlab("Spins in RPM") +
  ylab("Whiff %") +
  theme(legend.position = "none") +
  theme_dark() + 
  facet_wrap(~pitch_type, nrow = 3, scale = "free")

#Weak contact and speed
order <- c("< 70", "70s", "80s", "90s", "100+")
#A list of all description values which correspond to the batter taking a swing
db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("PO", "FO", "EP", "SC", "KN"),
         !is.na(release_speed),
         description %in% swing_events
  )%>%
  select(pitch_type, 
         description,
         release_speed,
         launch_speed_angle
  )%>%
  collect()%>%
  mutate(speed_bins = case_when(
    release_speed < 70                        ~ "< 70",
    release_speed >= 70 & release_speed < 80  ~ "70s",
    release_speed >= 80 & release_speed < 90  ~ "80s",
    release_speed >= 90 & release_speed < 100 ~ "90s",
    release_speed >=100                       ~ "100+"
  ))%>%
  group_by(pitch_type, speed_bins)%>%
  #Calculate whiff percent for each pitch type, in each zone
  summarise(weak_prop = sum(launch_speed_angle %in% weak_values)/n(),
            total = n()
  )%>%
  ggplot(aes( x = factor(speed_bins, levels = order), y = weak_prop)) +
  geom_bar(aes(fill = pitch_type), 
           stat = "identity", 
           show.legend = FALSE
  ) +
  # Report the total number of swing events in each category 
  # to help recognize cases with low observations that 
  # may be skewing the graphs
  geom_text(aes(label = total, y = -Inf),
            position = position_dodge(0.9),
            vjust = -1.1
  ) +
  ggtitle("Weak Contact Percentages per Pitch by Speed") +
  xlab("Speeds in MPH") +
  ylab("Weak Contact %") +
  theme(legend.position = "none") +
  theme_dark() + 
  facet_wrap(~pitch_type, nrow = 3, scales = "free")

#Weak contact and spin rate
order <- c("< 1000", "< 1500", "< 2000", "< 2500", "< 3000", "3000+")

db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("PO", "FO", "EP", "SC", "KN"),
         !is.na(release_spin_rate),
         description %in% swing_events
  )%>%
  select(pitch_type, 
         description,
         release_spin_rate,
         launch_speed_angle
  )%>%
  collect()%>%
  mutate(spin_bins = case_when(
    release_spin_rate < 1000                              ~"< 1000",
    release_spin_rate >= 1000 & release_spin_rate < 1500  ~"< 1500",
    release_spin_rate >= 1500 & release_spin_rate < 2000  ~"< 2000",
    release_spin_rate >= 2000 & release_spin_rate < 2500  ~"< 2500",
    release_spin_rate >= 2500 & release_spin_rate < 3000  ~"< 3000",
    release_spin_rate >= 3000                             ~"3000+"
  ))%>%
  group_by(pitch_type, spin_bins)%>%
  #Calculate whiff percent for each pitch type, in each zone
  summarise(weak_prop = sum(launch_speed_angle %in% weak_values)/n(),
            total = n()
  )%>%
  ggplot(aes( x = factor(spin_bins, levels = order), y = weak_prop)) +
  geom_bar(aes(fill = pitch_type), 
           stat = "identity", 
           show.legend = FALSE
  ) +
  # Report the total number of swing events in each category 
  # to help recognize cases with low observations that 
  # may be skewing the graphs
  geom_text(aes(label = total, y = -Inf),
            position = position_dodge(0.9),
            vjust = -1.1
  ) +
  ggtitle("Weak Contact per Pitch by Spin Rate") +
  xlab("Spins in RPM") +
  ylab("Weak Contact %") +
  theme(legend.position = "none") +
  theme_dark() + 
  facet_wrap(~pitch_type, nrow = 3, scale = "free")

#Full story by zone, righties
smacked <- c(4,5,6)
db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("FO", "PO", "KN", "SC", "EP"),
         p_throws == "R",
         stand == "R",
         !is.na(zone),
         description %in% swing_events
  )%>%
  select(pitch_type, 
         launch_speed,
         hit_distance_sc,
         launch_speed_angle,
         description,
         zone)%>%
  collect()%>%
  mutate(launch_speed_hits = case_when(launch_speed_angle %in% smacked ~ launch_speed,
                                       !launch_speed_angle %in% smacked ~ 0
  ),
  hit_distance_hits = case_when(launch_speed_angle %in% smacked ~ hit_distance_sc,
                                !launch_speed_angle %in% smacked ~ 0
  )
  )%>%
  group_by(pitch_type, zone)%>%
  summarise(smacked_prop = sum(launch_speed_angle %in% smacked)/n(),
            whiff_prop = sum(description %in% c("swinging_strike", "swinging_strike_blocked"))/n(),
            weak_prop = sum(launch_speed_angle %in% weak_values)/n()
            #avg_launch_speed = sum(launch_speed_hits)/sum(launch_speed_angle %in% smacked),
            #avg_hit_dist = sum(hit_distance_hits, na.rm = TRUE)/sum(launch_speed_angle %in% smacked)
  )%>%
  mutate( zone_x = case_when(
    zone %in% zones_x      ~ -17/36,
    zone %in% (zones_x+1)  ~  0,
    zone %in% (zones_x+2)  ~  17/36,
    zone == 11           ~ -34/36,                 
    zone == 12           ~  34/36, 
    zone == 13           ~ -34/36,
    zone == 14           ~  34/36,
  ),
  zone_y = case_when( 
    zone %in% zones_y    ~  19/6,
    zone %in% (zones_y+3)  ~  15/6,
    zone %in% (zones_y+6)  ~  11/6,
    zone == 11           ~  23/6,                 
    zone == 12           ~  23/6, 
    zone == 13           ~  7/6,
    zone == 14           ~  7/6,
  )
  ) ->
  summarized_R

db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("PO", "FO", "KN", "SC", "EP"),
         p_throws == "R",
         stand == "R",
         !is.na(zone),
         description %in% swing_events
  )%>%
  select(pitch_type, p_x, p_z, zone)%>%
  collect()%>%
  
  ggplot() +
  stat_density_2d(aes( x = p_x, 
                       y = p_z,
                       fill = ..density..), 
                  geom = "raster",
                  contour = FALSE,
                  show.legend = FALSE
  ) + 
  scale_fill_distiller(palette="OrRd", direction=1) +
  coord_cartesian(xlim = c(-1.8, 1.8), ylim = c(0.75,4)) + 
  geom_text(data = summarized_R,
            aes(x=zone_x, 
                y=zone_y, 
                label = paste0(numformat(whiff_prop + weak_prop), "\n",
                               numformat(smacked_prop))),
            fontface = "bold"
  ) +
  geom_rect(data = test_zones,
            aes( xmin = x_lower,
                 xmax = x_upper, 
                 ymin = y_lower,
                 ymax = y_upper), 
            fill = NA, 
            color = "grey20",
            lwd = 1
  ) +
  geom_segment(aes(x = 0, y = 3.5 , xend =  0, yend = 3.5 + (2/3))) +
  geom_segment(aes(x = 0, xend = 0, y = 0.8333, yend = 1.5)) +
  geom_segment(aes(x = 17/24, xend = (17/24)+(17/36), y = 2.5, yend = 2.5)) +
  geom_segment(aes(x = (-17/24 - 17/36), xend = -17/24, y = 2.5, yend = 2.5)) +
  ggtitle("Good/Bad Outcome Percentages by Zone, by Pitch Type. 
                     Righty Batters (Catcher's Perspective)") +
  theme(legend.position = "none") +
  theme_dark() + 
  facet_wrap(~pitch_type, nrow = 3)

rm(summarized_R)

#Full story by zone, lefties
db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("FO", "PO", "KN", "SC", "EP"),
         p_throws == "R",
         stand == "L",
         !is.na(zone),
         description %in% swing_events
  )%>%
  select(pitch_type, 
         launch_speed,
         hit_distance_sc,
         launch_speed_angle,
         description,
         zone)%>%
  collect()%>%
  mutate(launch_speed_hits = case_when(launch_speed_angle %in% smacked ~ launch_speed,
                                       !launch_speed_angle %in% smacked ~ 0
  ),
  hit_distance_hits = case_when(launch_speed_angle %in% smacked ~ hit_distance_sc,
                                !launch_speed_angle %in% smacked ~ 0
  )
  )%>%
  group_by(pitch_type, zone)%>%
  summarise(smacked_prop = sum(launch_speed_angle %in% smacked)/n(),
            whiff_prop = sum(description %in% c("swinging_strike", "swinging_strike_blocked"))/n(),
            weak_prop = sum(launch_speed_angle %in% weak_values)/n()
            #avg_launch_speed = sum(launch_speed_hits)/sum(launch_speed_angle %in% smacked),
            #avg_hit_dist = sum(hit_distance_hits, na.rm = TRUE)/sum(launch_speed_angle %in% smacked)
  )%>%
  mutate( zone_x = case_when(
    zone %in% zones_x      ~ -17/36,
    zone %in% (zones_x+1)  ~  0,
    zone %in% (zones_x+2)  ~  17/36,
    zone == 11           ~ -34/36,                 
    zone == 12           ~  34/36, 
    zone == 13           ~ -34/36,
    zone == 14           ~  34/36,
  ),
  zone_y = case_when( 
    zone %in% zones_y    ~  19/6,
    zone %in% (zones_y+3)  ~  15/6,
    zone %in% (zones_y+6)  ~  11/6,
    zone == 11           ~  23/6,                 
    zone == 12           ~  23/6, 
    zone == 13           ~  7/6,
    zone == 14           ~  7/6,
  )
  ) ->
  summarized_L

db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("PO", "FO", "KN", "SC", "EP"),
         p_throws == "R",
         stand == "L",
         !is.na(zone),
         description %in% swing_events
  )%>%
  select(pitch_type, p_x, p_z, zone)%>%
  collect()%>%
  
  ggplot() +
  stat_density_2d(aes( x = p_x, 
                       y = p_z,
                       fill = ..density..), 
                  geom = "raster",
                  contour = FALSE,
                  show.legend = FALSE
  ) + 
  scale_fill_distiller(palette="OrRd", direction=1) +
  coord_cartesian(xlim = c(-1.8, 1.8), ylim = c(0.75,4)) + 
  geom_text(data = summarized_L,
            aes(x=zone_x, 
                y=zone_y, 
                label = paste0(numformat(whiff_prop + weak_prop), "\n",
                               numformat(smacked_prop))),
            fontface = "bold"
  ) +
  geom_rect(data = test_zones,
            aes( xmin = x_lower,
                 xmax = x_upper, 
                 ymin = y_lower,
                 ymax = y_upper), 
            fill = NA, 
            color = "grey20",
            lwd = 1
  ) +
  geom_segment(aes(x = 0, y = 3.5 , xend =  0, yend = 3.5 + (2/3))) +
  geom_segment(aes(x = 0, xend = 0, y = 0.8333, yend = 1.5)) +
  geom_segment(aes(x = 17/24, xend = (17/24)+(17/36), y = 2.5, yend = 2.5)) +
  geom_segment(aes(x = (-17/24 - 17/36), xend = -17/24, y = 2.5, yend = 2.5)) +
  ggtitle("Good/Bad Outcome Percentages by Zone, by Pitch Type. 
                     Lefty Batters (Catcher's Perspective)") +
  theme(legend.position = "none") +
  theme_dark() + 
  facet_wrap(~pitch_type, nrow = 3)

rm(summarized_L)

#Returns spray angle in radians
getSprayAngle <- function(hc_x, hc_y){
  #Adjust params first, for purpose of readability 
  x = hc_x-125.42
  y = 198.27-hc_y
  return(atan(x/y))
}


#loon plot of hard hit balls
```{r loon plot of hits, echo = FALSE}
#I use the arccast table here because it contains the batter names
#It was the original table I was using for this analysis but
#I ended up changing some things. Getting the batter names takes
#over four hours using a baseballr function, so I did not repeat the procedure.
db%>%
  tbl("arccast")%>%
  filter(!is.na(hit_distance_sc),
         !is.na(hc_x),
         !pitch_type %in% c("PO", "FO", "EP", "KN", "SC"),
         p_throws == "R",
         stand =="R",
         launch_angle_speed %in% smacked
  )%>%
  select(pitch_type, 
         description,
         pitcher_name,
         batter_name,
         des,
         events,
         bb_type,
         hc_x,
         hc_y,
         hit_distance_sc,
         launch_speed,
         launch_angle,
         p_x,
         p_z)%>%
  collect() ->
  lefty_hit_tbl

#Labels for the plot
lefty_hit_tbl%$%
  paste0( "Batter Name:  ", batter_name, "\n",
          "Pitcher Name:  ", pitcher_name, "\n",
          "Pitch Type:  ", pitch_type, "\n",
          "Hit Distance:  ", hit_distance_sc, "\n",
          "Launch Angle:  ", launch_angle, "\n") ->
  hit_labels


#Strike zone hits plot
lefty_hit_tbl%>%
  gg_pipe(
    ggplot(aes(x= p_x, y = p_z, group = pitch_type)) + 
      #theme_dark() +
      geom_point(aes(color = pitch_type), shape = 19, alpha = 0.4) +
      geom_rect(aes(xmin = -17/24,
                    xmax =  17/24,
                    ymin =  1.5,
                    ymax =  3.5), 
                fill = NA, color = "black", size = 1) +
      facet_wrap(~pitch_type, nrow = 3)
  )%>%
  loon.ggplot(linkingGroup = "hits") ->
  l_zone_plot

#Field hits plot
lefty_hit_tbl%>%
  mutate(spray_angle = getSprayAngle(hc_x, hc_y),  #custom functions used here
         travel_x = hit_distance_sc*sin(spray_angle),
         travel_y = hit_distance_sc*cos(spray_angle))%>%
  gg_pipe(
    ggplot(aes(x = travel_x, y = travel_y, group = pitch_type, color = pitch_type)) +
      #annotation_custom(bb_field,xmin=-250, xmax=255, ymin=-70, ymax=460) +
      geom_abline(slope = 1, intercept = 0)+
      geom_abline(slope = -1, intercept = 0) +
      geom_point(pch =1)
  )%>%
  loon.ggplot(linkingGroup = "hits") ->
  l_hits_plot

plots <- list(zone = l_zone_plot, field = l_hits_plot)
for(plot in plots){
  if(is(plot, "l_plot")){
    plot["itemLabel"] <- hit_labels
    plot["showItemLabels"] <- TRUE
  }
}

#Proportion of use by count, righty bats
db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("PO", "SC", "EP", "FO", "KN"),
         p_throws == "R",
         stand =="R"
  )%>%
  select(pitch_type, balls, strikes)%>%
  collect()%>%
  mutate(
    style = case_when(pitch_type %in% c("FF", "FC")        ~ "Speed Up",
                      pitch_type %in% c("FT", "SI")        ~ "Weak Contact",
                      pitch_type %in% c("KC", "CU", "SL")  ~ "Away Whiff",
                      pitch_type %in% c("CH", "FS")        ~ "Whiff Low"
    )
  )%>%
  ggplot(., aes(x= style, group = balls)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..) ), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  #coord_cartesian( ylim = c(-Inf,0.8))+ 
  ggtitle("Pitch Use Percentages, by Count. Righty Batters") +
  labs(y = "Use Percentage") +
  guides(fill = FALSE) +
  theme_dark() +
  facet_grid(strikes~balls) +
  scale_y_continuous(labels = scales::percent, 
                     limits = c(NA, 0.75))


#Proportion of use my count, lefty bats
db%>%
  tbl("calccast")%>%
  filter(!pitch_type %in% c("PO", "SC", "EP", "FO", "KN"),
         p_throws == "R",
         stand =="L"
  )%>%
  select(pitch_type, balls, strikes)%>%
  collect()%>%
  mutate(
    style = case_when(pitch_type %in% c("FF", "FC")         ~ "Speed Up",
                      pitch_type %in% c("FT", "SI")         ~ "Weak Contact",
                      pitch_type %in% c("KC", "CU", "SL")   ~ "Inside Whiff",
                      pitch_type %in% c("CH", "FS")         ~ "Whiff Low"
    )
  )%>%
  ggplot(., aes(x= style, group = balls)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..) ), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  ggtitle("Pitch Use Percentages, by Count. Lefty Batters") +
  labs(y = "Use Percentage") +
  guides(fill = FALSE) +
  theme_dark() +
  facet_grid(strikes~balls) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(NA, 0.75))





