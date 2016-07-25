###Plot.ly- Data Visualization Techniques#####

library(plotly)
set.seed(100)
#scatterplot using plotly
plot_ly(Cars93, x = Length, y = Width, text = paste("Type: ", Type),
        mode = "markers", color = Length, size = Length)


#GGPLOTLY: ggplot plus plotly
p <- ggplot(data = Cars93, aes(x = Horsepower, y = Price)) +
  geom_point(aes(text = paste("Type:", Type)), size = 2, color="darkorchid4") +
  geom_smooth(aes(colour = Origin, fill = Origin)) + facet_wrap(~ Origin)

(gg <- ggplotly(p))


#MIX DATA MANIPULATION AND VISUALIZATION VERBS
str(p <- plot_ly(economics, x = date, y = uempmed))

library(dplyr)
p %>%
  add_trace(y = fitted(loess(uempmed ~ as.numeric(date)))) %>%
  layout(title = "Median duration of unemployment (in weeks)",
         showlegend = FALSE) %>%
  dplyr::filter(uempmed == max(uempmed)) %>%
  layout(annotations = list(x = date, y = uempmed, text = "Peak", showarrow = T))

#3D WEBGL AND MORE
plot_ly(z = volcano, type = "surface")

#RUN LOCALLY OR PUBLISH TO THE WEB
library(plotly)
(p <- plot_ly(midwest, x = percollege, color = state, type = "box"))

# plotly_POST publishes the figure to your plotly account on the web
# account needed
plotly_POST(p, filename = "r-docs/midwest-boxplots", world_readable=TRUE)


###############################
###Bar Charts
###############################
library(plotly)
attach(Cars93)
p <- plot_ly(
  x = Type,
  y = Price,
  name = "Price by Type",
  type = "bar")
p

p2 <- add_trace(
  p,
  x = c("giraffes", "orangutans", "monkeys"),
  y = c(12, 18, 29),
  name = "LA Zoo",
  filename="r-docs/simple-bars"
)
p2

layout(p2, barmode = "stack")

## customizing colors
library(dplyr)
ggplot2::diamonds %>% count(cut) %>%
  plot_ly(x = cut, y = n, type = "bar", marker = list(color = toRGB("skyblue")))

# mapping a color variable
ggplot2::diamonds %>% count(cut, clarity) %>%
  plot_ly(x = cut, y = n, type = "bar", color = clarity)

#############################################
#Line and Scatter Plots in R
#############################################
# Simple scatterplot
library(plotly)
plot_ly(data = Cars93, x = Horsepower, y = MPG.highway, mode = "markers")

#Scatter Plot with Qualitative Colorscale
plot_ly(data = Cars93, x = Horsepower, y = MPG.city, mode = "markers",
        color = Type)

#ColorBrewer Palette Names
# By default, colors will 'span the gamut'
# scales::show_col(RColorBrewer::brewer.pal("Set1"))
plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, mode = "markers",
        color = Species, colors = "Set1")

#Custom Color Scales
# pass RGB or hex color codes directly to colors for finer control
pal <- RColorBrewer::brewer.pal(nlevels(iris$Species), "Set1")
plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, color = Species,
        colors = pal, mode = "markers")

#Adding Color and Size Mapping
library(plotly)
d <- diamonds[sample(nrow(diamonds), 1000), ]
# note how size is automatically scaled and added as hover text
plot_ly(d, x = carat, y = price, size = carat, mode = "markers")

plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
        mode = "markers", color = carat, size = carat, opacity = carat)

#Basic Time Series Plot with Loess Smooth
p <- plot_ly(economics, x = date, y = uempmed, name = "unemployment")
p %>% add_trace(y = fitted(loess(uempmed ~ as.numeric(date))))

#Density Plot
dens <- with(diamonds, tapply(price, INDEX = cut, density))
df <- data.frame(
  x = unlist(lapply(dens, "[[", "x")),
  y = unlist(lapply(dens, "[[", "y")),
  cut = rep(names(dens), each = length(dens[[1]]$x))
)
plot_ly(df, x = x, y = y, color = cut)

#Line Interpolation Options
x <- 1:5
y <- c(1, 3, 2, 3, 1)
plot_ly(x = x, y = y, name = "linear", line = list(shape = "linear")) %>%
  add_trace(y = y + 5, name = "spline", line = list(shape = "spline")) %>%
  add_trace(y = y + 10, name = "vhv", line = list(shape = "vhv")) %>%
  add_trace(y = y + 15, name = "hvh", line = list(shape = "hvh")) %>%
  add_trace(y = y + 20, name = "vh", line = list(shape = "vh")) %>%
  add_trace(y = y + 25, name = "hv", line = list(shape = "hv"))

#Filled Area Plots in R
library(plotly)
p <- plot_ly(x = c(1, 2, 3, 4), y = c(0, 2, 3, 5), fill = "tozeroy")
add_trace(p, x = c(1, 2, 3, 4), y = c(3, 5, 1, 7), fill = "tonexty")

#Histograms in R
### Basic histogram
library(plotly)
plot_ly(x = rnorm(50), type = "histogram")

### Overlaid histograms
plot_ly(x = rnorm(500), opacity = 0.6, type = "histogram") %>%
  add_trace(x = rnorm(500)+1) %>%
  layout(barmode="overlay")

#Box Plots
library(plotly)
### basic boxplot
plot_ly(y = MPG.highway, type = "box") %>%
  add_trace(y = MPG.highway)

### adding jittered points
plot_ly(y = MPG.highway, type = "box", boxpoints = "all", jitter = 0.3,
        pointpos = -1.8)

### several box plots
plot_ly(Cars93, y = MPG.highway, color = Type, type = "box")

### grouped box plots
plot_ly(Cars93, x = Type, y = MPG.city, color = AirBags, type = "box") %>%
  layout(boxmode = "group")

#2D Histogram in R
library(plotly)
s <- matrix(c(1, -.75, -.75, 1), ncol = 2)
obs <- mvtnorm::rmvnorm(500, sigma = s)
plot_ly(x = obs[,1], y = obs[,2], type = "histogram2d")

#Contour Plots
### Basic contour
library(plotly)
plot_ly(z = volcano, type = "contour")

#2D Density Contour Plot
x <- rnorm(200)
y <- rnorm(200)
p1 <- plot_ly(x = x, type = "histogram")
p2 <- plot_ly(x = x, y = y, type = "histogram2dcontour")
p3 <- plot_ly(y = y, type = "histogram")
a1 <- list(domain = c(0, .85))
a2 <- list(domain = c(.85, 1))
subplot(
  layout(p1, xaxis = a1, yaxis = a2),
  layout(p2, xaxis = a1, yaxis = a1),
  layout(p3, xaxis = a2, yaxis = a1)
)

#Dumbbell plots in R
library(tidyr)
library(plotly)
s <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")
s <- s[order(s$Men), ]
gather(s, Sex, value, Women, Men) %>%
  plot_ly(x = value, y = School, mode = "markers",
          color = Sex, colors = c("pink", "blue")) %>%
  add_trace(x = value, y = School, mode = "lines", 
            group = School, showlegend = F, line = list(color = "gray")) %>%
  layout(
    title = "Gender earnings disparity",
    xaxis = list(title = "Annual Salary (in thousands)"),
    margin = list(l = 65)
  )

#Dot plots in R
s <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")
s <- s[order(s$Men), ]
library(plotly)
p <- plot_ly(s, x = Women, y = School, name = "Women",
             mode = "markers", marker = list(color = "pink")) %>%
  add_trace(x = Men, name = "Men", marker = list(color = "blue")) %>%
  layout(
    title = "Gender earnings disparity",
    xaxis = list(title = "Annual Salary (in thousands)"),
    margin = list(l = 65)
  )
p

#Heatmaps in R
library(plotly)
plot_ly(z = volcano, type = "heatmap")

#Categorical Axes
m <- matrix(rnorm(9), nrow = 3, ncol = 3)
plot_ly(z = m,
        x = c("a", "b", "c"), y = c("d", "e", "f"),
        type = "heatmap")

#Sequential Colorscales: Hot
plot_ly(z = volcano, colorscale = "Hot", type = "heatmap")

#Sequential Colorscales: Greys
plot_ly(z = volcano, colorscale = "Greys", type = "heatmap")


#Sequential Colorscale: Greens
plot_ly(z = volcano, colorscale = "Greens", type = "heatmap")

#Custom colorscales
vals <- unique(scales::rescale(c(volcano)))
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
plot_ly(z = volcano, colorscale = colz, type = "heatmap")

library(viridis)
plot_ly(z = volcano, colors = viridis(256), type = "heatmap")


#Graphing Multiple Chart Types in R
### Scatterplot with loess smoother

library(plotly)
mtcars <- mtcars[order(mtcars$disp), ]
p <- plot_ly(mtcars, x = disp, y = mpg, mode = "markers",
             text = rownames(mtcars), showlegend = FALSE)
add_trace(p, y = fitted(loess(mpg ~ disp)), mode = "lines",
          name = "loess smoother", showlegend = TRUE)

### Scatterplot with loess smoother and it's uncertaincy estimates
m <- loess(mpg ~ disp, data = mtcars)
f <- with(predict(m, se = TRUE), data.frame(fit, se.fit))

l <- list(
  color = toRGB("gray90", alpha = 0.3),
  fillcolor = toRGB("gray90", alpha = 0.3)
)

p %>%
  add_trace(p, data = f, y = fit, mode = "lines") %>%
  add_trace(p, data = f, y = fit + 1.96 * se.fit, mode = "lines",
            fill = "tonexty", line = l) %>%
  add_trace(p, data = f, y = fit - 1.96 * se.fit, mode = "lines",
            fill = "tonexty", line = l)

#Rectangles and Shapes
library(plotly)
p <- plot_ly(economics, x = date, y = uempmed, name = "unemployment")
p
# add shapes to the layout
p <- layout(p, title = 'Highlighting with Rectangles', 
            shapes = list(
              list(type = "rect", 
                   fillcolor = "blue", line = list(color = "blue"), opacity = 0.3, 
                   x0 = "1980-01-01", x1 = "1985-01-01", xref = "x", 
                   y0 = 4, y1 = 12.5, yref = "y"), 
              list(type = "rect",
                   fillcolor = "blue", line = list(color = "blue"), opacity = 0.2, 
                   x0 = "2000-01-01", x1 = "2005-01-01", xref = "x", 
                   y0 = 4, y1 = 12.5, yref = "y"))) 
p

#Circles
library(plotly)
d <- diamonds[sample(nrow(diamonds), 1000), ]
d <- plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
             mode = "markers", color = carat, size = carat, opacity = carat)
d <- layout(d, title = 'Highlighting Regions with Circles', 
            shapes = list(
              list(type = 'circle',
                   xref = 'x', x0 = .2, x1 = .7,
                   yref = 'y', y0 = 20, y1 = 3000,
                   fillcolor = 'rgb(50, 20, 90)', line = list(color = 'rgb(50, 20, 90)'),
                   opacity = 0.2),
              list(type = 'circle',
                   xref = 'x', x0 = .75, x1 = 1.5,
                   yref = 'y', y0 = 2500, y1 = 7500,
                   fillcolor = 'rgb(30, 100, 120)', line = list(color = 'rgb(30, 100, 120)'),
                   opacity = 0.2),
              list(type = 'circle',
                   xref = 'x', x0 = 1.6, x1 = 2.5,
                   yref = 'y', y0 = 12500, y1 = 18500,
                   fillcolor = 'rgb(90, 200, 75)', line = list(color = 'rgb(90, 200, 75)'),
                   opacity = 0.2)))
d

#Error Bars
library(dplyr)
library(plotly)

p <- ggplot2::mpg %>% group_by(class) %>%
  summarise(mn = mean(hwy), sd = 1.96 * sd(hwy)) %>%
  arrange(desc(mn)) %>%
  plot_ly(x = class, y = mn, error_y = list(value = sd),
          mode = "markers", name = "Highway") %>%
  layout(yaxis = list(title = "Miles Per Gallon"))
p

df2 <- mpg %>% group_by(class) %>%
  summarise(mn = mean(cty), sd = 1.96 * sd(cty))

p2 <- add_trace(p, y = mn, error_y = list(value = sd),
                name = "City", data = df2)
p2

#Polar Charts in R
library(plotly)
pc <- plot_ly(Cars93, r = Price, t = RPM, color = AirBags, mode = "lines")
layout(pc, title = "Cars Price by RPM", orientation = -90)

#Polar Scatter Chart
pc <- plot_ly(Cars93, r = Price, t = Horsepower, color = Type, 
             opacity = 0.8, mode = "markers")
layout(pc, title = "Price of Cars by Horsepower", 
       plot_bgcolor = toRGB("grey90"))

#Polar Area Chart
pc <- plot_ly(Cars93, r = Price, t = Horsepower, color = Type, 
             type = "area")
layout(pc, radialaxis = list(ticksuffix = "%"), orientation = 270)

#3D Line Plots in R
# initiate a 100 x 3 matrix filled with zeros
m <- matrix(numeric(300), ncol = 3)
# simulate a 3D random-walk
for (i in 2:100) m[i, ] <- m[i-1, ] + rnorm(3)
# collect everything in a data-frame
df <- setNames(
  data.frame(m, seq(1, 100)),
  c("x", "y", "z", "time")
)

library(plotly)
plot_ly(df, x = x, y = y, z = z, color = time, type = "scatter3d")

#3D Scatter Plots in R
# variance-covariance matrix for a multivariate normal distribution
s <- matrix(c(1, .5, .5,
              .5, 1, .5,
              .5, .5, 1), ncol = 3)
# use the mvtnorm package to sample 200 observations
obs <- mvtnorm::rmvnorm(200, sigma = s)
# collect everything in a data-frame
df <- setNames(data.frame(obs), c("x", "y", "z"))

library(plotly)
plot_ly(Cars93, x = Length, y = Width, z = Wheelbase, 
        type = "scatter3d", mode = "markers")

#3D Scatter Plot with Hover Text
set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]
plot_ly(d, x = carat, y = price, z=depth, text = paste("Clarity: ", clarity),
        type="scatter3d", mode="markers")

#3D Surface Plots in R
library(plotly)
# volcano is a numeric matrix that ships with R
plot_ly(z = volcano, type = "surface")

#2D Kernel Density Estimation
kd <- with(MASS::geyser, MASS::kde2d(duration, waiting, n = 50))
with(kd, plot_ly(x = x, y = y, z = z, type = "surface"))


#United States Bubble Map
library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')
df$hover <- paste(df$name, "Population", df$pop/1e6, " million")

df$q <- with(df, cut(pop, quantile(pop)))
levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
df$q <- as.ordered(df$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

plot_ly(df, lon = lon, lat = lat, text = hover,
        marker = list(size = sqrt(pop/10000) + 1),
        color = q, type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)

#Choropleth Maps in R
library(plotly)
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

plot_ly(df, z = total.exports, text = hover, locations = code, type = 'choropleth',
        locationmode = 'USA-states', color = total.exports, colors = 'Purples',
        marker = list(line = l), colorbar = list(title = "Millions USD")) %>%
  layout(title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)', geo = g)

#World Choropleth Map
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

plot_ly(df, z = GDP..BILLIONS., text = COUNTRY, locations = CODE, type = 'choropleth',
        color = GDP..BILLIONS., colors = 'Blues', marker = list(line = l),
        colorbar = list(tickprefix = '$', title = 'GDP Billions US$')) %>%
  layout(title = '2014 Global GDP<br>Source:<a href="https://www.cia.gov/library/publications/the-world-factbook/fields/2195.html">CIA World Factbook</a>',
         geo = g)
###########################################################
#Choropleth Inset Map
###########################################################
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_ebola.csv')
# restrict from June to September
df <- subset(df, Month %in% 6:9)
# ordered factor variable with month abbreviations
df$abbrev <- ordered(month.abb[df$Month], levels = month.abb[6:9])
# September totals
df9 <- subset(df, Month == 9)

# common plot options
g <- list(
  scope = 'africa',
  showframe = F,
  showland = T,
  landcolor = toRGB("grey90")
)

g1 <- c(
  g,
  resolution = 50,
  showcoastlines = T,
  countrycolor = toRGB("white"),
  coastlinecolor = toRGB("white"),
  projection = list(type = 'Mercator'),
  list(lonaxis = list(range = c(-15, -5))),
  list(lataxis = list(range = c(0, 12))),
  list(domain = list(x = c(0, 1), y = c(0, 1)))
)

g2 <- c(
  g,
  showcountries = F,
  bgcolor = toRGB("white", alpha = 0),
  list(domain = list(x = c(0, .6), y = c(0, .6)))
)

plot_ly(df, type = 'scattergeo', mode = 'markers', locations = Country,
        locationmode = 'country names', text = paste(Value, "cases"),
        color = as.ordered(abbrev), marker = list(size = Value/50), inherit = F) %>%
  add_trace(type = 'scattergeo', mode = 'text', geo = 'geo2', showlegend = F,
            lon = 21.0936, lat = 7.1881, text = 'Africa') %>%
  add_trace(type = 'choropleth', locations = Country, locationmode = 'country names',
            z = Month, colors = "black", showscale = F, geo = 'geo2', data = df9) %>%
  layout(title = 'Ebola cases reported by month in West Africa 2014<br> Source: <a href="https://data.hdx.rwlabs.org/dataset/rowca-ebola-cases">HDX</a>',
         geo = g1, geo2 = g2)


#######################################################
###US Airports Map in R
########################
library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')
df$hover <- with(df, paste(airport, city, state, "Arrivals: ", cnt))

# marker styling
m <- list(
  colorbar = list(title = "Incoming flights February 2011"),
  size = 8, opacity = 0.8, symbol = 'square'
)

# geo styling
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)
plot_ly(df, lat = lat, lon = long, text = hover, color = cnt,
        type = 'scattergeo', locationmode = 'USA-states', mode = 'markers',
        marker = m) %>%
  layout(title = 'Most trafficked US airports<br>(Hover for airport)', geo = g)

#North American Precipitation Map from NOAA
library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2015_06_30_precipitation.csv')
df$hover <- paste(df$Globvalue, "inches")

# change default color scale title
m <- list(colorbar = list(title = "Total Inches"))

# geo styling
g <- list(
  scope = 'north america',
  showland = TRUE,
  landcolor = toRGB("grey83"),
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white"),
  showlakes = TRUE,
  lakecolor = toRGB("white"),
  showsubunits = TRUE,
  showcountries = TRUE,
  resolution = 50,
  projection = list(
    type = 'conic conformal',
    rotation = list(
      lon = -100
    )
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(-140, -55),
    dtick = 5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(20, 60),
    dtick = 5
  )
)

plot_ly(df, lat = Lat, lon = Lon, text = hover, color = Globvalue,
        type = 'scattergeo', marker = m) %>%
  layout(title = 'US Precipitation 06-30-2015<br>Source: NOAA', geo = g)

##Map Subplots and Small Multiples
# US map small multiples
library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/1962_2006_walmart_store_openings.csv')

# common map properties
g <- list(scope = 'usa', showland = T, landcolor = toRGB("gray90"), showcountries = F, subunitcolor = toRGB("white"))

# year text labels
yrs <- unique(df$YEAR)
id <- seq_along(yrs)
df2 <- data.frame(
  YEAR = yrs,
  id = id
)

# id for anchoring traces on different plots
df$id <- as.integer(factor(df$YEAR))

p <- plot_ly(df, type = 'scattergeo', lon = LON, lat = LAT, group = YEAR,
             geo = paste0("geo", id), showlegend = F,
             marker = list(color = toRGB("blue"), opacity = 0.5)) %>%
  add_trace(lon = -78, lat = 47, mode = 'text', group = YEAR,
            geo = paste0("geo", id), text = YEAR, data = df2) %>%
  layout(title = 'New Walmart Stores per year 1962-2006<br> Source: <a href="http://www.econ.umn.edu/~holmes/data/WalMart/index.html">University of Minnesota</a>',
         geo = g,
         autosize = F,
         width = 1000,
         height = 900,
         hovermode = F)

subplot(p, nrows = 9)

#USA Flight Paths Map
library(plotly)
# airport locations
air <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')
# flights between airports
flights <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_aa_flight_paths.csv')
flights$id <- seq_len(nrow(flights))

# map projection
geo <- list(
  scope = 'north america',
  projection = list(type = 'azimuthal equal area'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

plot_ly(air, lon = long, lat = lat, text = airport, type = 'scattergeo',
        locationmode = 'USA-states', marker = list(size = 2, color = 'red'),
        inherit = FALSE) %>%
  add_trace(lon = list(start_lon, end_lon), lat = list(start_lat, end_lat),
            group = id, opacity = cnt/max(cnt), data = flights,
            mode = 'lines', line = list(width = 1, color = 'red'),
            type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(title = 'Feb. 2011 American Airline flight paths<br>(Hover for airport names)',
         geo = geo, showlegend = FALSE, height=800)

#London to NYC Great Circle
library(plotly)
plot_ly(lat = c(40.7127, 51.5072), lon = c(-74.0059, 0.1275), type = 'scattergeo',
        mode = 'lines', line = list(width = 2, color = 'blue')) %>%
  layout(
    title = 'London to NYC Great Circle',
    showlegend = FALSE,
    geo = list(
      resolution = 50,
      showland = TRUE,
      showlakes = TRUE,
      landcolor = toRGB("grey80"),
      countrycolor = toRGB("grey80"),
      lakecolor = toRGB("white"),
      projection = list(type = "equirectangular"),
      coastlinewidth = 2,
      lataxis = list(
        range = c(20, 60),
        showgrid = TRUE,
        tickmode = "linear",
        dtick = 10
      ),
      lonaxis = list(
        range = c(-100, 20),
        showgrid = TRUE,
        tickmode = "linear",
        dtick = 20
      )
    )
  )

##Contour lines on globe
library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/globe_contours.csv')
df$id <- seq_len(nrow(df))

library(tidyr)
d <- df %>%
  gather(key, value, -id) %>%
  separate(key, c("l", "line"), "\\.") %>%
  spread(l, value)

p <- plot_ly(type = 'scattergeo', mode = 'lines',
             line = list(width = 2, color = 'violet'))

for (i in unique(d$line))
  p <- add_trace(p, lat = lat, lon = lon, data = subset(d, line == i))

geo <- list(
  showland = TRUE,
  showlakes = TRUE,
  showcountries = TRUE,
  showocean = TRUE,
  countrywidth = 0.5,
  landcolor = toRGB("grey90"),
  lakecolor = toRGB("white"),
  oceancolor = toRGB("white"),
  projection = list(
    type = 'orthographic',
    rotation = list(
      lon = -100,
      lat = 40,
      roll = 0
    )
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  )
)

layout(p, showlegend = FALSE, geo = geo,
       title = 'Contour lines over globe<br>(Click and drag to rotate)')

########################
#Multiple Axes
########################
library(plotly)
ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right"
)
plot_ly(x = 1:3, y = 10*(1:3), name = "slope of 10") %>%
  add_trace(x = 2:4, y = 1:3, name = "slope of 1", yaxis = "y2") %>%
  layout(title = "Double Y Axis", yaxis2 = ay)

#Insets
library(plotly)
p1 <- plot_ly(x = c(1, 2, 3), y = c(4, 3, 2))
p2 <- plot_ly(x = c(20, 30, 40), y = c(30, 40, 50)) %>%
  layout(xaxis = list(domain = c(0.6, 0.95)),
         yaxis = list(domain = c(0.6, 0.95)))
subplot(p1, p2)

#Basic Subplot
library(plotly)
subplot(
  plot_ly(economics, x = date, y = uempmed),
  plot_ly(economics, x = date, y = unemploy),
  margin = 0.05
) %>% layout(showlegend = FALSE)

#Subplot Rows:Specify the number of rows with nrows.
# Basic subplot
library(plotly)
subplot(
  plot_ly(economics, x = date, y = uempmed),
  plot_ly(economics, x = date, y = unemploy),
  margin = 0.05,
  nrows=2
) %>% layout(showlegend = FALSE)

#Creating Subplots with group
plot_ly(iris, x = Petal.Length, y = Petal.Width,
        group = Species, mode = "markers")

iris$id <- as.integer(iris$Species)
p <- plot_ly(iris, x = Petal.Length, y = Petal.Width, group = Species,
             xaxis = paste0("x", id), mode = "markers")
subplot(p)

#Coordinating Color Across Subplots
subplot(
  plot_ly(iris, x = Sepal.Length, y = Sepal.Width, color = Species, mode = "markers"),
  plot_ly(iris, x = Petal.Length, y = Petal.Width, color = Species, mode = "markers"),
  margin = 0.05
)

#Customizing Axes
p <- plot_ly(iris, x = Petal.Length, y = Petal.Width, group = Species,
             xaxis = paste0("x", id), mode = "markers")

layout(
  subplot(p),
  xaxis = list(title = "x axis 1", showgrid=FALSE),
  xaxis2 = list(title = "x axis 2", showgrid=FALSE),
  xaxis3 = list(title = "x axis 3", showgrid=FALSE),
  
  yaxis = list(title = "y axis 1", showgrid=FALSE),
  yaxis2 = list(title = "y axis 2", showgrid=FALSE),
  yaxis3 = list(title = "y axis 3", showgrid=FALSE)
)
p <- plot_ly(iris, x = Petal.Length, y = Petal.Width, group = Species,
             xaxis = paste0("x", id), mode = "markers")

p <- layout(
  p,
  xaxis = list(range = range(Petal.Length) + c(-0.1, 0.1)),
  yaxis = list(range = range(Petal.Width) + c(-0.1, 0.1))
)

subplot(p)

p <- plot_ly(iris, x = Petal.Length, y = Petal.Width, group = Species,
             xaxis = paste0("x", id), mode = "markers")

p <- subplot(p)
layout(p,
       xaxis = list(range = range(iris$Petal.Length) + c(-0.1, 0.1)),
       yaxis = list(range = range(iris$Petal.Width) + c(-0.1, 0.1)),
       
       xaxis2 = list(range = range(iris$Petal.Length) + c(-0.1, 0.1)),
       yaxis2 = list(range = range(iris$Petal.Width) + c(-0.1, 0.1)),
       
       xaxis3 = list(range = range(iris$Petal.Length) + c(-0.1, 0.1)),
       yaxis3 = list(range = range(iris$Petal.Width) + c(-0.1, 0.1))
)
#Updating Subplots
iris$id <- as.integer(iris$Species)
p <- plot_ly(iris, x = Petal.Length, y = Petal.Width, group = Species,
             xaxis = paste0("x", id), mode = "markers")
p <- plotly_build(subplot(p))
p$filename <- "overwrite_subplot"
p

p <- layout(title="Overwrite Subplots")
p <- plotly_build(subplot(p))
p$filename <- "overwrite_subplot"
p
