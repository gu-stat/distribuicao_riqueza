# ************************************************************************* ----
# Packages                                                                  ----
# >                                                                         ----

# Fonts
## install.packages('showtext', dependencies = TRUE)
## install.packages("emojifont")
## install.packages("sysfonts")

# Graphics
## install.packages("ggplot2")
library(ggplot2)
# > Instructions to Import Fonts ===============================================

## Install the Lato font (temporary)
sysfonts::font_add_google("Lato")
sysfonts::font_add_google("Lato", "Lato Light")

## Install FontAwesome (temporary)
sysfonts::font_add(
  family  = 'FontAwesome',
  regular = 'https://github.com/FortAwesome/Font-Awesome/blob/master/webfonts/fa-regular-400.ttf',
  bold    = 'https://github.com/FortAwesome/Font-Awesome/blob/master/webfonts/fa-solid-900.ttf'
)

## Check if they were imported (should see 'Lato', 'Lato Light',
## and 'FontAwesome')
sysfonts::font_families()

## automatically use showtext for new devices
showtext::showtext_auto() 

# ************************************************************************* ----
# Inputs for the Plot                                                       ----
# >                                                                         ----

# > DATA FRAME: Icons - df.icons ===============================================

cols.icons  <- 50 # number of columns to plot on the x-axis

total.icons <- rep(850, 5) # plot 850 icons for each of the 5 income brackets

df.icons    <- expand.grid(
  x = 1:cols.icons,
  y = seq_len((ceiling(sum(total.icons)/cols.icons)))
)

# > DATA FRAME: Ocean - df.ocean ===============================================

xmin.ocean <- 70  # x-coord to begin plotting the ocean
xmax.ocean <- 121 # x-coord to end plotting the ocean

ymin.ocean <- 0  # y-coord to begin plotting the ocean
ymax.ocean <- ceiling(sum(total.icons)/cols.icons) + 1 # y-coord ocean's end

df.ocean <- expand.grid(x = xmin.ocean:xmax.ocean, y = ymin.ocean:ymax.ocean)

# ************************************************************************* ----
# Layer Function                                                            ----
# >                                                                         ----

# \__ People -------------------------------------------------------------------

# We color the people following the inequality data from wid.world

# We multiply the length of each percentile by 1000:

# Percentile	Total_People
# p99.9p100	  1
# p99p99.9	  9
# p90p99	    90
# p50p90	    400
# p0p50	      500

layers.fct <- function(var.inv, var.b50, var.m40, var.n9, var.n0.9, var.t0.1)  {
  c(# \____ Bottom 50%  ----#----------------------------------------------#---- 
    
    # This layer has 500 colored icons (see p0p50), and 350 transparent icons
    # Of the 350 transparent icons:
    # 100 icons will be in the first layer and last layers (50 each layer)
    # 100 extra transparent icons before the first colored layer
    # 150 extra transparent icons after the last colored layer
    
    rep(var.inv, 50),      # layer   1     - 50 transparent icons
    rep(var.inv, 50),      # layer   2     - 50 transparent icons
    rep(var.inv, 50),      # layer   3     - 50 transparent icons
    rep(var.b50, 50*10),   # layers  4-13  - 50*10 colored icons
    rep(var.inv, 50),      # layer   14    - 50 transparent icons
    rep(var.inv, 50),      # layer   15    - 50 transparent icons
    rep(var.inv, 50),      # layer   16    - 50 transparent icons
    rep(var.inv, 50),      # layer   17    - 50 transparent icons
    
    # \____ Middle 40%  ----#----------------------------------------------#----
    
    # This layer has 400 colored icons (see p50p90), and 450 transparent icons
    # Of the 450 transparent icons:
    # 100 icons will be in the first layer and last layers (50 each layer)
    # 100 extra transparent icons before the first colored layer
    # 150 extra transparent icons after the last colored layer
    # 
    # In this case, each colored layer WON'T be fully colored.
    # There will be 5 transparent icons, 40 colored, and 5 transparent
    # There will be 10 layers like this.
    
    
    rep(var.inv, 50),      # layer   1     - 50 transparent icons
    rep(var.inv, 50*2),    # layers  2-3   - 50 transparent icons
    rep(c(rep(var.inv, 5),  rep(var.m40, 40), rep(var.inv, 5)), 10),#layers 4-13
    rep(var.inv, 50),      # layer   14    - 50 transparent icons
    rep(var.inv, 50),      # layer   15    - 50 transparent icons
    rep(var.inv, 50),      # layer   16    - 50 transparent icons
    rep(var.inv, 50),      # layer   17    - 50 transparent icons
    
    # \____ Next 9%  ----#-------------------------------------------------#----
    
    # This layer has 90 colored icons (see p90p99), and 760 transparent icons
    # Of the 760 transparent icons:
    # 100 icons will be in the first layer and last layers (50 each layer)
    # 100 extra transparent icons before the first colored layer
    # 100 extra transparent icons after the last colored layer
    # 
    # In this case, each colored layer WON'T be fully colored.
    # There will be 17 transparent icons, 15 colored, and 18 transparent
    # There will be 6 layers like this.
    # Between each of these layers, there will be a layer with 50 transparent
    # icons.
    
    rep(var.inv, 50),                                         # layer 1
    rep(var.inv, 50),                                         # layer 2
    rep(var.inv, 50),                                         # layer 3
    c(rep(var.inv, 17), rep(var.n9, 15),   rep(var.inv, 18)), # layer 4
    rep(var.inv, 50),                                         # layer 5
    c(rep(var.inv, 17), rep(var.n9, 15),   rep(var.inv, 18)), # layer 6
    rep(var.inv,50),                                          # layer 7
    c(rep(var.inv, 17), rep(var.n9, 15),   rep(var.inv, 18)), # layer 8
    rep(var.inv,50),                                          # layer 9
    c(rep(var.inv, 17), rep(var.n9, 15),   rep(var.inv, 18)), # layer 10
    rep(var.inv,50),                                          # layer 11
    c(rep(var.inv, 17), rep(var.n9, 15),   rep(var.inv, 18)), # layer 12
    rep(var.inv,50),                                          # layer 13
    c(rep(var.inv, 17), rep(var.n9, 15),   rep(var.inv, 18)), # layer 14
    rep(var.inv, 50),                                         # layer 15
    rep(var.inv, 50),                                         # layer 16
    rep(var.inv, 50),                                         # layer 17
    
    # \____ Next 0.9%  ----#-----------------------------------------------#---- 
    
    # This layer has 9 colored icons (see p99p99.9), and 691 transparent icons
    # Of the 691 transparent icons:
    # 100 icons will be in the first layer and last layers (50 each layer)
    # 150 extra transparent icons before the first colored layer
    # 150 extra transparent icons after the last colored layer
    # 
    # In this case, each colored layer WON'T be fully colored.
    # There will be 22 transparent icons, then 
    # 1 colored,  1 transparent, 1 colored,  1 transparent, 1 colored,  
    # 1 transparent, and then 22 transparent icons.
    # There will be 3 layers like this.
    # Between each of these layers, there will be a layer with 150 transparent
    # icons.
    
    rep(var.inv, 50),                                                  # layer 1
    rep(var.inv, 50),                                                  # layer 2
    rep(var.inv, 50),                                                  # layer 3
    rep(var.inv, 50),                                                  # layer 4
    c(rep(var.inv, 22), rep(c(var.n0.9, var.inv), 3),  rep(var.inv, 22)),#layer5
    rep(var.inv, 50),                                                  # layer 6
    rep(var.inv, 50),                                                  # layer 7
    rep(var.inv, 50),                                                  # layer 8
    c(rep(var.inv, 22), rep(c(var.n0.9, var.inv), 3),  rep(var.inv, 22)),#layer9
    rep(var.inv, 50),                                                  # layer10
    rep(var.inv, 50),                                                  # layer11
    rep(var.inv, 50),                                                  # layer12
    c(rep(var.inv, 22), rep(c(var.n0.9, var.inv), 3),  rep(var.inv, 22)),#laye13
    rep(var.inv, 50),                                                  # layer14
    rep(var.inv, 50),                                                  # layer15
    rep(var.inv, 50),                                                  # layer16
    rep(var.inv, 50),                                                  # layer17
    # \____ Top 0.1%  ----#-----------------------------------------------#---- 
    
    # This layer has 1 colored icons (see p99.9p100), and 699 transparent icons
    # Of the 699 transparent icons:
    # 100 icons will be in the first layer and last layers (50 each layer)
    # 350 extra transparent icons before the first colored layer
    # 350 extra transparent icons after the last colored layer
    # 
    # In this case, the colored layer WON'T be fully colored.
    # There will be 24 transparent icons, then 1 colored, and 25 transparent.
    
    rep(var.inv,   50),                                            # layer  1
    rep(var.inv,   50*7),                                          # layers 2-8
    c(rep(var.inv, 24),     rep(var.t0.1, 1),    rep(var.inv, 25)),# layer 9
    rep(var.inv,   50*7),                                          # layers10-16
    rep(var.inv,   50)                                             # layer17
  )
}
# ************************************************************************* ----
# Plot Options                                                              ----
# >                                                                         ----

# > Glyphs =====================================================================

## Search desired glyph/icon here: https://fontawesome.com/v5/cheatsheet

### If the selected icon is in the 'Solid' then we should use fontface = "bold"
### If the selected icon is in the 'Regular' then we should use fontface = "plain"
glyph.fontface.people <- "bold"
glyph.fontface.fish   <- "bold"

glyph.people <- intToUtf8(strtoi("f007", 16L))
glyph.fish   <- intToUtf8(strtoi("f578", 16L))

# > FONT =======================================================================

# \__ Color --------------------------------------------------------------------
font.color.title      <- "white"
font.color.subtitle   <- "white"
font.color.annotation <- "#d6d6d6"
font.color.source     <- "white"

# \__ Size ---------------------------------------------------------------------
font.size.title      <- 22
font.size.subtitle   <- 17
font.size.annotation <- 4.5
font.size.source     <- 3.5

# \__ Family -------------------------------------------------------------------
font.family.title      <- "Lato"
font.family.subtitle   <- "Lato Light"
font.family.annotation <- "Lato"
font.family.source     <- "Lato"

# > GLYPH SIZE =================================================================

# \__ People -------------------------------------------------------------------
glyph.size.inv  <- 3
glyph.size.b50   <- 1.75
glyph.size.m40   <- 1.8
glyph.size.n9    <- 3.15
glyph.size.n0.9  <- 3.75
glyph.size.t0.1  <- 5

# \__ Fish ---------------------------------------------------------------------
glyph.size.fish <- 3.25

# > COLORS =====================================================================

# \__ Separator ----------------------------------------------------------------
color.separator <- '#4e4d49'

# \__ Ocean --------------------------------------------------------------------
color.ocean <- '#00379a'

# \__ Background ---------------------------------------------------------------
color.background <- "#3a3935"

# \__ Income Brackets ----------------------------------------------------------

col.inv   <- "transparent" #              - invisible layers
col.b50   <- '#ff6d12'     # orange       - bottom 50%
col.m40   <- '#8bdc97'     # light green  - middle 40%
col.n9    <- "#fffca1"     # light yellow - p90 - p99
col.n0.9  <- "#ffbffe"     # light pink   - p99 - p99.9
col.t0.1  <- "white"       #              - top 0.1%

# \__ Fish ---------------------------------------------------------------------

# We color the fish following the inequality data from wid.world
# Data for Net Personal wealth (share)
# Access on: 05/28/2021
# Country	Year	Percentile	Share
# USA	    2019	p99.9p100	  0.1780
# USA	    2019	p99p99.9	  0.1707
# USA	    2019	p90p99	    0.3580
# USA	    2019	p50p90	    0.2782
# USA	    2019	p0p50	      0.0151

# We multiply each share by 1000 and round it to the nearest integer*:
# Percentile	Total
# p99.9p100	  178
# p99p99.9	  171
# p90p99	    358
# p50p90	    278
# p0p50	      015

colors.fish <-   
  c(# \____ Bottom 50%  ----#----------------------------------------------#---- 
    # We have 17 layers of 50 icons each.
    # This layer has 015 colored icons (see p0p50), and 835 transparent icons.
    #
    # Of the 835 transparent icons:
    # 100 icons will be in the first layer and last layers (50 each layer)
    # 350 extra transparent icons before the first colored layer
    # 350 extra transparent icons after the last colored layer
    # 
    # In this case, the colored layer WON'T be fully colored.
    # There will be 17 transparent icons, then 15 colored, and 18 transparent.
    
    rep(col.inv, 50),                                               # layer 1
    rep(col.inv, 50),                                               # layer 2
    rep(col.inv, 50),                                               # layer 3
    rep(col.inv, 50),                                               # layer 4
    rep(col.inv, 50),                                               # layer 5
    rep(col.inv, 50),                                               # layer 6
    rep(col.inv, 50),                                               # layer 7
    rep(col.inv, 50),                                               # layer 8
    c(rep(col.inv, 17),     rep(col.b50, 15),    rep(col.inv, 18)), # layer 9
    rep(col.inv, 50),                                               # layer 10
    rep(col.inv, 50),                                               # layer 11
    rep(col.inv, 50),                                               # layer 12
    rep(col.inv, 50),                                               # layer 13
    rep(col.inv, 50),                                               # layer 14
    rep(col.inv, 50),                                               # layer 15
    rep(col.inv, 50),                                               # layer 16
    rep(col.inv, 50),                                               # layer 17
    
    # \____ Middle 40%  ----#----------------------------------------------#----
    
    # This group has 278 colored icons (see p50p90), and 572 transparent icons.
    #
    # Of the 572 transparent icons:
    # 50 icons will be in the first layer
    # 50 icons will be in the last layer
    # 50 extra transparent icons before the first colored layer
    # 99 extra transparent icons after the last colored layer
    # 300 transparent icons between the colored layers
    
    rep(col.inv, 50),   # layer 1      - 50 transparent icons
    rep(col.inv, 50),   # layer 2      - 50 transparent icons
    rep(col.inv, 50),   # layer 3      - 50 transparent icons
    rep(col.m40, 50),   # layer 4      - 50 colored icons
    rep(col.inv, 50),   # layer 5      - 50 transparent icons
    rep(col.m40, 50),   # layer 6      - 50 colored icons
    rep(col.inv, 50),   # layer 7      - 50 transparent icons
    rep(col.m40, 50),   # layer 8      - 50 colored icons
    rep(col.inv, 50),   # layer 9      - 50 transparent icons
    rep(col.m40, 50),   # layer 10      - 50 colored icons
    rep(col.inv, 50),   # layer 11     - 50 transparent icons
    rep(col.m40, 50),   # layer 12     - 50 colored icons
    rep(col.inv, 50),   # layer 13     - 50 transparent icons
    c(rep(col.m40, 28),    rep(col.inv, 22)), # layer 14
    rep(col.inv, 50),   # layer 15     - 50 transparent icons
    rep(col.inv, 50),   # layer 16     - 50 transparent icons
    rep(col.inv, 50),   # layer 17     - 50 transparent icons
    
    # \____ Next 9%  ----#-------------------------------------------------#----
    
    # This group has 358 colored icons (see p90p99), and 492 transparent icons.
    #
    # Of the 492 transparent icons:
    # 50 icons will be in the first layer
    # 50 icons will be in the last layer
    # 50 extra transparent icons before the first colored layer
    # 53 extra transparent icons after the last colored layer
    # 300 transparent icons between the colored layers
    
    rep(col.inv, 50),   # layer 1      - 50 transparent icons
    rep(col.n9,  50),   # layer 2      - 50 colored icons
    rep(col.inv, 50),   # layer 3      - 50 transparent icons
    rep(col.n9,  50),   # layer 4      - 50 colored icons
    rep(col.inv, 50),   # layer 5      - 50 transparent icons
    rep(col.n9,  50),   # layer 6      - 50 colored icons
    rep(col.inv, 50),   # layer 7      - 50 transparent icons
    rep(col.n9,  50),   # layer 8      - 50 colored icons
    rep(col.inv, 50),   # layer 9     - 50 transparent icons
    rep(col.n9,  50),   # layer 10     - 50 colored icons
    rep(col.inv, 50),   # layer 11     - 50 transparent icons
    rep(col.n9,  50),   # layer 12     - 50 colored icons
    rep(col.inv, 50),   # layer 13     - 50 transparent icons
    rep(col.n9,  50),   # layer 14     - 50 colored icons
    rep(col.inv, 50),   # layer 15      - 50 transparent icons
    c(rep(col.n9, 8),    rep(col.inv, 42)), # layer 16
    rep(col.inv, 50),   # layer 17     - 50 transparent icons
    
    
    # \____ Next 0.9%  ----#-----------------------------------------------#---- 
    
    # This group has 171 colored icons (see p99p99.9), and 679 transparent icons
    # Of the 677 transparent icons:
    # 50 icons will be in the first layer
    # 50 icons will be in the last layer
    # 2500 extra transparent icons before the first colored layer
    # 227 extra transparent icons after the last colored layer
    # 150 transparent icons between the colored layers
    
    rep(col.inv,  50),   # layer  1     - 50 transparent icons
    rep(col.inv,  50*4), # layers 2-5   - 50*4 transparent icons
    rep(col.n0.9, 50),   # layer  6     - 50 colored icons
    rep(col.inv,  50),   # layer  7     - 50 transparent icons
    rep(col.n0.9, 50),   # layer  8     - 50 colored icons
    rep(col.inv,  50),   # layer  9     - 50 transparent icons
    rep(col.n0.9, 50),   # layer  10    - 50 colored icons
    rep(col.inv,  50),   # layer  11    - 50 transparent icons
    c(rep(col.n0.9, 21),    rep(col.inv, 29)), # layer 12
    rep(col.inv,  50*4), # layers 13-16 - 50*4 transparent icons
    rep(col.inv,  50),   # layer  17    - 50 transparent icons
    
    # \____ Top 0.1%  ----#-----------------------------------------------#---- 
    
    # This layer has 178 colored icons (see p99p99.9), and 672 transparent icons
    # Of the 672 transparent icons:
    # 50 icons will be in the first layer
    # 50 icons will be in the last layer
    # 200 extra transparent icons before the first colored layer
    # 222 extra transparent icons after the last colored layer
    # 150 transparent icons between the colored layers
    
    rep(col.inv,  50),   # layer  1      - 50 transparent icons
    rep(col.inv,  50*4), # layers 2-5    - 50*4 transparent icons
    rep(col.t0.1, 50),   # layer  6      - 50 colored icons
    rep(col.inv,  50),   # layer  7      - 50 transparent icons
    rep(col.t0.1, 50),   # layer  8      - 50 colored icons
    rep(col.inv,  50),   # layer  9      - 50 transparent icons
    rep(col.t0.1, 50),   # layer  10     - 50 colored icons
    rep(col.inv,  50),   # layer  11     - 50 transparent icons
    rep(col.t0.1, 28),   # layer  12.1   - 28 colored icons
    rep(col.inv,  22),   # layer  12.2   - 22 transparent icons
    rep(col.inv,  50*4), # layers 13-16  - 50*4 transparent icons
    rep(col.inv,  50)    # layer  17     - 50 transparent icons
  )

# ************************************************************************* ----
# Plot                                                                      ----
# >                                                                         ----

tmpPlot <- ggplot2::ggplot() +
  # > PLOT: People =============================================================
  ggplot2::geom_text(
    data    = df.icons,
    mapping = aes(
      x     = x + 0.5, 
      y     = y, 
      label = glyph.people
    ), 
    color   = layers.fct(
      var.inv  = col.inv, 
      var.b50  = col.b50, 
      var.m40  = col.m40, 
      var.n9   = col.n9, 
      var.n0.9 = col.n0.9, 
      var.t0.1 = col.t0.1
    ),
    
    size = layers.fct(
      var.inv  = glyph.size.inv,   
      var.b50  = glyph.size.b50,   
      var.m40  = glyph.size.m40,   
      var.n9   = glyph.size.n9,  
      var.n0.9 = glyph.size.n0.9, 
      var.t0.1 = glyph.size.t0.1
    ), 

    show.legend = FALSE,
    family      = "FontAwesome",
    fontface    = glyph.fontface.people
  ) + 
  # # > PLOT: Ocean ==============================================================
  ggplot2::geom_raster(
    data    = df.ocean,
    mapping = aes(x = x, y = y),
    fill    = "blue"
  ) +
  # > PLOT: Fish ===============================================================
  ggplot2::geom_text(
    data    = df.icons,
    mapping = aes(
      x     = x + 70, 
      y     = y, 
      label = glyph.fish
    ),
    color   = colors.fish,
    size    = glyph.size.fish,
    vjust   = 0.5,
    show.legend = FALSE,
    family      = 'FontAwesome',
    fontface    = glyph.fontface.fish
  ) +
  # > PLOT: Separators =========================================================
  ggplot2::geom_point(
    color   = color.separator,
    mapping = ggplot2::aes(
      x = rep(1:xmax.ocean, 6), 
      y = c(rep(0,              length(1:xmax.ocean)), # below bot50
            rep(ymax.ocean/5,   length(1:xmax.ocean)), # btwn bot50 & mid40
            rep(2*ymax.ocean/5, length(1:xmax.ocean)), # btwn mid40 & next9
            rep(3*ymax.ocean/5, length(1:xmax.ocean)), # btwn next9 & next0.9
            rep(4*ymax.ocean/5, length(1:xmax.ocean)), # btwn next0.9 & top0.1
            rep(5*ymax.ocean/5, length(1:xmax.ocean))) # above top0.1
    )
  ) +
# > SCALE ======================================================================
  scale_y_continuous(expand = expand_scale(add = c(3, 3))) +
  scale_x_continuous(expand = expand_scale(mult = c(0.015, 0.01))) +
# > TITLE & SUBTITLE ===========================================================
  labs(
    title = "     É melhor dar o peixe ou ensinar a pescar?",
    subtitle = paste0("                     ",
                      "Se a riqueza dos EUA* equivalesse a 1.000 peixes...")
  ) +
  # > SOURCE =====================================================================
  annotate(geom = "text",
           x      = rep(c(-2.75, 3.75), 2),
           y      = c(-3, -3, -5.25, -5.25),
           hjust  = rep(0, 4),
           size   = rep(font.size.source, 4),
           family = rep(font.family.source, 4),
           color  = rep(font.color.source, 4),
           label  = c("Infográfico: ",
                      "Gustavo Varela-Alvarenga - www.ogustavo.com",
                      "Fonte dos dados: ",
                      "World Inequality Database - wid.world")
  ) +
  # > NOTES ======================================================================
  annotate(geom   = "text",
           x      = rep(121, 2),
           y      = c(-2.25, -4.5),
           hjust  = rep(1, 2),
           size   = rep(font.size.source,   2),
           family = rep(font.family.source, 2),
           color  = rep(font.color.source,  2),
           label  = c(paste0("*Riqueza pessoal líquida em 2019."),
                      paste0("**A unidade é o indivíduo adulto ",
                             "(20 anos ou mais).")
           )
           
  ) +
# > ANNOTATIONS ================================================================
# \__ 1,000 People -------------------------------------------------------------
  annotate(geom   = "text",
           x      = -2,
           y      = (5*ymax.ocean/5)+2,
           hjust  = 0,
           size   = font.size.annotation,
           family = font.family.annotation,
           color  = font.color.annotation,
           label  = "Considere que 100% da população** equivale a 1.000 pessoas:"
  ) +
# \__ Bottom 50% ---------------------------------------------------------------
# \____ Annotation: Middle ----#-------------------------------------------#----
  annotate(geom = "text", 
           x      = rep(60, 3), 
           y      = c((1*ymax.ocean/5)-4.2, 
                      (1*ymax.ocean/5)-8.6, 
                      (1*ymax.ocean/5)-13), 
           hjust  = rep(0.5, 3),
           family = rep(font.family.annotation, 3),
           size   = rep(font.size.annotation,   3),
           color  = c(col.b50, font.color.annotation, col.b50),
           label   = c("500 pessoas", "dividiriam", "15 peixes")
  ) + 
  # \____ Annotation: Left of People  ----#----------------------------------#----
  annotate(geom = "text", 
           x      = -1.5, 
           y      = (1*ymax.ocean/5)-8.6, 
           hjust  = 0.5,
           family = font.family.annotation,
           size   = font.size.source,
           color  = font.color.annotation,
           label   = "50% mais \n pobres"
  ) +
  annotate(geom = "line", 
           x        = -1.5, 
           y        = c(0:((1*ymax.ocean/5)-10.5)),
           color    = font.color.annotation,
           linetype = 3
  ) +
  annotate(geom = "line", 
           x        = -1.5, 
           y        = c(((1*ymax.ocean/5)-6.5):((1*ymax.ocean/5))),
           color    = font.color.annotation,
           linetype = 3
  ) +
# \__ Middle 40% ---------------------------------------------------------------
# \____ Annotation: Middle ----#-------------------------------------------#----
  annotate(geom = "text", 
           x      = rep(60, 3), 
           y      = c((2*ymax.ocean/5)-4.2, 
                      (2*ymax.ocean/5)-8.6, 
                      (2*ymax.ocean/5)-13), 
           hjust  = rep(0.5, 3),
           family = rep(font.family.annotation, 3),
           size   = rep(font.size.annotation,   3),
           color  = c(col.m40, font.color.annotation, col.m40),
           label  = c("400 pessoas", "dividiriam", "278 peixes") 
  ) +
  # \____ Annotation: Left of People  ----#----------------------------------#----
  annotate(geom = "text", 
           x      = -1.5, 
           y      = (2*ymax.ocean/5)-8.6, 
           hjust  = 0.5,
           family = font.family.annotation,
           size   = font.size.source,
           color  = font.color.annotation,
           label   = "Os 40% \n do meio"
  ) +
  annotate(geom = "line", 
           x        = -1.5,
           y        = c((1*ymax.ocean/5):((2*ymax.ocean/5)-10.5)),
           color    = font.color.annotation,
           linetype = 1
  ) +
  annotate(geom = "line", 
           x        = -1.5, 
           y        = c(((2*ymax.ocean/5)-6.5):((2*ymax.ocean/5))),
           color    = font.color.annotation,
           linetype = 1
  ) +  
# \__ Next 9% ------------------------------------------------------------------
# \____ Annotation: Middle ----#-------------------------------------------#----
  annotate(geom = "text", 
           x      = rep(60, 3), 
           y      = c((3*ymax.ocean/5)-4.2, 
                      (3*ymax.ocean/5)-8.6, 
                      (3*ymax.ocean/5)-13), 
           hjust  = rep(0.5, 3),
           family = rep(font.family.annotation, 3),
           size   = rep(font.size.annotation,   3),
           color  = c(col.n9, font.color.annotation, col.n9),
           label  = c("90 pessoas", "dividiriam", "358 peixes") 
  ) +
  # \____ Annotation: Left of People  ----#----------------------------------#----
  annotate(geom = "text", 
           x      = -1.5, 
           y      = (3*ymax.ocean/5)-8.6, 
           hjust  = 0.5,
           family = font.family.annotation,
           size   = font.size.source,
           color  = font.color.annotation,
           label  = "Os próximos \n 9%"
  ) +
  annotate(geom = "line", 
           x        = -1.5, 
           y        = c((2*ymax.ocean/5):((3*ymax.ocean/5)-10.5)),
           color    = font.color.annotation,
           linetype = 3
  ) +
  annotate(geom = "line", 
           x        = -1.5, 
           y        = c(((3*ymax.ocean/5)-6.5):((3*ymax.ocean/5))),
           color    = font.color.annotation,
           linetype = 3
  ) +  
  # \__ Next 0.9% ----------------------------------------------------------------
  annotate(geom = "text", 
           x      = rep(60, 3), 
           y      = c((4*ymax.ocean/5)-4.2, 
                      (4*ymax.ocean/5)-8.6, 
                      (4*ymax.ocean/5)-13), 
           hjust  = rep(0.5, 3),
           family = rep(font.family.annotation, 3),
           size   = rep(font.size.annotation,   3),
           color  = c(col.n0.9, font.color.annotation, col.n0.9),
           label  = c("9 pessoas", "dividiriam", "171 peixes") 
  ) +
  # \__ Top 0.1% -----------------------------------------------------------------
  annotate(geom = "text", 
           x      = rep(60, 3), 
           y      = c((5*ymax.ocean/5)-4.2, 
                      (5*ymax.ocean/5)-8.6, 
                      (5*ymax.ocean/5)-13), 
           hjust  = rep(0.5, 3),
           family = rep(font.family.annotation, 3),
           size   = rep(font.size.annotation,   3),
           color  = c(col.t0.1, font.color.annotation, col.t0.1),
           label  = c("1 pessoa", "teria", "178 peixes"),
           fontface = c("bold", "plain", "bold")
  ) +
  # \__ Top 1% -----------------------------------------------------------------
  # \____ Annotation: Left of People  ----#----------------------------------#--
  annotate(geom = "text", 
           x      = -1.5, 
           y      = 4*ymax.ocean/5, 
           hjust  = 0.5,
           family = font.family.annotation,
           size   = font.size.source,
           color  = font.color.annotation,
           label  = "Top \n 1%"
  ) +
  annotate(geom = "line", 
           x        = -1.5, 
           y        = c((3*ymax.ocean/5):((4*ymax.ocean/5)-2.25)),
           color    = font.color.annotation,
           linetype = 1
  ) +
  annotate(geom = "line", 
           x        = -1.5, 
           y        = c(((4*ymax.ocean/5)+2):((5*ymax.ocean/5)+0.7)),
           color    = font.color.annotation,
           linetype = 1
  ) + 
# > THEME ======================================================================
  theme(
# \__ Axis ---------------------------------------------------------------------
    axis.text        = element_blank(),
    axis.title.x     = element_blank(),
    axis.title.y     = element_blank(),
    
# \__ Panel --------------------------------------------------------------------
    panel.background = element_rect(fill = color.background, color = NA),
    panel.grid       = element_blank(),
    
# \__ Plot Options -------------------------------------------------------------
    plot.background  = element_rect(fill = color.background, color = NA), 
    plot.margin      = margin(t = 0.75, r = 0.25, b = 0, l = 0.25, unit = "cm"),
    
    plot.title       = element_text(size   = font.size.title, 
                                    color  = font.color.title,
                                    family = font.family.title,
                                    margin = margin(b=0.5, unit = "cm")),
    
    plot.subtitle    = element_text(size   = font.size.subtitle, 
                                    color  = font.color.subtitle,
                                    family = font.family.subtitle,
                                    margin = margin(b=0.75, unit = "cm"))
                                 
  )


## IF USING WINDOWS' GRAPHIC DEVICE, UNCOMMENT THE NEXT LINE
windows()
tmpPlot


# ************************************************************************* ----
# Export Plot                                                               ----
# >                                                                         ----

# I'M USING RSTUDIO'S EXPORT OPTION WITH Width: 1435 AND Height: 890

## IF USING WINDOWS GRAPHIC DEVICE, UNCOMMENT THE NEXT LINES
# ggplot2::ggsave(
#   filename = "infographic_inequality_br_v01.png",
#   plot     = tmpPlot,
#   device   = "png",
#   type     = "cairo",
#   dpi      = 320,
#   width    = 5,
#   height   = 2.5
# )

# IF USING WINDOWS GRAPHIC DEVICE, UNCOMMENT THE NEXT LINE
# dev.off()
