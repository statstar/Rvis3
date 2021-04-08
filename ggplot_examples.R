# 1. Correlation

# 1) Scatterplot
library(ggplot2)
data("midwest", package = "ggplot2")

# Scatterplot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) +
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) + 
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot", 
       caption = "Source: midwest")

plot(gg)


# 2) Scatterplot With Encircling
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
if(!require(ggalt)) install.packages("ggalt");library(ggalt)
midwest_select <- midwest[midwest$poptotal > 350000 & 
                            midwest$poptotal <= 500000 & 
                            midwest$area > 0.01 & 
                            midwest$area < 0.1, ]

# Plot
ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) +   # geom_point를 사용하여 점을 그려준다.
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) +   # smoothing line을 그려준다.
  geom_encircle(aes(x=area, y=poptotal), 
                data=midwest_select, 
                color="red", 
                size=2, 
                expand=0.08) +   # 붉은색 선을 그려준다.
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot + Encircle", 
       caption="Source: midwest")


# 3) Jitter Plot
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)

# 3.1)
data(mpg, package="ggplot2") # ggplot2의 mpg data를 가져온다.

g <- ggplot(mpg, aes(cty, hwy))

# Scatterplot
g + geom_point() + 
  geom_smooth(method="lm", se=F) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Scatterplot with overlapping points", 
       caption="Source: midwest")

# 3.2)
data(mpg, package="ggplot2")

# Scatterplot
g <- ggplot(mpg, aes(cty, hwy))
g + geom_jitter(width = .5, size=1) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Jittered Points")


# 4) Counts Chart
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
data(mpg, package="ggplot2")

# Scatterplot
g <- ggplot(mpg, aes(cty, hwy))
g + geom_count(col="tomato3", show.legend=F) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Counts Plot")


# 5) Bubble plot
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
data(mpg, package="ggplot2")

mpg_select <- mpg[mpg$manufacturer %in% c("audi", "ford", "honda", "hyundai"), ]

# Scatterplot
g <- ggplot(mpg_select, aes(displ, cty)) + 
  labs(subtitle="mpg: Displacement vs City Mileage",
       title="Bubble chart")

g + geom_jitter(aes(col=manufacturer, size=hwy)) + 
  geom_smooth(aes(col=manufacturer), method="lm", se=F)


 # 6) Animated Bubble chart
if(!require(plotly)) install.packages("plotly");library(plotly)
if(!require(gapminder)) install.packages("gapminer"); library(gapminder)


# p <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
#   geom_point(aes(size = pop, frame = year, ids = country)) +
#   scale_x_log10()
# 
# p <- ggplotly(p)

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started

# Sys.setenv("plotly_username"="statstar")
# Sys.setenv("plotly_api_key"="iMqNe0Zs7NojEeRgdUIA")
# 
# chart_link = api_create(p, filename="mulitple-trace")
# chart_link


# 7) Marginal Histogram / Boxplot
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
if(!require(ggExtra)) install.packages("ggExtra");library(ggExtra)
data(mpg, package="ggplot2")

# Scatterplot
mpg_select <- mpg[mpg$hwy >= 35 & mpg$cty > 27, ]
g <- ggplot(mpg, aes(cty, hwy)) + 
  geom_count() + 
  geom_smooth(method="lm", se=F)

ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")
ggMarginal(g, type = "density", fill="transparent")


# 8) Correlogram
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
if(!require(ggcorrplot)) install.packages("ggcorrplot");library(ggcorrplot)

# 상관행렬
data(mtcars)
corr <- round(cor(mtcars), 1)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)




# 2.Deviation

# 1) Diverging bars
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)

# 데이터 준비
data("mtcars")  # 데이터 불러들인다.
mtcars$`car name` <- rownames(mtcars)  # car name이라는 새로운 변수를 생성한다.
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # 정규화된 mpg를 계산한다.
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # 위 / 아래 평균 flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # 분류
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # plot에서 정렬된 순서를 유지하기 위해 factor로 변환합니다.

# Diverging Barcharts
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip()


# 2) Diverging Lollipop Chart
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)

ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_point(stat='identity', fill="black", size=6)  +
  geom_segment(aes(y = 0, 
                   x = `car name`, 
                   yend = mpg_z, 
                   xend = `car name`), 
               color = "black") +
  geom_text(color="white", size=2) +
  labs(title="Diverging Lollipop Chart", 
       subtitle="Normalized mileage from 'mtcars': Lollipop") + 
  ylim(-2.5, 2.5) +
  coord_flip()


# 3) Diverging Dot Plot
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)

# Plot
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_point(stat='identity', aes(col=mpg_type), size=6)  +
  scale_color_manual(name="Mileage", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="Diverging Dot Plot", 
       subtitle="Normalized mileage from 'mtcars': Dotplot") + 
  ylim(-2.5, 2.5) +
  coord_flip()


# 3. Ranking

# 1) Ordered Bar Chart
# 데이터 준비: 제조업체별 평균 도시 마일리지
cty_mpg <- aggregate(mpg$cty, by=list(mpg$manufacturer), FUN=mean)  # 합계
colnames(cty_mpg) <- c("make", "mileage")  # 열(변수) 이름 변경
cty_mpg <- cty_mpg[order(cty_mpg$mileage), ]  # 분류
cty_mpg$make <- factor(cty_mpg$make, levels = cty_mpg$make)  # plot에서 순서를 유지하는 것
head(cty_mpg, 4)

if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)

# Draw plot
ggplot(cty_mpg, aes(x=make, y=mileage)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


# 2) Lollipop Chart
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)

# Plot
ggplot(cty_mpg, aes(x=make, y=mileage)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=make, 
                   xend=make, 
                   y=0, 
                   yend=mileage)) + 
  labs(title="Lollipop Chart", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


# 3) Dot Plot
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
if(!require(scales)) install.packages("scales");library(scales)

# Plot
ggplot(cty_mpg, aes(x=make, y=mileage)) + 
  geom_point(col="tomato2", size=3) +   # 점을 그려준다.
  geom_segment(aes(x=make, 
                   xend=make, 
                   y=min(mileage), 
                   yend=max(mileage)), 
               linetype="dashed", 
               size=0.1) +   # 점선을 그려준다.
  labs(title="Dot Plot", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") +  
  coord_flip()


# 4) Slope Chart
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
if(!require(scales)) install.packages("scales");library(scales)
theme_set(theme_classic())

# prep data
df <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/gdppercap.csv")
colnames(df) <- c("continent", "1952", "1957")
left_label <- paste(df$continent, round(df$`1952`),sep=", ")
right_label <- paste(df$continent, round(df$`1957`),sep=", ")
df$class <- ifelse((df$`1957` - df$`1952`) < 0, "red", "green")

# Plot
p <- ggplot(df) + geom_segment(aes(x=1, xend=2, y=`1952`, yend=`1957`, col=class), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) +  # 선의 색상
  labs(x="", y="Mean GdpPerCap") +  # Axis labels
  xlim(.5, 2.5) + ylim(0,(1.1*(max(df$`1952`, df$`1957`))))  # X와 Y축의 한계

# Add texts
p <- p + geom_text(label=left_label, y=df$`1952`, x=rep(1, NROW(df)), hjust=1.1, size=3.5)
p <- p + geom_text(label=right_label, y=df$`1957`, x=rep(2, NROW(df)), hjust=-0.1, size=3.5)
p <- p + geom_text(label="Time 1", x=1, y=1.1*(max(df$`1952`, df$`1957`)), hjust=1.2, size=5)  # title
p <- p + geom_text(label="Time 2", x=2, y=1.1*(max(df$`1952`, df$`1957`)), hjust=-0.1, size=5)  # title

# Minify theme
p + theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm"))


# 4. Distribution

# 1) Histogram
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
theme_set(theme_classic())

# Histogram on a Continuous (Numeric) Variable
g <- ggplot(mpg, aes(displ)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=class), 
                   binwidth = .1, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Engine Displacement across Vehicle Classes")  

g + geom_histogram(aes(fill=class), 
                   bins=5, 
                   col="black", 
                   size=.1) +   # change number of bins
  labs(title="Histogram with Fixed Bins", 
       subtitle="Engine Displacement across Vehicle Classes") 


# 2) Histogram on a categorical variable
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
theme_set(theme_classic())

# Histogram on a Categorical variable
g <- ggplot(mpg, aes(manufacturer))
g + geom_bar(aes(fill=class), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Manufacturer across Vehicle Classes") 


# 3) Density plot
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
theme_set(theme_classic())

# Plot
g <- ggplot(mpg, aes(cty))
g + geom_density(aes(fill=factor(cyl)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="City Mileage Grouped by Number of cylinders",
       caption="Source: mpg",
       x="City Mileage",
       fill="# Cylinders")


# 4) Box Plot
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
theme_set(theme_classic())

# Plot
g <- ggplot(mpg, aes(class, cty))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

if(!require(ggthemes)) install.packages("ggthemes");library(ggthemes)
g <- ggplot(mpg, aes(class, cty))
g + geom_boxplot(aes(fill=factor(cyl))) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Box plot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")


# 5) Dot + Box Plot
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
theme_set(theme_bw())

# plot
g <- ggplot(mpg, aes(manufacturer, cty))
g + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Box plot + Dot plot", 
       subtitle="City Mileage vs Class: Each dot represents 1 row in source data",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")


# 6) Tufte Boxplot
if(!require(ggthemes)) install.packages("ggthemes");library(ggthemes)
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
theme_set(theme_tufte())  # ggthemes에서

# plot
g <- ggplot(mpg, aes(manufacturer, cty))
g + geom_tufteboxplot() + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Tufte Styled Boxplot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")


# 7) Violin Plot
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
theme_set(theme_bw())

# plot
g <- ggplot(mpg, aes(class, cty))
g + geom_violin() + 
  labs(title="Violin plot", 
       subtitle="City Mileage vs Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")


# 8) Population Pyramid
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
if(!require(ggthemes)) install.packages("ggthemes");library(ggthemes)

# 데이터를 불러온다.
email_campaign_funnel <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/email_campaign_funnel.csv")

# X축 나누기 및 레이블 
brks <- seq(-15000000, 15000000, 5000000)
lbls = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")

# Plot
ggplot(email_campaign_funnel, aes(x = Stage, y = Users, fill = Gender)) +   # 열 채우기
  geom_bar(stat = "identity", width = .6) +   # 막대를 그린다.
  scale_y_continuous(breaks = brks,   # X축 나누기
                     labels = lbls) + # 레이블
  coord_flip() +  # Flip 축
  labs(title="Email Campaign Funnel") +
  theme_tufte() +  # ggfortify의 Tufte 테마
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # plot 제목의 위치를 가운데로
  scale_fill_brewer(palette = "Dark2")  # 팔레트 색상지정




# 5. Composition

# 1) Waffle Chart
var <- mpg$class  # 범주형 데이터

## 데이터 준비 (여기서는 변경할 사항 없음)
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
categ_table <- round(table(var) * ((nrows*nrows)/(length(var))))
categ_table

df$category <- factor(rep(names(categ_table), categ_table))

## Plot
ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_brewer(palette = "Set3") +
  labs(title="Waffle Chart", subtitle="'Class' of vehicles",
       caption="Source: mpg") + 
  theme(panel.border = element_rect(size = 2),
        plot.title = element_text(size = rel(1.2)),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "right")


# 2) Pie Chart
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
theme_set(theme_classic())

df <- as.data.frame(table(mpg$class))
colnames(df) <- c("class", "freq")
pie <- ggplot(df, aes(x = "", y=freq, fill = factor(class))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of class", 
       caption="Source: mpg")

pie + coord_polar(theta = "y", start=0)

# mpg$class
pie <- ggplot(mpg, aes(x = "", fill = factor(class))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of class", 
       caption="Source: mpg")

pie + coord_polar(theta = "y", start=0)


 # 3) Treemap 안돌아갑니다.
 if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
 if(!require(treemapify)) install.packages("treemapify");library(treemapify)
 
head(G20)

ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label = country,
                subgroup = region)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T)


ggplot(G20, aes(area = 1, label = country, subgroup = hemisphere,
                subgroup2 = region, subgroup3 = econ_classification)) +
  geom_treemap() +
  geom_treemap_subgroup3_border(colour = "blue", size = 1) +
  geom_treemap_subgroup2_border(colour = "white", size = 3) +
  geom_treemap_subgroup_border(colour = "red", size = 5) +
  geom_treemap_subgroup_text(
    place = "middle",
    colour = "red",
    alpha = 0.5,
    grow = T
  ) +
  geom_treemap_subgroup2_text(
    colour = "white",
    alpha = 0.5,
    fontface = "italic"
  ) +
  geom_treemap_subgroup3_text(place = "top", colour = "blue", alpha = 0.5) +
  geom_treemap_text(colour = "white", place = "middle", reflow = T)


# 4) Bar Chart
freqtable <- table(mpg$manufacturer)
df <- as.data.frame.table(freqtable)
head(df)

# plot
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
theme_set(theme_classic())

# Plot
g <- ggplot(df, aes(Var1, Freq))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Bar Chart", 
       subtitle="Manufacturer of vehicles", 
       caption="Source: Frequency of Manufacturers from 'mpg' dataset") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# 범주형 열 변수에서
g <- ggplot(mpg, aes(manufacturer))
g + geom_bar(aes(fill=class), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart", 
       subtitle="Manufacturer of vehicles", 
       caption="Source: Manufacturers from 'mpg' dataset")



# 8) Calendar Heatmap
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
if(!require(plyr)) install.packages("plyr");library(plyr)
if(!require(scales)) install.packages("scales");library(scales)
if(!require(zoo)) install.packages("zoo");library(zoo)

df <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/yahoo.csv")
df$date <- as.Date(df$date)  # 날짜 형식
df <- df[df$year >= 2012, ]  # 연도 필터

# 월별 주 생성
df$yearmonth <- as.yearmon(df$date)
df$yearmonthf <- factor(df$yearmonth)
df <- ddply(df,.(yearmonthf), transform, monthweek=1+week-min(week))  # 월별 주가 몇주인지 계산한다.
df <- df[, c("year", "yearmonthf", "monthf", "week", "monthweek", "weekdayf", "VIX.Close")]
head(df)

# Plot
ggplot(df, aes(monthweek, weekdayf, fill = VIX.Close)) + 
  geom_tile(colour = "white") + 
  facet_grid(year~monthf) + 
  scale_fill_gradient(low="red", high="green") +
  labs(x="Week of Month",
       y="",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Yahoo Closing Price", 
       fill="Close")


# 9) Slope Chart
if(!require(dplyr)) install.packages("dplyr");library(dplyr)
theme_set(theme_classic())
source_df <- read.csv("https://raw.githubusercontent.com/jkeirstead/r-slopegraph/master/cancer_survival_rates.csv")

# Define functions. Source: https://github.com/jkeirstead/r-slopegraph
tufte_sort <- function(df, x="year", y="value", group="group", method="tufte", min.space=0.05) {
  ## 먼저, 일관성을 위해 열 이름 변경
  ids <- match(c(x, y, group), names(df))
  df <- df[,ids]
  names(df) <- c("x", "y", "group")
  
  ## 모든 조합에 정의된 값이 있는지 확인하기위해 눈금을 확장
  tmp <- expand.grid(x=unique(df$x), group=unique(df$group))
  tmp <- merge(df, tmp, all.y=TRUE)
  df <- mutate(tmp, y=ifelse(is.na(y), 0, y))
  
  ## 행렬 모양으로 변환하고 첫 번째 열로 정렬
  require(reshape2)
  tmp <- dcast(df, group ~ x, value.var="y")
  ord <- order(tmp[,2])
  tmp <- tmp[ord,]
  
  min.space <- min.space*diff(range(tmp[,-1]))
  yshift <- numeric(nrow(tmp))
  ## 하단'행에서 시작
  ## 상단에 도착할 때까지 나머지 행에 대해 반복합니다.
  for (i in 2:nrow(tmp)) {
    ## 같은 간격으로 후속 행을 위로 이동합니다.
    ## two entries is >= minimum
    mat <- as.matrix(tmp[(i-1):i, -1])
    d.min <- min(diff(mat))
    yshift[i] <- ifelse(d.min < min.space, min.space - d.min, 0)
  }
  
  
  tmp <- cbind(tmp, yshift=cumsum(yshift))
  
  scale <- 1
  tmp <- melt(tmp, id=c("group", "yshift"), variable.name="x", value.name="y")
  ## 이러한 갭을 스케일링 할 수 있도록 별개의 변수에 저장하십시오. ypos = a * yshift + y
  
  tmp <- transform(tmp, ypos=y + scale*yshift)
  return(tmp)
  
}

plot_slopegraph <- function(df) {
  ylabs <- subset(df, x==head(x,1))$group
  yvals <- subset(df, x==head(x,1))$ypos
  fontSize <- 3
  gg <- ggplot(df,aes(x=x,y=ypos)) +
    geom_line(aes(group=group),colour="grey80") +
    geom_point(colour="white",size=8) +
    geom_text(aes(label=y), size=fontSize, family="American Typewriter") +
    scale_y_continuous(name="", breaks=yvals, labels=ylabs)
  return(gg)
}    

## Prepare data    
df <- tufte_sort(source_df, 
                 x="year", 
                 y="value", 
                 group="group", 
                 method="tufte", 
                 min.space=0.05)

df <- transform(df, 
                x=factor(x, levels=c(5,10,15,20), 
                         labels=c("5 years","10 years","15 years","20 years")), 
                y=round(y))

## Plot
plot_slopegraph(df) + labs(title="Estimates of % survival rates") + 
  theme(axis.title=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5,
                                  family = "American Typewriter",
                                  face="bold"),
        axis.text = element_text(family = "American Typewriter",
                                 face="bold"))


# 10) Seasonal Plot
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
if(!require(forecast)) install.packages("forecast");library(forecast)
theme_set(theme_classic())

# 부분집합 데이터
nottem_small <- window(nottem, start=c(1920, 1), end=c(1925, 12))  # 더 작은 timewindow의 부분집합

# Plot
ggseasonplot(AirPassengers) + labs(title="Seasonal plot: International Airline Passengers")
ggseasonplot(nottem_small) + labs(title="Seasonal plot: Air temperatures at Nottingham Castle")




# 7. Groups

# 1) Hierarchical Dendrogram
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
if(!require(ggdendro)) install.packages("ggdendro");library(ggdendro)
theme_set(theme_bw())

hc <- hclust(dist(USArrests), "ave")  # 계층적 군집분석

# plot
ggdendrogram(hc, rotate = TRUE, size = 2)


# 2) Clusters
if(!require(ggplot2)) install.packages("ggplot2");library(ggplot2)
if(!require(ggalt)) install.packages("ggalt");library(ggalt)
if(!require(ggfortify)) install.packages("ggfortify");library(ggfortify)
theme_set(theme_classic())

# 주성분으로 데이터 계산 ------------------
df <- iris[c(1, 2, 3, 4)]
pca_mod <- prcomp(df)  # 주성분을 계산

# 주요 구성 요소의 데이터 프레임 ----------------------
df_pc <- data.frame(pca_mod$x, Species=iris$Species)  # 주요 구성 요소의 데이터 프레임
df_pc_vir <- df_pc[df_pc$Species == "virginica", ]  # df_pc에서 Species가 Virginica인 관측값만 df_pc_vir에 저장
df_pc_set <- df_pc[df_pc$Species == "setosa", ]  # df_pc에서 Species가 Setosa인 관측값만 df_pc_set에 저장
df_pc_ver <- df_pc[df_pc$Species == "versicolor", ]  # df_pc에서 Species가 versicolor인 관측값만 df_pc_ver에 저장

# Plot ----------------------------------------------------
ggplot(df_pc, aes(PC1, PC2, col=Species)) + 
  geom_point(aes(shape=Species), size=2) +   # 점을 그려준다
  labs(title="Iris Clustering", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: Iris") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc$PC1), max(df_pc$PC1)), 
                  ylim = 1.2 * c(min(df_pc$PC2), max(df_pc$PC2))) +   # 축 한계 변경
  geom_encircle(data = df_pc_vir, aes(x=PC1, y=PC2)) +   # 원을 그린다.
  geom_encircle(data = df_pc_set, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = df_pc_ver, aes(x=PC1, y=PC2))
