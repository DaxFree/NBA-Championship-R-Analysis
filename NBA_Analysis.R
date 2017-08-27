# Dax Freeman
# May 2017
# Senior Capstone
# NBA Championship Teams 80'-16' Basic R Analysis
library(readr)
install.packages("ggplot2")
library(ggplot2)

# Bulls
Bulls1993 <- read_csv("~/Desktop/CapstoneCSV/Bulls1993.csv")
Bulls91 <- read_csv("~/Desktop/CapstoneCSV/Bulls91.csv")
Bulls92 <- read_csv("~/Desktop/CapstoneCSV/Bulls92.csv")
Bulls96 <- read_csv("~/Desktop/CapstoneCSV/Bulls96.csv")
Bulls97 <- read_csv("~/Desktop/CapstoneCSV/Bulls97.csv")
Bulls98 <- read_csv("~/Desktop/CapstoneCSV/Bulls98.csv")

# Cavaliers
Cavaliers16 <- read_csv("~/Desktop/CapstoneCSV/Cavaliers16.csv")

# Celtics
Celtics81 <- read_csv("~/Desktop/CapstoneCSV/Celtics81.csv")
Celtics84 <- read_csv("~/Desktop/CapstoneCSV/Celtics84.csv")
Celtics86 <- read_csv("~/Desktop/CapstoneCSV/Celtics86.csv")
Celtics08 <- read_csv("~/Desktop/CapstoneCSV/Celtics08.csv")

# Golden State
Goldenstate15 <- read_csv("~/Desktop/CapstoneCSV/Goldenstate15.csv")

# Miami Heat
Heat06 <- read_csv("~/Desktop/CapstoneCSV/Heat06.csv")
Heat12 <- read_csv("~/Desktop/CapstoneCSV/Heat12.csv")
Heat13 <- read_csv("~/Desktop/CapstoneCSV/Heat13.csv")

# LA Lakers
Lakers_80 <- read_csv("~/Desktop/CapstoneCSV/Lakers-80.csv")
Lakers82 <- read_csv("~/Desktop/CapstoneCSV/Lakers82.csv")
Lakers85 <- read_csv("~/Desktop/CapstoneCSV/Lakers85.csv")
Lakers87 <- read_csv("~/Desktop/CapstoneCSV/Lakers87.csv")
Lakers88 <- read_csv("~/Desktop/CapstoneCSV/Lakers88.csv")
Lakers00 <- read_csv("~/Desktop/CapstoneCSV/Lakers00.csv")
Lakers01 <- read_csv("~/Desktop/CapstoneCSV/Lakers01.csv")
Lakers02 <- read_csv("~/Desktop/CapstoneCSV/Lakers02.csv")
Lakers09 <- read_csv("~/Desktop/CapstoneCSV/Lakers09.csv")
Lakers10 <- read_csv("~/Desktop/CapstoneCSV/Lakers10.csv")

# Mavericks
Mavericks11 <- read_csv("~/Desktop/CapstoneCSV/Mavericks11.csv")

# Detroit Pistons
Pistons89 <- read_csv("~/Desktop/CapstoneCSV/Pistons89.csv")
Pistons90 <- read_csv("~/Desktop/CapstoneCSV/Pistons90.csv")
Pistons04 <- read_csv("~/Desktop/CapstoneCSV/Pistons04.csv")

# Houston Rockets
Rockets94 <- read_csv("~/Desktop/CapstoneCSV/Rockets94.csv")

# San Antonio Spurs
Spurs99 <- read_csv("~/Desktop/CapstoneCSV/Spurs99.csv")
Spurs03 <- read_csv("~/Desktop/CapstoneCSV/Spurs03.csv")
Spurs05 <- read_csv("~/Desktop/CapstoneCSV/Spurs05.csv")
Spurs07 <- read_csv("~/Desktop/CapstoneCSV/Spurs07.csv")
Spurs14 <- read_csv("~/Desktop/CapstoneCSV/Spurs14.csv")

# 80'-16' Team  totals
AllTeams <- read_csv("~/Desktop/CapstoneCSV/AllTeams.csv")

AllTeams.na <- na.omit(AllTeams)
plot(Age ~ PTS, data = AllTeams)
plot(Age ~ PTS, pch=19, col=c(rep("#fa0003", 75), rep("yellow", 93), rep("green", 145), rep("#00d2fa", 160) , rep("#9b081d", 211), rep("#919900", 356), rep("black", 374), rep("orange", 422), rep("black", 437), rep("black", 518)), data = AllTeams)


Bulls1991 <- na.omit(Bulls91)
mean(Bulls1991$Age)
mean(Bulls1991$'3P%')
cor(Bulls1991$'3P%', Bulls1991$'2P%' )
Bulls1991.lm = lm(Bulls1991$'3P%' ~ Bulls1991$'2P%', data = Bulls1991)
Bulls1991.lm.2 = lm(Bulls1991$'3P%' ~ Bulls1991$'2P%' + Bulls1991$'FT%', data = Bulls1991)
summary(Bulls1991.lm.2)

summary(Bulls1991.lm)
summary(Bulls1991.lm.2)
plot(PTS ~ Age, data=Bulls1991)


Bulls1992 <- na.omit(Bulls92)
mean(Bulls1992$Age)
mean(Bulls1992$'3P%')
cor(Bulls1992$'3P%', Bulls1992$'2P%' )
Bulls1992.lm = lm(Bulls1992$'3P%' ~ Bulls1992$'2P%', data = Bulls1992)
summary(Bulls1992.lm)

Bulls1992.lm.2 = lm(Bulls1992$'3P%' ~ Bulls1992$'2P%' + Bulls1992$'FT%', data = Bulls1992)
summary(Bulls1992.lm.2)

plot(PTS ~ Age, data=Bulls1992)


Bulls1993 <- na.omit(Bulls1993)
mean(Bulls1993$Age)
mean(Bulls1993$'3P%')
cor(Bulls1993$'3P%', Bulls1993$'2P%' )
Bulls1993.lm = lm(Bulls1993$'3P%' ~ Bulls1993$'2P%', data = Bulls1993)
summary(Bulls1993.lm)

Bulls1993.lm.2 = lm(Bulls1993$'3P%' ~ Bulls1993$'2P%' + Bulls1993$'FT%', data = Bulls1993)
summary(Bulls1993.lm.2)

plot(PTS ~ Age, data=Bulls1993)


Bulls1996 <- na.omit(Bulls96)
mean(Bulls1996$Age)
mean(Bulls1996$'3P%')
cor(Bulls1996$'3P%', Bulls1996$'2P%' )
Bulls1996.lm = lm(Bulls1996$'3P%' ~ Bulls1996$'2P%', data = Bulls1996)
summary(Bulls1996.lm)

Bulls1996.lm.2 = lm(Bulls1996$'3P%' ~ Bulls1996$'2P%' + Bulls1996$'FT%', data = Bulls1996)
summary(Bulls1996.lm.2)


plot(PTS ~ Age, data=Bulls1996)


Bulls1997 <- na.omit(Bulls97)
mean(Bulls1997$Age)
mean(Bulls1997$'3P%')
cor(Bulls1997$'3P%', Bulls1997$'2P%' )
Bulls1997.lm = lm(Bulls1997$'3P%' ~ Bulls1997$'2P%', data = Bulls1997)
summary(Bulls1997.lm)

Bulls1997.lm.2 = lm(Bulls1997$'3P%' ~ Bulls1997$'2P%' + Bulls1997$'FT%', data = Bulls1997)
summary(Bulls1997.lm.2)


plot(PTS ~ Age, data=Bulls1997, pch = 19)


Bulls1998 <- na.omit(Bulls98)
mean(Bulls1998$Age)
mean(Bulls1998$'3P%')
cor(Bulls1998$'3P%', Bulls1998$'2P%' )
Bulls1998.lm = lm(Bulls1998$'3P%' ~ Bulls1998$'2P%', data = Bulls1998)
summary(Bulls1998.lm)

Bulls1998.lm.2 = lm(Bulls1998$'3P%' ~ Bulls1998$'2P%' + Bulls1998$'FT%', data = Bulls1998)
summary(Bulls1998.lm.2)


plot(PTS ~ Age, data=Bulls1998)


Cavaliers2016 <- na.omit(Cavaliers16)
mean(Cavaliers2016$Age)
mean(Cavaliers2016$'3P%')
cor(Cavaliers2016$'3P%', Cavaliers2016$'2P%' )
Cavaliers2016.lm = lm(Cavaliers2016$'3P%' ~ Cavaliers2016$'2P%', data = Cavaliers2016)
summary(Cavaliers2016.lm)

Cavaliers2016.lm.2 = lm(Cavaliers2016$'3P%' ~ Cavaliers2016$'2P%' + Cavaliers2016$'FT%', data = Cavaliers2016)
summary(Cavaliers2016.lm.2)


plot(PTS ~ Age, data=Cavaliers2016)


Celtics1981 <- na.omit(Celtics81)
mean(Celtics1981$Age)
mean(Celtics1981$'3P%')
cor(Celtics1981$'3P%', Celtics1981$'2P%' )
Celtics1981.lm = lm(Celtics1981$'3P%' ~ Celtics1981$'2P%', data = Celtics1981)
summary(Celtics1981.lm)

Celtics1981.lm.2 = lm(Celtics1981$'3P%' ~ Celtics1981$'2P%' + Celtics1981$'FT%', data = Celtics1981)
summary(Celtics1981.lm.2)


plot(PTS ~ Age, data=Celtics1981)


Celtics1984 <- na.omit(Celtics84)
mean(Celtics1984$Age)
mean(Celtics1984$'3P%')
cor(Celtics1984$'3P%', Celtics1984$'2P%' )
Celtics1984.lm = lm(Celtics1984$'3P%' ~ Celtics1984$'2P%', data = Celtics1984)
summary(Celtics1984.lm)

Celtics1984.lm.2 = lm(Celtics1984$'3P%' ~ Celtics1984$'2P%' + Celtics1984$'FT%', data = Celtics1984)
summary(Celtics1984.lm.2)


plot(PTS ~ Age, data=Celtics1984)


Celtics1986 <- na.omit(Celtics86)
mean(Celtics1986$Age)
mean(Celtics1986$'3P%')
cor(Celtics1986$'3P%', Celtics1986$'2P%' )
Celtics1986.lm = lm(Celtics1986$'3P%' ~ Celtics1986$'2P%', data = Celtics1986)
summary(Celtics1986.lm)

Celtics1986.lm.2 = lm(Celtics1986$'3P%' ~ Celtics1986$'2P%' + Celtics1986$'FT%', data = Celtics1986)
summary(Celtics1986.lm.2)


plot(PTS ~ Age, data=Celtics1986)


Celtics2008 <- na.omit(Celtics08)
mean(Celtics2008$Age)
mean(Celtics2008$'3P%')
cor(Celtics2008$'3P%', Celtics2008$'2P%' )
Celtics2008.lm = lm(Celtics2008$'3P%' ~ Celtics2008$'2P%', data = Celtics2008)
summary(Celtics2008.lm)

Celtics2008.lm.2 = lm(Celtics2008$'3P%' ~ Celtics2008$'2P%' + Celtics2008$'FT%', data = Celtics2008)
summary(Celtics2008.lm.2)


plot(PTS ~ Age, data=Celtics2008)


Goldenstate2015 <- na.omit(Goldenstate15)
mean(Goldenstate2015$Age)
mean(Goldenstate2015$'3P%')
cor(Goldenstate2015$'3P%', Goldenstate2015$'2P%' )
Goldenstate2015.lm = lm(Goldenstate2015$'3P%' ~ Goldenstate2015$'2P%', data = Goldenstate2015)
summary(Goldenstate2015.lm)

Goldenstate2015.lm.2 = lm(Goldenstate2015$'3P%' ~ Goldenstate2015$'2P%' + Goldenstate2015$'FT%', data = Goldenstate2015)
summary(Goldenstate2015.lm.2)

plot(PTS ~ Age, data=Goldenstate2015)


Heat2006 <- na.omit(Heat06)
mean(Heat2006$Age)
mean(Heat2006$'3P%')
cor(Heat2006$'3P%', Heat2006$'2P%' )
Heat2006.lm = lm(Heat2006$'3P%' ~ Heat2006$'2P%', data = Heat2006)
summary(Heat2006.lm)

Heat2006.lm.2 = lm(Heat2006$'3P%' ~ Heat2006$'2P%' + Heat2006$'FT%', data = Heat2006)
summary(Heat2006.lm.2)


plot(PTS ~ Age, data=Heat2006)


Heat2012 <- na.omit(Heat12)
mean(Heat2012$Age)
mean(Heat2012$'3P%')
cor(Heat2012$'3P%', Heat2012$'2P%' )
Heat2012.lm = lm(Heat2012$'3P%' ~ Heat2012$'2P%', data = Heat2012)
summary(Heat2012.lm)

Heat2012.lm.2 = lm(Heat2012$'3P%' ~ Heat2012$'2P%' + Heat2012$'FT%', data = Heat2012)
summary(Heat2012.lm.2)


plot(PTS ~ Age, data=Heat2012)


Heat2013 <- na.omit(Heat13)
mean(Heat2013$Age)
mean(Heat2013$'3P%')
cor(Heat2013$'3P%', Heat2013$'2P%' )
Heat2013.lm = lm(Heat2013$'3P%' ~ Heat2013$'2P%', data = Heat2013)
summary(Heat2013.lm)

Heat2013.lm.2 = lm(Heat2013$'3P%' ~ Heat2013$'2P%' + Heat2013$'FT%', data = Heat2013)
summary(Heat2013.lm.2)


plot(PTS ~ Age, data=Heat2013)


Lakers1980 <- na.omit(Lakers_80)
mean(Lakers1980$Age)
mean(Lakers1980$'3P%')
cor(Lakers1980$'3P%', Lakers1980$'2P%' )
Lakers1980.lm = lm(Lakers1980$'3P%' ~ Lakers1980$'2P%', data = Lakers1980)
summary(Lakers1980.lm)

Lakers1980.lm.2 = lm(Lakers1980$'3P%' ~ Lakers1980$'2P%' + Lakers1980$'FT%', data = Lakers1980)
summary(Lakers1980.lm.2)

plot(PTS ~ Age, data=Lakers1980)


Lakers1982 <- na.omit(Lakers82)
mean(Lakers1982$Age)
mean(Lakers1982$'3P%')
cor(Lakers1982$'3P%', Lakers1982$'2P%' )
Lakers1982.lm = lm(Lakers1982$'3P%' ~ Lakers1982$'2P%', data = Lakers1982)
summary(Lakers1982.lm)

Lakers1982.lm.2 = lm(Lakers1982$'3P%' ~ Lakers1982$'2P%' + Lakers1982$'FT%', data = Lakers1982)
summary(Lakers1982.lm.2)

plot(PTS ~ Age, data=Lakers1982)


Lakers1985 <- na.omit(Lakers85)
mean(Lakers1985$Age)
mean(Lakers1985$'3P%')
cor(Lakers1985$'3P%', Lakers1985$'2P%' )
Lakers1985.lm = lm(Lakers1985$'3P%' ~ Lakers1985$'2P%', data = Lakers1985)
summary(Lakers1985.lm)

Lakers1985.lm.2 = lm(Lakers1985$'3P%' ~ Lakers1985$'2P%' + Lakers1985$'FT%', data = Lakers1985)
summary(Lakers1985.lm.2)

plot(PTS ~ Age, data=Lakers1985)


Lakers1987 <- na.omit(Lakers87)
mean(Lakers1987$Age)
mean(Lakers1987$'3P%')
cor(Lakers1987$'3P%', Lakers1987$'2P%' )
Lakers1987.lm = lm(Lakers1987$'3P%' ~ Lakers1987$'2P%', data = Lakers1987)
summary(Lakers1987.lm)

Lakers1987.lm.2 = lm(Lakers1987$'3P%' ~ Lakers1987$'2P%' + Lakers1987$'FT%', data = Lakers1987)
summary(Lakers1987.lm.2)

plot(PTS ~ Age, data=Lakers1987)


Lakers1988 <- na.omit(Lakers88)
mean(Lakers1988$Age)
mean(Lakers1988$'3P%')
cor(Lakers1988$'3P%', Lakers1988$'2P%' )
Lakers1988.lm = lm(Lakers1988$'3P%' ~ Lakers1988$'2P%', data = Lakers1988)
summary(Lakers1988.lm)

Lakers1988.lm.2 = lm(Lakers1988$'3P%' ~ Lakers1988$'2P%' + Lakers1988$'FT%', data = Lakers1988)
summary(Lakers1988.lm.2)

plot(PTS ~ Age, data=Lakers1988)


Lakers2000 <- na.omit(Lakers00)
mean(Lakers2000$Age)
mean(Lakers2000$'3P%')
cor(Lakers2000$'3P%', Lakers2000$'2P%' )
Lakers2000.lm = lm(Lakers2000$'3P%' ~ Lakers2000$'2P%', data = Lakers2000)
summary(Lakers2000.lm)

Lakers2000.lm.2 = lm(Lakers2000$'3P%' ~ Lakers2000$'2P%' + Lakers2000$'FT%', data = Lakers2000)
summary(Lakers2000.lm.2)

plot(PTS ~ Age, data=Lakers2000)


Lakers2001 <- na.omit(Lakers01)
mean(Lakers2001$Age)
mean(Lakers2001$'3P%')
cor(Lakers2001$'3P%', Lakers2001$'2P%' )
Lakers2001.lm = lm(Lakers2001$'3P%' ~ Lakers2001$'2P%', data = Lakers2001)
summary(Lakers2001.lm)

Lakers2001.lm.2 = lm(Lakers2001$'3P%' ~ Lakers2001$'2P%' + Lakers2001$'FT%', data = Lakers2001)
summary(Lakers2001.lm.2)

plot(PTS ~ Age, data=Lakers2001)


Lakers2002 <- na.omit(Lakers02)
mean(Lakers2002$Age)
mean(Lakers2002$'3P%')
cor(Lakers2002$'3P%', Lakers2002$'2P%' )
Lakers2002.lm = lm(Lakers2002$'3P%' ~ Lakers2002$'2P%', data = Lakers2002)
summary(Lakers2002.lm)

Lakers2002.lm.2 = lm(Lakers2002$'3P%' ~ Lakers2002$'2P%' + Lakers2002$'FT%', data = Lakers2002)
summary(Lakers2002.lm.2)

plot(PTS ~ Age, data=Lakers2002)


Lakers2009 <- na.omit(Lakers09)
mean(Lakers2009$Age)
mean(Lakers2009$'3P%')
cor(Lakers2009$'3P%', Lakers2009$'2P%' )
Lakers2009.lm = lm(Lakers2009$'3P%' ~ Lakers2009$'2P%', data = Lakers2009)
summary(Lakers2009.lm)

Lakers2009.lm.2 = lm(Lakers2009$'3P%' ~ Lakers2009$'2P%' + Lakers2009$'FT%', data = Lakers2009)
summary(Lakers2009.lm.2)

plot(PTS ~ Age, data=Lakers2009)


Lakers2010 <- na.omit(Lakers10)
mean(Lakers2010$Age)
mean(Lakers2010$'3P%')
cor(Lakers2010 $'3P%', Lakers2010 $'2P%' )
Lakers2010.lm = lm(Lakers2010$'3P%' ~ Lakers2010 $'2P%', data = Lakers2010 )
summary(Lakers2010.lm)

Lakers2010.lm.2 = lm(Lakers2010$'3P%' ~ Lakers2010$'2P%' + Lakers2010$'FT%', data = Lakers2010)
summary(Lakers2010.lm.2)

plot(PTS ~ Age, data=Lakers2010)


Mavericks2011 <- na.omit(Mavericks11)
mean(Mavericks2011$Age)
mean(Mavericks2011$'3P%')
cor(Mavericks2011$'3P%', Mavericks2011$'2P%' )
Mavericks2011.lm = lm(Mavericks2011$'3P%' ~ Mavericks2011$'2P%', data = Mavericks2011)
summary(Mavericks2011.lm)

Mavericks2011.lm.2 = lm(Mavericks2011$'3P%' ~ Mavericks2011$'2P%' + Mavericks2011$'FT%', data = Mavericks2011)
summary(Mavericks2011.lm.2)

plot(PTS ~ Age, data=Mavericks2011)


Pistons1989 <- na.omit(Pistons89)
mean(Pistons1989$Age)
mean(Pistons1989$'3P%')
cor(Pistons1989$'3P%', Pistons1989$'2P%' )
Pistons1989.lm = lm(Pistons1989$'3P%' ~ Pistons1989$'2P%', data = Pistons1989)
summary(Pistons1989.lm)

Pistons1989.lm.2 = lm(Pistons1989$'3P%' ~ Pistons1989$'2P%' + Pistons1989$'FT%', data = Pistons1989)
summary(Pistons1989.lm.2)

plot(PTS ~ Age, data=Pistons1989)


Pistons1990 <- na.omit(Pistons90)
mean(Pistons1990$Age)
mean(Pistons1990$'3P%')
cor(Pistons1990$'3P%', Pistons1990$'2P%' )
Pistons1990.lm = lm(Pistons1990$'3P%' ~ Pistons1990 $'2P%', data = Pistons1990 )
summary(Pistons1990.lm)

Pistons1990.lm.2 = lm(Pistons1990$'3P%' ~ Pistons1990$'2P%' + Pistons1990$'FT%', data = Pistons1990)
summary(Pistons1990.lm.2)

plot(PTS ~ Age, data=Pistons1990 )


Pistons2004 <- na.omit(Pistons04)
mean(Pistons2004$Age)
mean(Pistons2004$'3P%')
cor(Pistons2004$'3P%', Pistons2004$'2P%' )
Pistons2004.lm = lm(Pistons2004$'3P%' ~ Pistons2004$'2P%', data = Pistons2004)
summary(Pistons2004.lm)

Pistons2004.lm.2 = lm(Pistons2004$'3P%' ~ Pistons2004$'2P%' + Pistons2004$'FT%', data = Pistons2004)
summary(Pistons2004.lm.2)

plot(PTS ~ Age, data=Pistons2004)


Rockets1994 <- na.omit(Rockets94)
mean(Rockets1994$Age)
mean(Rockets1994$'3P%')
cor(Rockets1994$'3P%', Rockets1994$'2P%' )
Rockets1994.lm = lm(Rockets1994$'3P%' ~ Rockets1994$'2P%', data = Rockets1994)
summary(Rockets1994.lm)

Rockets1994.lm.2 = lm(Rockets1994$'3P%' ~ Rockets1994$'2P%' + Rockets1994$'FT%', data = Rockets1994)
summary(Rockets1994.lm.2)

plot(PTS ~ Age, data=Rockets1994)


Spurs1999 <- na.omit(Spurs99)
mean(Spurs1999$Age)
mean(Spurs1999$'3P%')
cor(Spurs1999$'3P%', Spurs1999$'2P%' )
Spurs1999.lm = lm(Spurs1999$'3P%' ~ Spurs1999$'2P%', data = Spurs1999)
summary(Spurs1999.lm)

Spurs1999.lm.2 = lm(Spurs1999$'3P%' ~ Spurs1999$'2P%' + Spurs1999$'FT%', data = Spurs1999)
summary(Spurs1999.lm.2)

plot(PTS ~ Age, data=Spurs1999)


Spurs2003 <- na.omit(Spurs03)
mean(Spurs2003$Age)
mean(Spurs2003$'3P%')
cor(Spurs2003$'3P%', Spurs2003$'2P%' )
Spurs2003.lm = lm(Spurs2003$'3P%' ~ Spurs2003$'2P%', data = Spurs2003)
summary(Spurs2003.lm)

Spurs2003.lm.2 = lm(Spurs2003$'3P%' ~ Spurs2003$'2P%' + Spurs2003$'FT%', data = Spurs2003)
summary(Spurs2003.lm.2)

plot(PTS ~ Age, data=Spurs2003)


Spurs2005 <- na.omit(Spurs05)
mean(Spurs2005$Age)
mean(Spurs2005$'3P%')
cor(Spurs2005$'3P%', Spurs2005$'2P%' )
Spurs2005.lm = lm(Spurs2005$'3P%' ~ Spurs2005$'2P%', data = Spurs2005)
summary(Spurs2005.lm)

Spurs2005.lm.2 = lm(Spurs2005$'3P%' ~ Spurs2005$'2P%' + Spurs2005$'FT%', data = Spurs2005)
summary(Spurs2005.lm.2)

plot(PTS ~ Age, data=Spurs2005)


Spurs2007 <- na.omit(Spurs07)
mean(Spurs2007$Age)
mean(Spurs2007$'3P%')
cor(Spurs2007$'3P%', Spurs2007$'2P%' )
Spurs2007.lm = lm(Spurs2007$'3P%' ~ Spurs2007$'2P%', data = Spurs2007)
summary(Spurs2007.lm)

Spurs2007.lm.2 = lm(Spurs2007$'3P%' ~ Spurs2007$'2P%' + Spurs2007$'FT%', data = Spurs2007)
summary(Spurs2007.lm.2)

plot(PTS ~ Age, data=Spurs2007)


Spurs2014 <- na.omit(Spurs14)
mean(Spurs2014$Age)
cor(Spurs2014$'3P%', Spurs2014$'2P%' )
Spurs2014.lm = lm(Spurs2014$'3P%' ~ Spurs2014$'2P%', data = Spurs2014)
summary(Spurs2014.lm)

Spurs2014.lm.2 = lm(Spurs2014$'3P%' ~ Spurs2014$'2P%' + Spurs2014$'FT%', data = Spurs2014)
summary(Spurs2014.lm.2)

plot(PTS ~ Age, data=Spurs2014)
