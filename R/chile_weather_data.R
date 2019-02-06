#' Get historical records of extreme temperatures for Chile
#'
#' This function allows to obtain information about weather stations located in Chile as well as
#' a data frame containing historical records of minimum and maximum temperatures from those weather
#' stations. This function works with data downloaded from the website of the Center for Climate and
#' Resilience Research (CR)2 sponsored by the University of Chile. The function works for previously
#' downloaded data in ".zip" format. Data can be downloaded from the following links:
#' <http://www.cr2.cl/download/cr2_tasmindaily_2018_ghcn-zip/?wpdmdl=15125> for minimum temperatures and
#' <http://www.cr2.cl/download/cr2_tasmaxdaily_2018_ghcn-zip/?wpdmdl=15126> for maximum temperatures.
#' Function require both zip files in the same folder.
#'
#' @param data Character string input which define the kind of data wanted. There are three options
#' for this parameter. "info_stations" provides a dataframe with information about a given number
#' of weather stations (set in "Number_of_stations" parameter) located close to the ubication
#' established with "latitude" and "longitude" parameters. "station_list" provides a list of
#' dataframes containing minimum and maximum temperature records from each weather station
#' obtained with the "info_stations" option within the period established in the call of the function.
#' Finally, "my_weather" provides the first dataframe of the previous list and represent the
#' data of the closest weather station to the ubication established.
#' @param Initial_Date Character string input in the form "YYYY-MM-DD". This parameter represents the
#' initial date of the period of interest. If it is not provided, the default is established as
#' "1950-01-01" which correspond to the oldest date posible to use.
#' @param End_Date Character string input in the form "YYYY-MM-DD". This parameter represents the
#' final date of the period of interest. If it is not provided, the default is established as
#' "2017-12-31" which correspond to the earliest date posible to use.
#' @param latitude Numerical input. Latitude of the site of interest in decimal format.
#' @param longitude Numerical input. Longitude of the site of interest in decimal format.
#' @param Number_of_stations Numerical input. Number of stations wanted to use as closest stations to
#' the site of interest. Default option is the value 25.
#' @param path_zip_tmin Character string input. Ubication of the zip file containing minimum
#' temperatures. This input must include the name and extension of the file.
#' @param path_zip_tmax Character string input. Ubication of the zip file containing maximum
#' temperatures. This input must include the name and extension of the file.
#'
#' @examples
#' #Getting the pubication of zip files 
#' path_zip_tmin <- "cr2_tasminDaily_2018_ghcn.zip" #these should be on your machine
#' path_zip_tmax <- "cr2_tasmaxDaily_2018_ghcn.zip"
#'
#' #Call of the function
#' chile_weather(data="my_weather", Initial_Date="2000-01-01", End_Date="2017-12-31",latitude=-32.8958,
#'               longitude=-71.2092, Number_of_stations=25, path_zip_tmin=path_zip_tmin,path_zip_tmax=path_zip_tmax)
#' 
#' @export chillscatter
chile_weather <- function(data, latitude, longitude, path_zip_tmin, path_zip_tmax, 
                Initial_Date = "1950-01-01", End_Date = "2017-12-31", 
                Number_of_stations = 25){
  
  actual_WD<-getwd()
  setwd(paste(substr(path_zip_tmin,1,nchar(path_zip_tmin)-30)))
  
  path_tmin<-unzip(path_zip_tmin,list = T)
  path_tmax<-unzip(path_zip_tmax,list = T)
  
  unzip(path_zip_tmin,exdir = paste(substr(path_zip_tmin,1,nchar(path_zip_tmin)-30)) )
  unzip(path_zip_tmax,exdir = paste(substr(path_zip_tmax,1,nchar(path_zip_tmax)-30)))
  
  Stations<-read.table(as.character(path_tmin[3,1]),sep = ",", header = T,quote = "\"")
  Stations<-subset(Stations,select = c(1,2,3,4,5,6,7))
  Cod_stations<-as.character(Cod_stations<-Stations$codigo_estacion)
  
  
  Cod_Stations=NULL
  for (i in 1:length(Cod_stations)) {
    tempcod<-paste("Cod","_",Cod_stations[[i]],sep="")
    Cod_Stations<-c(Cod_Stations,tempcod)
  }
  
  Stations$codigo_estacion<-Cod_Stations
  Cod_Stations<-c("Fecha",Cod_Stations)
  
  Tmin<-read.table(as.character(path_tmin[2,1]),sep = ",",quote = "",skip=15,blank.lines.skip = F,
                   col.names = Cod_Stations,na.strings = c("-9999","-9999.000"))
  Tmin$Fecha<-as.Date(Tmin$Fecha)
  Tmin<-subset(Tmin, subset = Fecha >= "1950-01-01" & Fecha <="2017-12-31")
  
  Stations_Max<-read.table(as.character(path_tmax[3,1]),sep = ",", header = T,quote = "\"")
  Stations_Max<-subset(Stations_Max,select = c(1,2,3,4,5,6,7))
  Cod_Stations_Max<-as.character(Cod_Stations_Max<-Stations_Max$codigo_estacion)
  
  Cod_stations_Max=NULL
  for (i in 1:length(Cod_Stations_Max)) {
    tempcod<-paste("Cod","_",Cod_Stations_Max[[i]],sep="")
    Cod_stations_Max<-c(Cod_stations_Max,tempcod)
  }
  
  Cod_stations_Max <- c("Fecha",Cod_stations_Max)
  
  Tmax<-read.table(as.character(path_tmax[2,1]),sep = ",",quote = "",skip=15,blank.lines.skip = F,
                   col.names = Cod_stations_Max,na.strings = c("-9999","-9999.000"))
  Tmax$Fecha<-as.Date(Tmax$Fecha)
  Tmax<-subset(Tmax, subset = Fecha >= "1950-01-01" & Fecha <="2017-12-31")
  
  setwd(actual_WD)
  
  mypoint<-c(longitude,latitude)
  Stations[,"distancia"] <- round(sp::spDistsN1(as.matrix(Stations[,c("longitud","latitud")]),mypoint,longlat = T),2)
  Sorted_Stations<-Stations[order(Stations$distancia),]
  Sumarized_stations<-Sorted_Stations[c(1:Number_of_stations),]
  
  
  Year<-c(rep(1950,365),rep(1951,365),rep(1952,366),rep(1953,365),rep(1954,365),rep(1955,365),rep(1956,366),
          rep(1957,365),rep(1958,365),rep(1959,365),rep(1960,366),rep(1961,365),rep(1962,365),rep(1963,365),
          rep(1964,366),rep(1965,365),rep(1966,365),rep(1967,365),rep(1968,366),rep(1969,365),rep(1970,365),
          rep(1971,365),rep(1972,366),rep(1973,365),rep(1974,365),rep(1975,365),rep(1976,366),rep(1977,365),
          rep(1978,365),rep(1979,365),rep(1980,366),rep(1981,365),rep(1982,365),rep(1983,365),rep(1984,366),
          rep(1985,365),rep(1986,365),rep(1987,365),rep(1988,366),rep(1989,365),rep(1990,365),rep(1991,365),
          rep(1992,366),rep(1993,365),rep(1994,365),rep(1995,365),rep(1996,366),rep(1997,365),rep(1998,365),
          rep(1999,365),rep(2000,366),rep(2001,365),rep(2002,365),rep(2003,365),rep(2004,366),rep(2005,365),
          rep(2006,365),rep(2007,365),rep(2008,366),rep(2009,365),rep(2010,365),rep(2011,365),rep(2012,366),
          rep(2013,365),rep(2014,365),rep(2015,365),rep(2016,366),rep(2017,365))
  
  Month <- c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),
           rep(10,31),rep(11,30),rep(12,31))
  L_Month <- c(rep(1,31),rep(2,29),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),
             rep(10,31),rep(11,30),rep(12,31))
  Months <- c(Month,Month,L_Month,Month,Month,Month,L_Month,Month,Month,Month,L_Month,Month,Month,Month,L_Month,Month,
            Month,Month,L_Month,Month,Month,Month,L_Month,Month,Month,Month,L_Month,Month,Month,Month,L_Month,Month,
            Month,Month,L_Month,Month,Month,Month,L_Month,Month,Month,Month,L_Month,Month,Month,Month,L_Month,Month,
            Month,Month,L_Month,Month,Month,Month,L_Month,Month,Month,Month,L_Month,Month,Month,Month,L_Month,Month,
            Month,Month,L_Month,Month)
  Day <- c(seq(1:31),seq(1:28),seq(1:31),seq(1:30),seq(1:31),seq(1:30),seq(1:31),seq(1:31),seq(1:30),seq(1:31),seq(1:30),
         seq(1:31))
  L_Day <- c(seq(1:31),seq(1:29),seq(1:31),seq(1:30),seq(1:31),seq(1:30),seq(1:31),seq(1:31),seq(1:30),seq(1:31),seq(1:30),
           seq(1:31))
  Days <- c(Day,Day,L_Day,Day,Day,Day,L_Day,Day,Day,Day,L_Day,Day,Day,Day,L_Day,Day,Day,Day,L_Day,Day,Day,Day,L_Day,Day,
          Day,Day,L_Day,Day,Day,Day,L_Day,Day,Day,Day,L_Day,Day,Day,Day,L_Day,Day,Day,Day,L_Day,Day,Day,Day,L_Day,Day,
          Day,Day,L_Day,Day,Day,Day,L_Day,Day,Day,Day,L_Day,Day,Day,Day,L_Day,Day,Day,Day,L_Day,Day)
  
  dfs<-NULL
  for (i in 1:length(Sumarized_stations$codigo_estacion)) {
    df<-data.frame(Date=Tmax$Fecha,Year=Year,Month=Months,Day=Days,Tmin=Tmin[,Sumarized_stations[i,1]],
                   Tmax=Tmax[,Sumarized_stations[i,1]])
    df<-subset(df, subset = Date >= as.Date(Initial_Date) & Date <= as.Date(End_Date))
    df<-list(subset(df, select = -Date))
    dfs<-c(dfs,df)
  }
  
  dfs2<-NULL
  for (i in 1:length(dfs)) {
    df2<-list(data.frame(Weather_Station=rep(Sumarized_stations[[i,4]],length(dfs[[i]][,1])),dfs[[i]]))
    dfs2<-c(dfs2,df2)
  }
  
  
  if(data == "station_list")
    return(dfs2)
  
  N_obs<-NULL
  for (i in 1:length(dfs)) {
    if (table(is.na(dfs[[i]][,c("Tmin","Tmax")]))[[1]] != length(dfs[[i]][,1])*2){
      NobsTemp<-table(is.na(dfs[[i]][,c("Tmin","Tmax")]))[[1]]/2
    }else{
      NobsTemp<-0
    }
    
    N_obs<-c(N_obs,NobsTemp)
  }
  
  Sumarized_stations[,"N_Obs"]<-N_obs
  
  Porc_completo<-NULL
  for (i in 1:length(dfs)) {
    if (table(is.na(dfs[[i]][,c("Tmin","Tmax")]))[[1]] != length(dfs[[i]][,1])*2){
      Per_Comp_Temp<-round(((table(is.na(dfs[[i]][,c("Tmin","Tmax")]))[[1]]/
                               (table(is.na(dfs[[i]][,c("Tmin","Tmax")]))[[1]]+
                                  table(is.na(dfs[[i]][,c("Tmin","Tmax")]))[[2]]))*100),1)
    }else{
      Per_Comp_Temp<-0
    }
    
    Porc_completo<-c(Porc_completo,Per_Comp_Temp)
  }
  
  Sumarized_stations[,"Porc_Completo"]<-Porc_completo
  if(data == "info_stations")
    return(Sumarized_stations)
  
  my_weather<-dfs2[[1]]
  if(data == "my_weather")
    return(my_weather)
}
