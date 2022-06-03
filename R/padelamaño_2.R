padelamaño_2<-function(g1=NA,g2=NA,g3=NA,p1=NA,p2=NA,p3=NA, ausencia=FALSE,empate=FALSE,fecha=NA,premio=0,castigo=0,especial=FALSE){

  jugadores_v<-c(g1,g2,g3,p1,p2,p3)
  puntos_ant<-c(0,0,0,0,0,0)
  jugados_ant<-c(0,0,0,0,0,0)
  ganados_ant<-c(0,0,0,0,0,0)
  perdidos_ant<-c(0,0,0,0,0,0)
  empatados_ant<-c(0,0,0,0,0,0)
  i<-1
  for (val in jugadores_v) {


    puntos_ant[i]<-ifelse(is.na(val),NA,as.numeric(tabla%>%
                                                     filter(jugador==val)%>%
                                                     filter(jugados==max(jugados))%>%
                                                     summarize(puntos)))

    jugados_ant[i]<-ifelse(is.na(val),NA,as.numeric(tabla%>%
                                                      filter(jugador==val)%>%
                                                      filter(jugados==max(jugados))%>%
                                                      summarize(jugados)))

    ganados_ant[i]<-ifelse(is.na(val),NA,as.numeric(tabla%>%
                                                      filter(jugador==val)%>%
                                                      filter(jugados==max(jugados))%>%
                                                      summarize(ganados)))

    perdidos_ant[i]<-ifelse(is.na(val),NA,as.numeric(tabla%>%
                                                       filter(jugador==val)%>%
                                                       filter(jugados==max(jugados))%>%
                                                       summarize(perdidos)))

    empatados_ant[i]<-ifelse(is.na(val),NA,as.numeric(tabla%>%
                                                        filter(jugador==val)%>%
                                                        filter(jugados==max(jugados))%>%
                                                        summarize(empatados)))
    i<-i+1
  }



  promedio_ganador<-mean(c(puntos_ant[1:3]),na.rm=TRUE)
  promedio_perdedor<-mean(c(puntos_ant[4:6]),na.rm=TRUE)


  max_partido<-as.numeric(partidos%>%
                            summarize(ifelse(length(partidos$num_partido)==0,0,max(num_partido))))

  if (empate==TRUE){
    tabla<<-rbind(tabla,tibble(jugador=g1,puntos=puntos_ant[1]+40*(.5-1/(1+10**((promedio_perdedor-promedio_ganador)/400))),jugados=jugados_ant[1]+1,ganados=ganados_ant[1],empatados=empatados_ant[1]+1,perdidos=perdidos_ant[1],fecha=ymd(fecha)))
    ifelse(is.na(g2),nada<-"nada",tabla<<-rbind(tabla,tibble(jugador=g2,puntos=puntos_ant[2]+40*(.5-1/(1+10**((promedio_perdedor-promedio_ganador)/400))),jugados=jugados_ant[2]+1,ganados=ganados_ant[2],empatados=empatados_ant[2]+1,perdidos=perdidos_ant[2],fecha=ymd(fecha))))
    tabla<<-rbind(tabla,tibble(jugador=p1,puntos=puntos_ant[4]+40*(.5-1/(1+10**((promedio_ganador-promedio_perdedor)/400))),jugados=jugados_ant[4]+1,ganados=ganados_ant[4],empatados=empatados_ant[4]+1,perdidos=perdidos_ant[4],fecha=ymd(fecha)))
    ifelse(is.na(p2),nada<-"nada",tabla<<-rbind(tabla,tibble(jugador=p2,puntos=puntos_ant[5]+40*(.5-1/(1+10**((promedio_ganador-promedio_perdedor)/400))),jugados=jugados_ant[5]+1,ganados=ganados_ant[5],empatados=empatados_ant[5]+1,perdidos=perdidos_ant[5],fecha=ymd(fecha))))

    partidos<<-rbind(partidos,tibble(num_partido=max_partido+1,ganador_1=g1,ganador_2=ifelse(is.na(g2),NA,g2),perdedor_1=p1,perdedor_2=ifelse(is.na(p2),NA,p2),perdedor_3=ifelse(is.na(p3),NA,p3),empate=TRUE,ausencia=FALSE,fecha=ymd(fecha)))

  } else if (ausencia==TRUE){
    tabla<<-rbind(tabla,tibble(jugador=g1,puntos=puntos_ant[1],jugados=jugados_ant[1]+1,ganados=ganados_ant[1],empatados=empatados_ant[1]+1,perdidos=perdidos_ant[1],fecha=ymd(fecha)))
    ifelse(is.na(g2),nada<-"nada",tabla<<-rbind(tabla,tibble(jugador=g2,puntos=puntos_ant[2],jugados=jugados_ant[2]+1,ganados=ganados_ant[2],empatados=empatados_ant[2]+1,perdidos=perdidos_ant[2],fecha=ymd(fecha))))
    ifelse(is.na(g3),nada<-"nada",tabla<<-rbind(tabla,tibble(jugador=g3,puntos=puntos_ant[3],jugados=jugados_ant[3]+1,ganados=ganados_ant[3],empatados=empatados_ant[3]+1,perdidos=perdidos_ant[3],fecha=ymd(fecha))))
    tabla<<-rbind(tabla,tibble(jugador=p1,puntos=puntos_ant[4]-12,jugados=jugados_ant[4]+1,ganados=ganados_ant[4],empatados=empatados_ant[4]+1,perdidos=perdidos_ant[4],fecha=ymd(fecha)))
    ifelse(is.na(p2),nada<-"nada",tabla<<-rbind(tabla,tibble(jugador=p2,puntos=puntos_ant[5]-12,jugados=jugados_ant[5]+1,ganados=ganados_ant[5],empatados=empatados_ant[5]+1,perdidos=perdidos_ant[5],fecha=ymd(fecha))))
    ifelse(is.na(p3),nada<-"nada",tabla<<-rbind(tabla,tibble(jugador=p3,puntos=puntos_ant[6]-12,jugados=jugados_ant[6]+1,ganados=ganados_ant[6],empatados=empatados_ant[6]+1,perdidos=perdidos_ant[6],fecha=ymd(fecha))))

    partidos<<-rbind(partidos,tibble(num_partido=max_partido+1,ganador_1=g1,ganador_2=ifelse(is.na(g2),NA,g2),perdedor_1=p1,perdedor_2=ifelse(is.na(p2),NA,p2),perdedor_3=ifelse(is.na(p3),NA,p3),empate=FALSE,ausencia=TRUE,fecha=ymd(fecha)))
  } else if (especial==TRUE){

    ifelse(is.na(g1),nada<-"nada",tabla<<-rbind(tabla,tibble(jugador=g1,puntos=puntos_ant[1]+premio,jugados=jugados_ant[1],ganados=ganados_ant[1],empatados=empatados_ant[1],perdidos=perdidos_ant[1],fecha=ymd(fecha))))
    ifelse(is.na(p1),nada<-"nada",tabla<<-rbind(tabla,tibble(jugador=p1,puntos=puntos_ant[4]-castigo,jugados=jugados_ant[4],ganados=ganados_ant[4],empatados=empatados_ant[4],perdidos=perdidos_ant[4],fecha=ymd(fecha))))

  } else {
    tabla<<-rbind(tabla,tibble(jugador=g1,puntos=puntos_ant[1]+40*(1-1/(1+10**((promedio_perdedor-promedio_ganador)/400))),jugados=jugados_ant[1]+1,ganados=ganados_ant[1]+1,empatados=empatados_ant[1],perdidos=perdidos_ant[1],fecha=ymd(fecha)))
    ifelse(is.na(g2),nada<-"nada",tabla<<-rbind(tabla,tibble(jugador=g2,puntos=puntos_ant[2]+40*(1-1/(1+10**((promedio_perdedor-promedio_ganador)/400))),jugados=jugados_ant[2]+1,ganados=ganados_ant[2]+1,empatados=empatados_ant[2],perdidos=perdidos_ant[2],fecha=ymd(fecha))))
    tabla<<-rbind(tabla,tibble(jugador=p1,puntos=puntos_ant[4]+40*(0-1/(1+10**((promedio_ganador-promedio_perdedor)/400))),jugados=jugados_ant[4]+1,ganados=ganados_ant[4],empatados=empatados_ant[4],perdidos=perdidos_ant[4]+1,fecha=ymd(fecha)))
    ifelse(is.na(p2),nada<-"nada",tabla<<-rbind(tabla,tibble(jugador=p2,puntos=puntos_ant[5]+40*(0-1/(1+10**((promedio_ganador-promedio_perdedor)/400))),jugados=jugados_ant[5]+1,ganados=ganados_ant[5],empatados=empatados_ant[5],perdidos=perdidos_ant[5]+1,fecha=ymd(fecha))))

    partidos<<-rbind(partidos,tibble(num_partido=max_partido+1,ganador_1=g1,ganador_2=ifelse(is.na(g2),NA,g2),perdedor_1=p1,perdedor_2=ifelse(is.na(p2),NA,p2),perdedor_3=ifelse(is.na(p3),NA,p3),empate=FALSE,ausencia=FALSE, fecha=ymd(fecha)))
  }

  # Creación de tabla general con los últimos resultados de puntos
  tabla_general<<-tabla%>%
    group_by(jugador)%>%
    filter(jugados==max(fecha))%>%
    ungroup()%>%
    arrange(jugador)%>%
    arrange(-puntos)
  tabla_general$puntos<<-format(round(as.numeric(tabla_general$puntos),2),big.mark = ",")


  # Visualizar registro de los partidos
  partidos_org<<-gather(partidos, resultado, jugador, c("ganador_1","ganador_2","perdedor_1","perdedor_2","perdedor_3"))%>%
    mutate(resultado=case_when(
      resultado=="ganador_1"~"ganador",
      resultado=="ganador_2"~"ganador",
      resultado=="perdedor_1"~"perdedor",
      resultado=="perdedor_2"~"perdedor",
      resultado=="perdedor_3"~"perdedor"),
      resultado=case_when(
        empate==TRUE~"empate",
        empate==FALSE~resultado),
      resultado=case_when(
        ausencia==TRUE~"ausencia",
        ausencia==FALSE~resultado)
    )%>%
    drop_na()%>%
    arrange(num_partido)%>%
    select(num_partido,resultado,jugador,fecha)
}

