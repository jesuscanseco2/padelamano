ver_gráficos<-function (){

  último_jug<<-tabla%>%
    group_by(jugador)%>%
    slice(which.max(jugados))

  último_jug2 <<- aggregate(último_jug$puntos, by=list(jugador=último_jug$jugador), FUN = sum)
  último_jug2 <<- as.data.frame(último_jug2)
  names(último_jug2) <<- c("jugador", "puntos")
  último_jug2$puntos<<-round(último_jug2$puntos)



  uno<<-ggplot(tabla,aes(jugados,puntos,color=jugador))+
  geom_point()+
  geom_line()+
  facet_wrap(vars(jugador))+
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_hline(yintercept=1000, linetype="dashed", color = "red")



  dos<<-ggplot(tabla,aes(jugados,puntos,color=jugador))+
    geom_point()+
    geom_line()+
    geom_hline(yintercept=1000, linetype="dashed", color = "red")+
    geom_text(data=último_jug,aes(label = jugador),vjust=-.5)+
    theme(legend.position="none")+
    scale_x_continuous()+
    annotate("text",label=as.character(max(último_jug$fecha)),y=Inf,x=Inf,vjust=1,hjust=1)




  tres<<-ggplot(último_jug2,aes(reorder(jugador, -puntos,sum),puntos,fill=jugador))+
    geom_col()+
    coord_cartesian(ylim=c(min(último_jug2$puntos)-10,max(último_jug2$puntos)+10))+
    geom_text(aes(label = puntos),vjust=-.5)+
    theme(legend.position="none",
          axis.title.x=element_blank())+
    scale_fill_hue()+
    annotate("text",label=as.character(max(último_jug$fecha)),y=Inf,x=Inf,vjust=1,hjust=1)+
    ggtitle("Tabla General")


hoy<-max(tabla$fecha)
dos_meses<- hoy %m-% months(2)

reciente<-tabla %>% filter(fecha>dos_meses)  %>% 
group_by(jugador) %>% top_n(1, fecha) %>% top_n(1, jugados) %>% ungroup() %>%
select(jugador,puntos)
pasado<-tabla %>% filter(fecha<=dos_meses) %>% 
group_by(jugador) %>% top_n(1, fecha) %>% top_n(1, jugados) %>% ungroup() %>%
select(jugador,puntos)

power_ranking<-left_join(reciente,pasado,by="jugador") %>% 
mutate(puntos.x=round(puntos.x,2),
	   puntos.y=round(puntos.y,2),
	   diferencia=round(puntos.x-puntos.y,2))

cuatro<<-ggplot(power_ranking,aes(reorder(jugador, -diferencia,sum),diferencia,fill=jugador))+
    geom_col()+
    coord_cartesian(ylim=c(min(power_ranking$diferencia)-10,max(power_ranking$diferencia)+10))+
    geom_text(aes(label = diferencia),vjust=-.5)+
    theme(legend.position="none",
          axis.title.x=element_blank())+
    scale_fill_hue()+
	ggtitle("Power Ránking Últimos Dos Meses")

fechas<<-as.Date((min(tabla$fecha)+1):max(tabla$fecha))

tablas_generales<<-tibble()

for (i in 1:length(fechas)) {
    
  tabla_diaria <<- tabla %>%
    filter(fecha <= fechas[i]) %>% 
    group_by(jugador) %>%
    filter(jugados == max(jugados)) %>%
    ungroup() %>%
    arrange(jugador) %>%
    arrange(-puntos) %>% 
    select(-fecha) %>% 
    mutate(fecha = fechas[i])
	
tablas_generales<<-rbind(tablas_generales,tabla_diaria)	
	
}

dias_de_lider<<-tablas_generales %>% group_by(fecha) %>% top_n(puntos,n=1) %>% ungroup() %>% group_by(jugador) %>% summarize(dias_lider=n()) %>% arrange(-dias_lider)
dias_de_sotanero<<-tablas_generales %>% group_by(fecha) %>% top_n(puntos,n=-1) %>% ungroup() %>% group_by(jugador) %>% summarize(dias_sotanero=n()) %>% arrange(-dias_sotanero)
tablas_generales_top<<-tablas_generales %>% group_by(fecha) %>% top_n(puntos,n=1) %>% ungroup()
tablas_generales_down<<-tablas_generales %>% group_by(fecha) %>% top_n(puntos,n=-1) %>% ungroup()
	

cinco<<-ggplot(dias_de_lider,aes(reorder(jugador, -dias_lider,sum),dias_lider,fill=jugador))+
    geom_col()+
    coord_cartesian(ylim=c(0,max(dias_de_lider$dias_lider)+5))+
    geom_text(aes(label = dias_lider),vjust=-.5)+
    theme(legend.position="none",
          axis.title.x=element_blank())+
    scale_fill_hue()+
	ggtitle("Días de Líder en la Tabla General")

seis<<-ggplot(dias_de_sotanero,aes(reorder(jugador, -dias_sotanero,sum),dias_sotanero,fill=jugador))+
    geom_col()+
    coord_cartesian(ylim=c(0,max(dias_de_sotanero$dias_sotanero)+5))+
    geom_text(aes(label = dias_sotanero),vjust=-.5)+
    theme(legend.position="none",
          axis.title.x=element_blank())+
    scale_fill_hue()+
	ggtitle("Días de Sotanero en la Tabla General")
	
  
  tabla_imagen<<-tableGrob(tabla_general[,-7])

  plot(tabla_imagen)
}
