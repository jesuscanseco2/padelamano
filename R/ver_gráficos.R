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
  facet_wrap(vars(jugador),ncol=5,nrow = 2)+
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
    scale_x_continuous(breaks=seq(0,max(tabla$jugados),1))+
    annotate("text",label=as.character(max(último_jug$fecha)),y=Inf,x=Inf,vjust=1,hjust=1)




  tres<<-ggplot(último_jug2,aes(reorder(jugador, -puntos,sum),puntos,fill=jugador))+
    geom_col()+
    coord_cartesian(ylim=c(min(último_jug2$puntos)-10,max(último_jug2$puntos)+10))+
    geom_text(aes(label = puntos),vjust=-.5)+
    theme(legend.position="none",
          axis.title.x=element_blank())+
    scale_fill_hue()+
    annotate("text",label=as.character(max(último_jug$fecha)),y=Inf,x=Inf,vjust=1,hjust=1)

  tabla_imagen<<-tableGrob(tabla_general[,-7])

  plot(tabla_imagen)
  list(uno,dos,tres)
}
