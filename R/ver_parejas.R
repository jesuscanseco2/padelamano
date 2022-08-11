ver_parejas<-function (){

# Análisis de partidos

# Todas las combinaciones de jugadores
combinaciones<<-partidos %>%
  rowwise() %>%
  mutate(partido_todos = paste(sort(c(ganador_1,ganador_2,perdedor_1,perdedor_2,perdedor_3)), collapse = " - ")) %>%  # sort the teams alphabetically and then combine them separating with -
  ungroup()%>%
  count(partido_todos)%>%
  arrange(-n)

# Parejas ganadoras y perdedoras
parejas<<-partidos %>%
  rowwise() %>%
  mutate(pareja_ganadora = paste(sort(c(ganador_1,ganador_2)), collapse = " - ")) %>%
  mutate(pareja_perdedora = paste(sort(c(perdedor_1,perdedor_2,perdedor_3)), collapse = " - "))%>%
  ungroup()%>%
  select(c(10,11))

# Repetición de partidos exactos
enfrentamientos_df<<-parejas%>%
  rowwise() %>%
  mutate(enfrentamiento=paste(sort(c(pareja_ganadora,pareja_perdedora)), collapse = " vs "))%>%
  ungroup()%>%
  select(3)%>%
  count(enfrentamiento)%>%
  arrange(-n)

# Parejas juntas
enamorados<<-as.data.frame(c(parejas$pareja_ganadora,parejas$pareja_perdedora))
names(enamorados)<-"pareja_final"
enamorados<-enamorados%>%
  count(pareja_final)%>%
  arrange(-n)

}
