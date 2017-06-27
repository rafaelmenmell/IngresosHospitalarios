Un caso práctico de Data Science
================
<rafaelmenmell@gmail.com>
21 de junio de 2017

Introducción
------------

Este documento es un intento de hacer un caso práctico de lo que se ha dado en llamar *Data Science*, desarrollando todas las fases del proyecto típico de *DS*. El siguiente esquema [1] muestra el proyecto típico de *data science*:

![Esquema de un proyecto de Data Science](http://r4ds.had.co.nz/diagrams/data-science.png)

Por lo tanto el desarrollo del proyecto va a seguir los siguientes pasos:

1.  Adquisición y preparación de datos

2.  Análisis exploratorio y visualización

3.  Modelización y predicción

Adquisición y preparación de datos
----------------------------------

### Adquisición de los datos

Los datos elegidos para el presente proyecto son los recogidos en la encuesta de morbilidad hospitalaria (*morbilidad*) recogida por el Instituto Nacional de Esatdística (*INE*) [2], se trata de una encuesta carácter nacional con periodicidad anual en la que se refieren todas las altas hospitalarias de aproximadamente 850 hospitales.

Actualmente se pueden encontrar los ficheros resultado de las encuestas de los años 1978 a 2015 en la sección de *Microdatos* [3], no se ha encontrado ningún medio para descargar estos ficheros programaticamente (acceso web directo, API) por lo que los ficheros se han descargado manualmente. Los resultados de cada año se descargan en un fichero comprimido llamado *datos\_morbiYY.zip*.

El contenido de cada archivo comprimido es un fichero de texto en el que cada línea se corresponde con un alta hospitalaria acaecida en el año en curso. Cada línea está compuesta de 54 caracteres. Por ejemplo

<p style="text-align: center;">
255883032822811503132180104005110000010010000000000000
</p>
*INE* [4] facilita el formato de los datos con los que es posible decodificar los mismos.

<table>
<colgroup>
<col width="22%" />
<col width="8%" />
<col width="14%" />
<col width="53%" />
</colgroup>
<thead>
<tr class="header">
<th>Campo</th>
<th>Formato</th>
<th>Posición</th>
<th>Descripción</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Número</td>
<td>a8</td>
<td>1--8</td>
<td>Número de orden</td>
</tr>
<tr class="even">
<td>Provincia de hospitalización</td>
<td>a2</td>
<td>9--10</td>
<td>Códigos de provincia: 01-52</td>
</tr>
<tr class="odd">
<td>Sexo</td>
<td>a1</td>
<td>11--11</td>
<td>Sexo: 1 = varón, 2 = mujer</td>
</tr>
<tr class="even">
<td>Provincia de residencia</td>
<td>a2</td>
<td>12--13</td>
<td>Códigos de provincia: 01-52 53 = extranjeros</td>
</tr>
<tr class="odd">
<td>Diagnóstico de entrada</td>
<td>a1</td>
<td>14--14</td>
<td>Diagnóstico de entrada: 1 = ordinario, 2 = urgente</td>
</tr>
<tr class="even">
<td>ys de alta</td>
<td>a6</td>
<td>15--20</td>
<td>Fecha de alta: aa/mm/dd</td>
</tr>
<tr class="odd">
<td>Diagnóstico principal (CIE-9-MC)</td>
<td>a4</td>
<td>21-24</td>
<td>Diagnóstico principal (código CIE-9-MC) Código '857' = 'Alta sin diagnóstico'</td>
</tr>
<tr class="even">
<td>Motivo del alta</td>
<td>a1</td>
<td>25--25</td>
<td>Motivo del alta: 1 = curación , 2 = traslado a otro centro, 3 = fallecimiento , 4 = otras causas</td>
</tr>
<tr class="odd">
<td>Edad en años</td>
<td>a3</td>
<td>26-28</td>
<td>Edad en años: 0-120</td>
</tr>
<tr class="even">
<td>Edad en meses</td>
<td>a2</td>
<td>29-3</td>
<td>Edad en meses: 0-12</td>
</tr>
<tr class="odd">
<td>Edad en días</td>
<td>a2</td>
<td>31-32</td>
<td>Edad en días: 0-31</td>
</tr>
<tr class="even">
<td>Estancia</td>
<td>a6</td>
<td>33-38</td>
<td>Días de estancia</td>
</tr>
<tr class="odd">
<td>Factor de elevación</td>
<td>999v99999</td>
<td>39-46</td>
<td>Factor de elevación (5 decimales)</td>
</tr>
<tr class="even">
<td>filler</td>
<td>a8</td>
<td>47-54</td>
<td>(en blanco)</td>
</tr>
</tbody>
</table>

### Preparación de los datos

Gracias al diccionario de datos se programa una función *LeeZip(year,d=FALSE,p=NULL,u)* en la que los parámetros tienen el siguiente uso:

-   d: (logical) conservar el diagnóstico principal
-   p: (integer) provincia de ingreso
-   U: (logical) filtrar sólo ingresos urgentes

Así por ejemplo:

``` r
ingresos.urgentes.madrid.2010 <- suppressMessages(LeeZip(year = "2005",d = TRUE,p = 28,u = TRUE))
head(ingresos.urgentes.madrid.2010)
```

    ## # A tibble: 6 × 9
    ##   prov_hosp  sexo prov_res diag_in diag_ppal motivo_alta estancia
    ##       <int> <int>    <int>   <int>     <chr>       <int>    <int>
    ## 1        28     1       28       2      5191           1        2
    ## 2        28     1       28       2      4131           1       15
    ## 3        28     2       28       2      2964           2        3
    ## 4        28     2       28       2      8832           1        2
    ## 5        28     2       28       2       486           1        7
    ## 6        28     1       28       2      1629           1        6
    ## # ... with 2 more variables: fecha_ingreso <date>, edad <int>

Ya casi tenemos toda la información preparada para poder empezar a hacer ciencia de datos. Sólo falta convertir el diagnóstico principal *diag\_ppal* en información directa. Gracias a la diccionario de Correspondencia entre los diagnósticos publicados y la Clasificación Internacional de Enfermedades de la O.M.S. (9ª Revisión MC)[5] podemos contruir sendos diccionarios simplificados que leemos con funciones *ad hoc*.

``` r
diccionario <- CargaDiccionario()
diccionario_masgeneral <- CargaDiciconarioMasGeneral()
head(diccionario1)
```

    ##                                                                                  diag
    ## 1                                             ENFERMEDADES INFECCIOSAS Y PARASITARIAS
    ## 2                                                                          NEOPLASIAS
    ## 3 ENFERMEDADES ENDOCRINAS, DE LA NUTRICION Y METABOLICAS Y TRASTORNOS DE LA INMUNIDAD
    ## 4                          ENFERMEDADES DE LA SANGRE Y DE LOS ORGANOS HEMATOPOYÉTICOS
    ## 5                 TRASTORNOS MENTALES, DEL COMPORTAMIENTO Y EL DESARROLLO NEUROLÓGICO
    ## 6                  ENFERMEDADES DEL SISTEMA NERVIOSO Y DE LOS ÓRGANOS DE LOS SENTIDOS
    ##   inicio fin     V ID
    ## 1      1 139 FALSE  1
    ## 2    140 239 FALSE  2
    ## 3    240 279 FALSE  3
    ## 4    280 289 FALSE  4
    ## 5    290 319 FALSE  5
    ## 6    320 389 FALSE  6

``` r
head(diccionario)
```

    ##                                                                                                                             diag
    ## 1                                                                                          ENFERMEDADES INFECCIOSAS INTESTINALES
    ## 2                                                                                                                   TUBERCULOSIS
    ## 3                                                                                            ENFERMEDADES BACTERIANAS ZOONOSICAS
    ## 4                                                                                                 OTRAS ENFERMEDADES BACTERIANAS
    ## 5                                                                             INFECCIÓN POR VIRUS DE LA INMUNODEFICIENCIA HUMANA
    ## 6 POLIOMIELITIS, OTRAS ENFERMEDADES VÍRICAS Y ENFERMEDADES PRIÓNICAS DEL SISTEMA NERVIOSO CENTRAL NO TRANSMITIDAS POR ARTRÓPODOS
    ##   inicio fin     V ID
    ## 1      1   9 FALSE  1
    ## 2     10  18 FALSE  2
    ## 3     20  27 FALSE  3
    ## 4     30  41 FALSE  4
    ## 5     42  42 FALSE  5
    ## 6     45  49 FALSE  6

Todavía hay un nivel de detalle mayor en el código de diagnóstico que no aparece en el mencionado diccionario, para acceder a esta información utilizamos la web <http://icd9cm.chrisendres.com/index.php> mediante una función *TraduceCodigoEspecifico &lt;- function(codigo)* en la que se la pasa el código que hemos obtenido y devuelve el diagnóstico específico, vamos a comprobar con un ejemplo los tres niveles de diagnóstico.

``` r
id_diagnostico <- 4131
id_masgeneral <- TraduceCodigoMasGeneral(id_diagnostico)
id_general <- TraduceCodigoGeneral(id_diagnostico)
diag_masgeneral <- diccionario_masgeneral[diccionario_masgeneral$ID==id_masgeneral,"diag"]
diag_general <- diccionario[diccionario$ID==id_general,"diag"]
diag_especifico <- suppressWarnings(TraduceCodigoEspecifico(id_diagnostico))
diag_masgeneral
```

    ## [1] "ENFERMEDADES DEL SISTEMA CIRCULATORIO"

``` r
diag_general
```

    ## [1] "CARDIOPATÍA ISQUÉMICA"

``` r
diag_especifico
```

    ## [1] " Prinzmetal angina"

Queda una última etapa para completar nuestro procesamiento de datos, se trata de tener una base poblacional sobre la que erlativizar nuestros datos. Necesitamos poder referir nuestros datos a la población de cada provincia en cada año, para ello usaremos lass Cifras Oficiales de Población de los Municipios Españoles: Revisión del Padrón Municipal[6], de esta forma evitaremos dar cifras absolutas y usaremos una medida estándar en lo referente a la salud pública como es la incidencia por cada 100.000 habitantes.

Con este procesado ya tenemos las herramientas necesarias para pasar a la siguiente fase del esquema de fludo del proyecto de *Data Science*

Análisis exploratorio y visualización
-------------------------------------

### Planteamineto

Las posibilidades que ofrece este conjunto de datos es inmensa, voy a centrar este estudio exploratorio en tres cuestiones:

1.  ¿Cómo de importantes son los ingresos de menores debidos a enfermedades respiratorias? ¿Tienen una estacionalidad?
2.  ¿Es distinta la tipología de las lesiones muscoloesqueléticas en hombre y en mujeres? ¿Hay alguna estacionalidad?
3.  ¿Es posible conocer el número de ingresos por herida de bala en cada provincia? ¿Es posible que haya alguna relación con la temporada de caza?

Voy a centrar el estudio en la última década disponible 2005-2015, en los dos primeros casos sólo en la Comunidad de Madrid y en el tercero en todas las provincias de España.

#### Menores y enfermedades respiratorias

Lo primero es hacernos con un conjunto de datos de los ingresos hospitalarios urgentes, periodo 2005-2015, en la Comunidad de Madrid para menores de 16 años.

El numero total de ingresos urgentes en ese periodo para menores de 17 años fue de 489047 de ellos, 131995 fueros debidos a enfermedades del aparato respiratorio, por lo que confirmamos la importancia de estas patologías.

``` r
ejemplo.count.year.diag1 <- ingresos1 %>% dplyr::group_by(anyo=year(fecha_ingreso),diag1) %>% dplyr::summarise(total=n()) %>% dplyr::top_n(10,total)

#quito los ingresos de 2004 y unos años que salen como NA
ejemplo.count.year.diag1 <- ejemplo.count.year.diag1[ejemplo.count.year.diag1$anyo!=2004,]
ejemplo.count.year.diag1 <- ejemplo.count.year.diag1[complete.cases(ejemplo.count.year.diag1),]
ejemplo.count.year.diag1 <- full_join(ejemplo.count.year.diag1,diccionario_masgeneral,by=c("diag1"="ID"))

ggplot(data=ejemplo.count.year.diag1)+geom_bar(stat="identity",aes(x=reorder(diag1,total),total,fill=diag1))+facet_wrap(~ anyo,nrow=5,scales = "fixed")+theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks.x=element_blank()) +  theme(legend.text=element_text(size=5),legend.title=element_blank())+labs(y="Ingresos",title="Diez razones principales de ingreso por año. 2005-2015",subtitle="Madrid. Pacientes menores de 17 años",caption="INE. Encuesta de morbilidad hospitalaria")
```

    ## Warning: Removed 6 rows containing missing values (position_stack).

![](documento_files/figure-markdown_github/graph1-1.png)

``` r
ejemplo.count.year.diag2 <- ingresos2 %>% dplyr::group_by(anyo=year(fecha_ingreso),diag2) %>% dplyr::summarise(total=n()) %>% dplyr::top_n(10,total)

#quito los ingresos de 2004 y unos años que salen como NA
ejemplo.count.year.diag2 <- ejemplo.count.year.diag2[ejemplo.count.year.diag2$anyo!=2004,]
ejemplo.count.year.diag2 <- ejemplo.count.year.diag2[complete.cases(ejemplo.count.year.diag2),]
ejemplo.count.year.diag2 <- full_join(ejemplo.count.year.diag2,diccionario,by=c("diag2"="ID"))

ggplot(data=ejemplo.count.year.diag2)+geom_bar(stat="identity",aes(x=reorder(diag2,total),total,fill=diag2))+facet_wrap(~ anyo,nrow=5,scales = "fixed")+theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks.x=element_blank()) +  theme(legend.text=element_text(size=5),legend.title=element_blank())+labs(y="Ingresos",title="Diez razones principales de ingreso por año por enfermedades del aparato respiratorio. 2005-2015",subtitle="Madrid. Pacientes menores de 17 años",caption="INE. Encuesta de morbilidad hospitalaria")
```

    ## Warning: Removed 117 rows containing missing values (position_stack).

![](documento_files/figure-markdown_github/graph2-1.png)

[1] [R for Data Science, Hadley Wickham & Garrett Grolemund](http://r4ds.had.co.nz/).

[2] [Encuesta de morbilidad hospitalaria. Instituto Nacional de Estadística](http://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176778&menu=metodologia&idp=1254735573175)

[3] [Encuesta de morbilidad hospitalaria. Instituto Nacional de Estadística. Microdatos](http://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176778&menu=metodologia&idp=1254735573175)

[4] [Encuesta de morbilidad hospitalaria. Instituto Nacional de Estadística. Diccionario](ftp://www.ine.es/temas/morbihos/disreg_morbi.zip)

[5] [Correspondencia entre los diagnósticos publicados y la Clasificación Internacional de Enfermedades de la O.M.S. (9ª Revisión MC)](http://www.ine.es/daco/daco42/sanitarias/cie_mc_emh.xls)

[6] [Cifras Oficiales de Población de los Municipios Españoles: Revisión del Padrón Municipal)](http://www.ine.es/jaxiT3/Tabla.htm?t=2852&L=0)
