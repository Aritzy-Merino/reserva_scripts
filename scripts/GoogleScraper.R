#GoogleScraper v1

#Correr después de API Custom Search
rm(list= ls()[!(ls() %in% c("filtrada", "directorio"))])

#100 búsquedas al día son gratis
#10 mil búsquedas al día son máximo
#To do:
#¿Cambiar búsqueda para que se limite a MX?

#Librerías
library(rvest)
library(urltools)
library(dplyr)

#Parámetros
nomArchivo <- "Scraped.csv"

#Importar csv con selectores por cada dominio
tSelector <- read.csv(file="tSelectores.csv", stringsAsFactors=FALSE)
sUtiles <- tSelector[tSelector$SchemaFrom != "", ]

#Análisis de dominios
filtrada$Dominio <- urltools::domain(filtrada$URLCrudo)
dominios <- table(filtrada$Dominio)
dominios <- data.frame("Dominio" = names(dominios),
                       "Frecuencia" = as.integer(dominios))
dominios <- dominios[order(-dominios$Frecuencia),]
dominios$Porcentaje <- dominios$Frecuencia/(sum(dominios$Frecuencia))
dominios$Acumulado <- cumsum(dominios$Porcentaje)

#Crear paquete para que todo sea funciones

#Scraper genérico
#Input: URL
#Output: lista con título, texto, fecha y dominio
#Refactor? los [1] para atomic vectors
#¿Extender para incluir autor?
scrape <- function(url="", par="p", header="h1", d="") {
  require(rvest)
  lista <- tryCatch({
      nodelist <- xml2::read_html(url)
      pnodes <- rvest::xml_nodes(nodelist, par)
      hnodes <- rvest::xml_nodes(nodelist, header)
      #Aquí necesito usar [[1]] porque ifelse obliga al output a ser lista
      dnodes <- ifelse(nchar(d) > 0, rvest::xml_nodes(nodelist, d), "NA")[[1]]
      
      #Si encuentra el título, lo pega
      title <- ifelse(xml2::xml_text(hnodes)>0, xml2::xml_text(hnodes), "NA")[1]
      
      #Pega todos los nodos p
      txt <- ifelse(xml2::xml_text(pnodes)>0, paste(xml2::xml_text(pnodes), collapse="\n"), "NA")[1]
      
      #Si fecha es character (parámetro vacío), regresa NA
      #Si no, regresa el valor del selector
      fcha <- ifelse(class(dnodes) == "xml_nodeset" || class(dnodes) == "xml_node",
                     ifelse(xml2::xml_text(dnodes)>0, xml_text(dnodes), "NA")[1],
                     dnodes)
      
      lista <- list(Titulo=title[1],
                    Texto=txt[1],
                    Fecha=fcha[1],
                    Dominio=urltools::domain(url))
    }, error = function(e) {
      print(paste0("Hubo un error en ", url, "llamado ", e, " y se darán valores vacíos."))
      # lista <- list(Titulo="NA",
      #               Texto="NA",
      #               Fecha="NA",
      #               Dominio="NA")
      return (list(Titulo="NA",
                   Texto="NA",
                   Fecha="NA",
                   Dominio="NA"))
      # return (lista)
    })
  return (lista)
}

metodos <- character(length(filtrada$URLCrudo))
aglom <- list()
#Procesamiento de BD
for (i in filtrada$URLCrudo) {
  #Revisar dominio
  # dom <- filtrada[filtrada$URLCrudo==i, "Dominio"][1]
  dom <- filtrada$Dominio[filtrada$URLCrudo==i]
  
  print(paste0("Scraping: ", i))
  #Si hay esquema predefinido, se utiliza para el scraping
  if (dom %in% sUtiles$Dominio) {
    #Define selectores de query con la tabla filtrada
    p <- sUtiles$Par[sUtiles$Dominio==dom]
    h <- sUtiles$Header[sUtiles$Dominio==dom]
    f <- sUtiles$Fecha[sUtiles$Dominio==dom]
    p <- ifelse(p=="", "p", p)
    h <- ifelse(h=="", "h1",h)
    # f <- ifelse(f=="")
    
    #Hace el scraping
    resultado <- scrape(i,
                        par=p,
                        header=h,
                        d=f)
    metodos[length(aglom)+1] <- "Selector"
  }
  
  #Si no hay esquema predefinido, se usan valores default
  else {
    resultado <- scrape(i)
    metodos[length(aglom)+1] <- "Heurístico"
  }
  
  #Checar lo de multibyte error (creo que es con emoji)
  
  #Se agrega el resultado a una lista
  aglom[[length(aglom) + 1]] <- resultado
}

#Transformar a dataframe
l <- length(aglom)
dfNotas <- data.frame("Titulo" = character(l),
                      "Texto" = character(l),
                      "Fecha" = character(l),
                      "Dominio" = character(l),
                      stringsAsFactors = FALSE)

#Refactor? Tal vez usar dplyr
for (j in 1:l) {
  dfNotas$Titulo[j] <- aglom[[j]]$Titulo
  dfNotas$Texto[j] <- aglom[[j]]$Texto
  dfNotas$Fecha[j] <- aglom[[j]]$Fecha
  dfNotas$Dominio[j] <- aglom[[j]]$Dominio
}

dfNotas$URL <- filtrada$URLCrudo[1:l]
dfNotas$Metodo <- metodos

write.csv(dfNotas, file=paste(directorio,nomArchivo, sep="/"))
