library(SPARQL)
library(xml2)

Sys.setenv(TZ='GMT')

# You might need to change this #

endpoint_select <- 'http://localhost:8080/marmotta/sparql/select'
endpoint_update <- 'http://localhost:8080/marmotta/sparql/update'

ontology <- read_xml('https://raw.githubusercontent.com/fraba/Population-data-ontology/master/popdata_ontology.owl')
# ontology <- read_xml('/Users/francesco/public_git/population_data_ontology/popdata_ontology.owl')

place_type_dict <- c('42D36EE8-5DA9-45DB-8A81-47C8317457A8' = 'stato',
                     '19E0AEE8-F078-4C74-BCC8-47501C9C5555' = 'regione',
                     'A56EB61D-178E-41D1-ACA5-42F188DB4E2D' = 'provincia',
                     '22C1A91C-8864-4AC1-873A-A65F8844FDDC' = 'comune')

# - #

# This will only work on unix
genUUIDs <- function(n) {
  res <- character()
  for (i in 1:n) {
    res <- c(res, toupper(system("uuidgen", intern=T)))
  }
  return(res)
}


formatUuid <- function(str) {
  require(stringr)
  str <- toupper(str)
  uuid_v4_regex <- '[0-9A-F]{8}-[0-9A-F]{4}-4[0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}'
  valid_str <- str_extract(str, uuid_v4_regex)
  if(is.na(valid_str)) stop('UUID is not valid')
  return(valid_str)
}

postUpdate <- function (query) {
  curl_args = NULL
  h <- basicHeaderGatherer()
  do.call(getURL, 
          append(list(url = paste(endpoint_update, "?query=", gsub("\\+", "%2B", URLencode(query, reserved = TRUE)), sep = ""),
                      headerfunction = h$update), 
                 curl_args))
  return(h$value())
}

getAllSettingTypes <- function() {
  xml_nodes <- xml_find_all(ontology, 
                            xpath = '//*/owl:Class[rdfs:subClassOf[@rdf:resource="http://purl.org/popdata#Setting"]]')
  types <- gsub('http://purl\\.org/popdata#', "", unlist(xml_attrs(xml_nodes), use.names = FALSE))
  return(types[order(types)])
}

getAllIdTypes <- function() {
  xml_nodes <- xml_find_all(ontology, 
                            xpath = '//*/owl:DatatypeProperty[rdfs:subPropertyOf[@rdf:resource="http://purl.org/popdata#identification"]]')
  types <- gsub('http://purl\\.org/popdata#', "", unlist(xml_attrs(xml_nodes), use.names = FALSE))
  return(types[order(types)])
}

getDataPropertyConstraint <- function(type) {
  xml_node <- xml_find_first(ontology, xpath = sprintf('//*[@rdf:about="http://purl.org/popdata#%s"]/rdfs:range', type))
  
  if (!is.na(xml_attr(xml_node, "resource"))) {
    datatype <-
      gsub("http://www\\.w3\\.org/2001/XMLSchema#", 
           "xsd:", xml_attr(xml_node, attr = 'resource'))
    return(list('datatype' = datatype, 'pattern' = NA))
  } else {
    datatype <-
      gsub("http://www\\.w3\\.org/2001/XMLSchema#", 
           "xsd:", xml_attr(xml_find_first(xml_node, 
                                           xpath = 'rdfs:Datatype/owl:onDatatype'),
                            attr = 'resource'))
    pattern <- 
      xml_text(xml_find_first(xml_node, 
                              xpath = 'rdfs:Datatype/owl:withRestrictions/rdf:Description/xsd:pattern'))
    return(list('datatype' = datatype, 'pattern' = pattern)) 
  }
}

queryPlaceByRegexToposet <- function(str) {
  query <-
    sprintf(
      "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX owl: <http://www.w3.org/2002/07/owl#>
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      PREFIX ppdt: <http://purl.org/popdata#>
      SELECT DISTINCT ?place WHERE {
      ?place rdf:type ppdt:Place.
      ?setting rdf:type ppdt:ToponymalSetting.
      ?place ppdt:scopedBy ?setting.
      ?setting rdfs:label ?settingLabel.
      FILTER regex(?settingLabel, '%s', 'i').
      FILTER (lang(?settingLabel) = 'it')
      }",
      str
    )
  res <- SPARQL(endpoint_select, query)
  return(unique(as.character(res$results)))
}

# CREATE / DELETE
createPlace <- function() {
  place_uuid <- genUUIDs(1)
  query <-
    sprintf("
            PREFIX ppdt: <http://purl.org/popdata#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            INSERT DATA {
            <http://purl.org/popdata/entity/%s> rdf:type ppdt:Place. 
            }", place_uuid)
  header <- postUpdate(query)
  if (header['status'] == '200') {
    return(place_uuid)
  } else {
    stop(paste0("Update error: ", header['statusMessage']))
  }
}

deletePlace <- function(place_uuid) {
  place_uuid <- formatUuid(place_uuid)
  query <- sprintf("PREFIX ppdt: <http://purl.org/popdata#>
                    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                   DELETE WHERE { 
                   <http://purl.org/popdata/entity/%s> rdf:type ppdt:Place ;
                      ppdt:scopeBy ?setting. 
                   }", place_uuid)
  header <- postUpdate(query)
  if (header['status'] != '200') {
    stop(paste0("Update error: ", header['statusMessage']))
  } else {
    return(header['statusMessage'])
  }
}

createSetting <- function(type) {
  setting_uuid <- genUUIDs(1)
  
  if (!type %in% getAllSettingTypes()) stop(sprintf("This setting type doesn't exit: %s", type))
  
  query <-
    sprintf("
            PREFIX ppdt: <http://purl.org/popdata#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            INSERT DATA {
            <http://purl.org/popdata/entity/%s> rdf:type ppdt:%s 
            }", setting_uuid, type)
  header <- postUpdate(query)
  if (header['status'] == '200') {
    return(setting_uuid)
  } else {
    stop(paste0("Update error: ", header['statusMessage']))
  }
}

deleteSetting <- function(setting_uuid, type) {
  if (!type %in% getAllSettingTypes()) stop(sprintf("This setting type doesn't exit: %s", type))
  setting_uuid <- formatUuid(setting_uuid)
  query <- sprintf("PREFIX ppdt: <http://purl.org/popdata#>
                   PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                   DELETE WHERE { 
                   <http://purl.org/popdata/entity/%s> rdf:type ppdt:%s; 
                      ppdt:hasTemporalScope ?temporalScope ;
                      ppdt:hasSpatialScope ?spatialScope ;
                      ppdt:hasSource ?source.
                   }", setting_uuid, type)
  header <- postUpdate(query)
  if (header['status'] != '200') {
    stop(paste0("Update error: ", header['statusMessage']))
  } else {
    return(header['statusMessage'])
  }
}

createOrGetInstant <- function(date) {
  date <- format(as.Date(date), "%Y-%m-%d")
  query <- sprintf('PREFIX ppdt: <http://purl.org/popdata#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX time: <http://www.w3.org/2006/time#>
            SELECT * WHERE {
            ?instant rdf:type time:Instant.
            ?instant time:inXSDDate ?dateInstant.
            FILTER (DATATYPE(?dateInstant) = xsd:date && STR(?dateInstant) = "%s")
            }', date)
  res <- SPARQL(endpoint_select, query)
  if(nrow(res$results) == 0) {
    instant_uuid <- genUUIDs(1)
    query <- sprintf("PREFIX ppdt: <http://purl.org/popdata#>
                     PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                     PREFIX time: <http://www.w3.org/2006/time#>
                     INSERT DATA {
                     <http://purl.org/popdata/entity/%s> rdf:type time:Instant.
                     <http://purl.org/popdata/entity/%s> time:inXSDDate '%s'^^xsd:date.
                     }", instant_uuid, instant_uuid, date)
    header <- postUpdate(query)
    if (header['status'] == '200') {
      return(instant_uuid)
    } else {
      paste0("Update error: ", header['statusMessage'])
    }
  } else if (nrow(res$results) == 1) {
    instant_uuid <- formatUuid(res$results$instant)
    return(instant_uuid)
  } else {
    stop(paste0("Error: duplicated instant with value ", date))
  }
}

deleteInstant <- function(instant_uuid = NULL, date = NULL) {
  
  if (is.null(instant_uuid) & is.null(date)) {
    stop("Provide either instant_uuid or date")
  }
  
  if (is.null(instant_uuid)) {
    query <- sprintf('PREFIX ppdt: <http://purl.org/popdata#>
                     PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                     PREFIX time: <http://www.w3.org/2006/time#>
                     SELECT ?instant WHERE {
                     ?instant time:inXSDDate ?instantDate.
                     FILTER (DATATYPE(?instantDate) = xsd:date && STR(?instantDate) = "%s")
                     }', date)
    res <- SPARQL(endpoint_select, query)
    instant_uuid <- res$results$instant
  }
  
  instant_uuid <- formatUuid(instant_uuid)
  query <- sprintf("PREFIX ppdt: <http://purl.org/popdata#>
                    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                   PREFIX time: <http://www.w3.org/2006/time#>
                   DELETE WHERE { 
                   <http://purl.org/popdata/entity/%s> rdf:type time:Instant ;
                   time:inXSDDate ?instantDate .
                   }", instant_uuid)
  header <- postUpdate(query)
  if (header['status'] != '200') {
    stop(paste0("Update error: ", header['statusMessage']))
  } else {
    return(header['statusMessage'])
  }
}

createOrGetInterval <- function(from, to) {
  
  from <- format(as.Date(from), "%Y-%m-%d")
  to <- format(as.Date(to), "%Y-%m-%d")
  
  from_uuid <- createOrGetInstant(from) # "FC0BA004-D843-4E79-A7C4-B8097F408228"
  if (!is.na(to)) {
    to_uuid <- createOrGetInstant(to)
    query <- sprintf('PREFIX ppdt: <http://purl.org/popdata#>
                    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                     PREFIX time: <http://www.w3.org/2006/time#>
                     SELECT ?interval WHERE {
                     ?interval rdf:type time:Interval.
                     ?interval time:hasBeginning <http://purl.org/popdata/entity/%s>.
                     ?interval time:hasEnd <http://purl.org/popdata/entity/%s>. 
                     }', from_uuid, to_uuid)
    res <- SPARQL(endpoint_select, query)
    if (nrow(res$results)==0) {
      interval_uuid <- genUUIDs(1)
      query <- sprintf('PREFIX ppdt: <http://purl.org/popdata#>
                        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                       PREFIX time: <http://www.w3.org/2006/time#>
                       INSERT DATA {
                       <http://purl.org/popdata/entity/%s> rdf:type time:Interval ; 
                          time:hasBeginning <http://purl.org/popdata/entity/%s> ; 
                          time:hasEnd <http://purl.org/popdata/entity/%s>. 
                       }', interval_uuid, from_uuid, to_uuid)
      header <- postUpdate(query)
      if (header['status'] != '200') {
        stop(paste0("Update error: ", header['statusMessage']))
      } else {
        return(interval_uuid)
      }
    } else if (nrow(res$results)==1) {
      interval_uuid <- formatUuid(res$results$interval)
      return(interval_uuid)
    } else {
      stop("Error: Duplicated instants")
    }
  } else { # is.na(to)
    query <- sprintf('PREFIX ppdt: <http://purl.org/popdata#>
                      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                     PREFIX time: <http://www.w3.org/2006/time#>
                     SELECT ?interval WHERE {
                     ?interval rdf:type time:Interval.
                     ?interval time:hasBeginning <http://purl.org/popdata/entity/%s>.
                      OPTIONAL {?interval time:hasEnd ?endInstant.}
                      FILTER (!bound(?endInstant))
                     }', from_uuid)
    res <- SPARQL(endpoint_select, query)
    if (nrow(res$results)==0) {
      interval_uuid <- genUUIDs(1)
      query <- sprintf('PREFIX ppdt: <http://purl.org/popdata#>
                       PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                       PREFIX time: <http://www.w3.org/2006/time#>
                       INSERT DATA {
                       <http://purl.org/popdata/entity/%s> rdf:type time:Interval ; 
                       time:hasBeginning <http://purl.org/popdata/entity/%s>.
                       }', interval_uuid, from_uuid)
      header <- postUpdate(query)
      if (header['status'] != '200') {
        stop(paste0("Update error: ", header['statusMessage']))
      } else {
        return(interval_uuid)
      }
    } else if (nrow(res$results)==1) {
      interval_uuid <- formatUuid(res$results$interval)
      return(interval_uuid)
    } else {
      stop("Error: Duplicated instants")
    }
  }
}

deleteInterval <- function(interval_uuid) {
  query <- sprintf('PREFIX ppdt: <http://purl.org/popdata#>
                    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                   PREFIX time: <http://www.w3.org/2006/time#>
                   SELECT * WHERE {
                   <http://purl.org/popdata/entity/%s> rdf:type time:Interval .
                   <http://purl.org/popdata/entity/%s> time:hasBeginning ?beginningInstant.
                   OPTIONAL{<http://purl.org/popdata/entity/%s> time:hasEnd ?endInstant.}
                   }', interval_uuid, interval_uuid, interval_uuid)
  res <- SPARQL(endpoint_select, query)
  if (nrow(res$results)==0) stop("Error: Interval not found")
  if(!is.na(res$results$endInstant)) {
    query <- sprintf('PREFIX ppdt: <http://purl.org/popdata#>
                     PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                     PREFIX time: <http://www.w3.org/2006/time#>
                     DELETE WHERE {
                     <http://purl.org/popdata/entity/%s> rdf:type time:Interval ; 
                     time:hasBeginning ?beginningInstant ; 
                     time:hasEnd ?endInstant . 
                     }', interval_uuid)
  } else {
    query <- sprintf('PREFIX ppdt: <http://purl.org/popdata#>
                        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                     PREFIX time: <http://www.w3.org/2006/time#>
                     DELETE WHERE {
                     <http://purl.org/popdata/entity/%s> rdf:type time:Interval ; 
                     time:hasBeginning ?beginningInstant . 
                     }', interval_uuid)
  }
  header <- postUpdate(query)
  if (header['status'] != '200') {
    stop(paste0("Update error: ", header['statusMessage']))
  } else {
    return(header['statusMessage'])
  }
}

createOrGetTemporalExtent <- function(from, to) {
  
  from <- as.Date(from)
  to <- as.Date(to)
  
  temporal_extent_uuid <- getTemporalExtent(from, to)
  
  if (is.na(temporal_extent_uuid)) {
    temporal_extent_uuid <- genUUIDs(1)
    query <- sprintf('PREFIX ppdt: <http://purl.org/popdata#>
              PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
              INSERT DATA {
              <http://purl.org/popdata/entity/%s> rdf:type ppdt:TemporalExtent.
              }', temporal_extent_uuid)
    header <- postUpdate(query)
    if (header['status'] != '200') {
      stop(paste0("Update error: ", header['statusMessage']))
    }
  }
  return(temporal_extent_uuid)
}

getTemporalExtent <- function(from, to = NA) {
  
  # Check if temporal scope relation already exists
  require(SPARQL)
  
  from <- as.Date(from)
  to <- as.Date(to)
  
  query_has_end <- 
    sprintf('PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      PREFIX ppdt: <http://purl.org/popdata#>
      PREFIX time: <http://www.w3.org/2006/time#>
      SELECT DISTINCT ?tempExtent (STR(?from) AS ?from_str) (STR(?to) AS ?to_str) WHERE {
      ?interval rdf:type time:Interval.
      ?interval time:hasBeginning ?beginningInstant.
      ?beginningInstant time:inXSDDate ?from.
      ?interval time:hasEnd ?endInstant.
      ?endInstant time:inXSDDate ?to.
      ?tempExtent ppdt:temporallyDefinedBy ?interval.
      FILTER (DATATYPE(?from) = xsd:date && STR(?from) = "%s" &&
      DATATYPE(?to) = xsd:date && STR(?to) = "%s")
             }', from, to)
  
  query_has_no_end <- 
    sprintf('PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      PREFIX ppdt: <http://purl.org/popdata#>
      PREFIX time: <http://www.w3.org/2006/time#>
      SELECT DISTINCT ?tempExtent (STR(?from) AS ?from_str) (STR(?to) AS ?to_str) WHERE {
      ?interval rdf:type time:Interval.
      ?interval time:hasBeginning ?beginningInstant.
      ?beginningInstant time:inXSDDate ?from.
      ?tempExtent ppdt:temporallyDefinedBy ?interval.
      OPTIONAL {?interval time:hasEnd ?endInstant.}
      FILTER (DATATYPE(?from) = xsd:date && STR(?from) = "%s" && !bound(?endInstant))
             }', from)
  
  if (!is.na(to)) {
    res <- SPARQL(endpoint_select, query_has_end)
  } else {
    res <- SPARQL(endpoint_select, query_has_no_end)
  }
  
  if (nrow(res$results) > 1) {
    stop("Error: duplicated temporal extent") 
  } else if (nrow(res$results) == 0) {
    return(NA)
  } else if (nrow(res$results) == 1) {
    return(formatUuid(res$results$tempExtent))
  }
}

deleteEntity <- function(entity_uuid) {
  entity_uuid <- formatUuid(entity_uuid)
  query <- sprintf('DELETE WHERE {
                   <http://purl.org/popdata/entity/%s> ?p ?o.
                   }', entity_uuid)
  header <- postUpdate(query)
  if (header['status'] != '200') {
    stop(paste0("Update error: ", header['statusMessage']))
  } else {
    return(header['statusMessage'])
  }
}

placeHasSetting <- function(place_uuid, type) {
  
}

insertScopedByRelation <- function(place_uuid, setting_uuid) {
  require(SPARQL)
  
  place_uuid <- formatUuid(place_uuid)
  setting_uuid <- formatUuid(setting_uuid)
  
  query <-
    sprintf("
            PREFIX ppdt: <http://purl.org/popdata#>
            INSERT DATA {
            <http://purl.org/popdata/entity/%s> ppdt:scopedBy <http://purl.org/popdata/entity/%s>.
            ", place_uuid, setting_uuid)
  SPARQL(endpoint_select, query)
}

insertPlaceIdentificationalSetting <- function(place_uuid, id, type, source_uuid) {
  
  prop_const <- getDataPropertyConstraint(type)
  
  if (grepl(type, id) == FALSE) stop("ID not compatible with ontology constraints.")
  
  place_uuid <- formatUuid(place_uuid)
  setting_uuid <- genUUIDs(1)
  source_uuid <- formatUuid(source_uuid)
  
  insertScopedByRelation(place_uuid, setting_uuid)
  
  query <-
    sprintf("
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX ppdt: <http://purl.org/popdata#>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
            INSERT DATA {
            <http://purl.org/popdata/entity/%s> 
            rdf:type <http://purl.org/popdata#IdentificationalSetting> ;
            ppdt:%s '%s'^^%s ;
            ppdt:hasSource <http://purl.org/popdata/entity/%s> . 
            
            }
            ", setting_uuid, type, id, prop_const[['datatype']], source_uuid)
  SPARQL(endpoint_select, query)
}

# GEO

makeSpatialPolygonsDataFrame <- function(res) {
  require(sp)
  require(rgeos)
  if (nrow(res)==0) return(res)
  this_dat <- as.data.frame(res)
  this_dat$id <- as.character(1:nrow(this_dat))
  this_dat$wkt <- NULL
  this_sp <- lapply(paste0("MULTIPOLYGON", gsub("^(.*)MULTIPOLYGON|\\)\\)\\)(.*)$", "", res$wkt), ")))"),
                    readWKT)
  this_sp <- mapply(spChFIDs, this_sp, this_dat$id)
  require(sp)
  return(SpatialPolygonsDataFrame(SpatialPolygons(unlist(lapply(this_sp, function(x) x@polygons))),  
                                  data = this_dat))
}

# GET

getGeographicalSettingByPlace <- function(place_uuid) {
  
  place_uuid <- formatUuid(place_uuid)
  
  query <-
    sprintf("
            PREFIX time: <http://www.w3.org/2006/time#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX ppdt: <http://purl.org/popdata#>
            PREFIX geo: <http://www.opengis.net/ont/geosparql#>
            SELECT DISTINCT ?multipolygon ?wkt ?from ?to WHERE {
            ?geoSet rdf:type ppdt:GeographicalSetting.
            <http://purl.org/popdata/entity/%s> ppdt:scopedBy ?geoSet.

            ?geoSet ppdt:hasSpatialScope ?spatialExtent.
            ?spatialExtent ppdt:spatiallyDefinedBy ?feature.
            ?feature ppdt:hasExactGeometry ?multipolygon.
            ?multipolygon geo:asWKT ?wkt.
            
            OPTIONAL {?geoSet ppdt:hasTemporalScope ?tempExtent. }
            OPTIONAL {?tempExtent ppdt:temporallyDefinedBy ?interval. }
            
            OPTIONAL {?interval time:hasBeginning ?beginningInstant. }
            OPTIONAL {?beginningInstant time:inXSDDate ?from. }
            
            OPTIONAL {?interval time:hasEnd ?EndInstant. }
            OPTIONAL {?EndInstant time:inXSDDate ?to. }

            }
            ", place_uuid)
  res <- SPARQL(endpoint_select, query)
  dat <- res$results
  if (nrow(dat)==0) return(dat)
  dat$from <- as.Date(as.POSIXct(dat$from, origin = '1970-01-01'))
  dat$to <- as.Date(as.POSIXct(dat$to, origin = '1970-01-01'))
  dat <- dat[order(dat$from, decreasing = TRUE),]
  return(dat)
}


getTypologicalSettingByPlace <- function(place_uuid) {
  
  place_uuid <- formatUuid(place_uuid)
  
  query <-
    sprintf("
            PREFIX time: <http://www.w3.org/2006/time#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX ppdt: <http://purl.org/popdata#>
            SELECT DISTINCT ?typeLabel ?from ?to WHERE {
            ?typoSet rdf:type ppdt:TypologicalSetting.
            <http://purl.org/popdata/entity/%s> 
            ppdt:scopedBy ?typoSet.

            ?typoSet ppdt:hasType ?type.
            ?type rdfs:label ?typeLabel.
            
            OPTIONAL {?typoSet ppdt:hasTemporalScope ?tempExtent. }
            OPTIONAL {?tempExtent ppdt:temporallyDefinedBy ?interval. }
            
            OPTIONAL {?interval time:hasBeginning ?beginningInstant. }
            OPTIONAL {?beginningInstant time:inXSDDate ?from. }
            
            OPTIONAL {?interval time:hasEnd ?EndInstant. }
            OPTIONAL {?EndInstant time:inXSDDate ?to. }

            FILTER (lang(?typeLabel) = 'en').
            
            }
            ", place_uuid)
  res <- SPARQL(endpoint_select, query)
  dat <- res$results
  if (nrow(dat)==0) return(dat)
  dat$from <- as.Date(as.POSIXct(dat$from, origin = '1970-01-01'))
  dat$to <- as.Date(as.POSIXct(dat$to, origin = '1970-01-01'))
  dat <- dat[order(dat$from, decreasing = TRUE),]
  return(dat)
}

getIdentificationalSettingByPlace <- function(place_uuid, type) {
  
  place_uuid <- formatUuid(place_uuid)
  
  query <-
    sprintf("
            PREFIX time: <http://www.w3.org/2006/time#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX ppdt: <http://purl.org/popdata#>
            SELECT DISTINCT ?id ?from ?to WHERE {
            ?IdSet rdf:type ppdt:IdentificationalSetting.
            <http://purl.org/popdata/entity/%s> 
            ppdt:scopedBy ?IdSet.
            ?IdSet ppdt:%s ?id.
    
            OPTIONAL {?IdSet ppdt:hasTemporalScope ?tempExtent. }
            OPTIONAL {?tempExtent ppdt:temporallyDefinedBy ?interval. }
            
            OPTIONAL {?interval time:hasBeginning ?beginningInstant. }
            OPTIONAL {?beginningInstant time:inXSDDate ?from. }
            
            OPTIONAL {?interval time:hasEnd ?EndInstant. }
            OPTIONAL {?EndInstant time:inXSDDate ?to. }
            
            }
            ", place_uuid, type)
  res <- SPARQL(endpoint_select, query)
  dat <- res$results
  if (nrow(dat)==0) return(dat)
  dat$from <- as.Date(as.POSIXct(dat$from, origin = '1970-01-01'))
  dat$to <- as.Date(as.POSIXct(dat$to, origin = '1970-01-01'))
  dat <- dat[order(dat$from, decreasing = TRUE),]
  return(dat)
}

getToponymalSettingByPlace <- function(place_uuid) {
  
  place_uuid <- formatUuid(place_uuid)
  
  query <-
    sprintf("
            PREFIX time: <http://www.w3.org/2006/time#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX ppdt: <http://purl.org/popdata#>
            SELECT DISTINCT ?topoSetLabel ?from ?to WHERE {
            ?TopoSet rdf:type ppdt:ToponymalSetting.
            <http://purl.org/popdata/entity/%s> 
            ppdt:scopedBy ?topoSet.
            ?topoSet rdfs:label ?topoSetLabel.
            FILTER (lang(?topoSetLabel) = 'it')
            
            OPTIONAL {?topoSet ppdt:hasTemporalScope ?tempExtent. }
            OPTIONAL {?tempExtent ppdt:temporallyDefinedBy ?interval. }
            
            OPTIONAL {?interval time:hasBeginning ?beginningInstant. }
            OPTIONAL {?beginningInstant time:inXSDDate ?from. }
            
            OPTIONAL {?interval time:hasEnd ?EndInstant. }
            OPTIONAL {?EndInstant time:inXSDDate ?to. }

            FILTER (lang(?topoSetLabel) = 'it')
            
            }
            ", place_uuid)
  res <- SPARQL(endpoint_select, query)
  dat <- res$results
  if (nrow(dat)==0) return(dat)
  dat$from <- as.Date(as.POSIXct(dat$from, origin = '1970-01-01'))
  dat$to <- as.Date(as.POSIXct(dat$to, origin = '1970-01-01'))
  dat <- dat[order(dat$from, decreasing = TRUE),]
  return(dat)
}

getHierarchicalSettingByPlace <- function(place_uuid) {
  
  place_uuid <- formatUuid(place_uuid)
  
  query <-
    sprintf("
            PREFIX time: <http://www.w3.org/2006/time#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX ppdt: <http://purl.org/popdata#>
            SELECT DISTINCT (?topoSetLabel AS ?part_of_label) ?from ?to WHERE {
            ?HierSet rdf:type ppdt:HierarchicalSetting.
            ?TopoSet rdf:type ppdt:ToponymalSetting.
            <http://purl.org/popdata/entity/%s> 
            ppdt:scopedBy ?HierSet.
            ?HierSet ppdt:partOf ?partOfPlace.
            ?partOfPlace ppdt:scopedBy ?topoSet.
            ?topoSet rdfs:label ?topoSetLabel.

            OPTIONAL {?HierSet ppdt:hasTemporalScope ?tempExtent. }
            OPTIONAL {?tempExtent ppdt:temporallyDefinedBy ?interval. }
            
            OPTIONAL {?interval time:hasBeginning ?beginningInstant. }
            OPTIONAL {?beginningInstant time:inXSDDate ?from. }
            
            OPTIONAL {?interval time:hasEnd ?EndInstant. }
            OPTIONAL {?EndInstant time:inXSDDate ?to. }

            FILTER (lang(?topoSetLabel) = 'it')
            }
            ", place_uuid)
  res <- SPARQL(endpoint_select, query)
  dat <- res$results
  if (nrow(dat)==0) return(dat)
  dat$from <- as.Date(as.POSIXct(dat$from, origin = '1970-01-01'))
  dat$to <- as.Date(as.POSIXct(dat$to, origin = '1970-01-01'))
  dat <- dat[order(dat$from, decreasing = TRUE),]
  return(dat)
}

# SERVICE

getHierTemporalScope <- function(place_type_label) {
  
  place_type_uuid <- names(place_type_dict)[match(place_type_label, place_type_dict)]
  
  if (is.na(place_type_uuid)) stop("Unrecognised label")
  
  query <- sprintf("PREFIX time: <http://www.w3.org/2006/time#>
                   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                   PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                   PREFIX ppdt: <http://purl.org/popdata#>
                   SELECT DISTINCT ?place ?partOf (STR(?from) AS ?from_str) (STR(?to) AS ?to_str) WHERE {
                   
                   ?hierSet rdf:type ppdt:HierarchicalSetting.
                   ?place ppdt:scopedBy ?hierSet.
                   
                   ?hierSet ppdt:partOf ?partOf.
                   
                   ?typoSet rdf:type ppdt:TypologicalSetting.
                   ?place ppdt:scopedBy ?typoSet.
                   
                   ?typoSet ppdt:hasType <http://purl.org/popdata/entity/%s>.
                   
                   OPTIONAL {?hierSet ppdt:hasTemporalScope ?tempExtent. }
                   OPTIONAL {?tempExtent ppdt:temporallyDefinedBy ?interval.}
                   
                   OPTIONAL {?interval time:hasBeginning ?beginningInstant.}
                   OPTIONAL {?interval time:hasEnd ?endInstant.}
                   
                   OPTIONAL {?beginningInstant time:inXSDDate ?from}
                   OPTIONAL {?endInstant time:inXSDDate ?to}   
                   }", place_type_uuid)
  
  res <- SPARQL(endpoint_select, query, curl_args = list(.encoding = 'UTF-8'))
  dat <- res$results
  dat$place <- sapply(dat$place, formatUuid, USE.NAMES = F)
  dat$partOf <- sapply(dat$partOf, formatUuid, USE.NAMES = F)
  return(dat)
}

getTypoTemporalScope <- function(place_type_label) {
  
  place_type_uuid <- names(place_type_dict)[match(place_type_label, place_type_dict)]
  
  if (is.na(place_type_uuid)) stop("Unrecognised label")
  
  query <- sprintf('PREFIX time: <http://www.w3.org/2006/time#>
                  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                   PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                   PREFIX ppdt: <http://purl.org/popdata#>
                   SELECT DISTINCT ?place (STR(?from) AS ?from_str) (STR(?to) AS ?to_str) WHERE {
                   
                   ?typoSet rdf:type ppdt:TypologicalSetting.
                   ?place ppdt:scopedBy ?typoSet.
                   
                   ?typoSet ppdt:hasType <http://purl.org/popdata/entity/%s>.
                   
                   OPTIONAL {?typoSet ppdt:hasTemporalScope ?tempExtent. }
                   OPTIONAL {?tempExtent ppdt:temporallyDefinedBy ?interval.}
                   
                   OPTIONAL {?interval time:hasBeginning ?beginningInstant.}
                   OPTIONAL {?interval time:hasEnd ?endInstant.}
                   
                   OPTIONAL {?beginningInstant time:inXSDDate ?from}
                   OPTIONAL {?endInstant time:inXSDDate ?to}   
                   
                   }',place_type_uuid)
  
  res <- SPARQL(endpoint_select, query)
  dat <- res$results
  dat$place <- sapply(dat$place, formatUuid, USE.NAMES = F)
  return(dat)
}

getTopoTemporalScope <- function(place_type_label) {
  
  place_type_uuid <- names(place_type_dict)[match(place_type_label, place_type_dict)]
  
  if (is.na(place_type_uuid)) stop("Unrecognised label")
  
  query <- sprintf("PREFIX time: <http://www.w3.org/2006/time#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX ppdt: <http://purl.org/popdata#>
  SELECT DISTINCT ?place ?topoSetLabel (STR(?from) AS ?from_str) (STR(?to) AS ?to_str) WHERE {
  
  ?topoSet rdf:type ppdt:ToponymalSetting.
  ?place ppdt:scopedBy ?topoSet.

  ?topoSet rdfs:label ?topoSetLabel.
  FILTER (lang(?topoSetLabel) = 'it')

  ?typoSet rdf:type ppdt:TypologicalSetting.
  ?place ppdt:scopedBy ?typoSet.
  
  ?typoSet ppdt:hasType <http://purl.org/popdata/entity/%s>.
  
  OPTIONAL {?topoSet ppdt:hasTemporalScope ?tempExtent. }
  OPTIONAL {?tempExtent ppdt:temporallyDefinedBy ?interval.}
  
  OPTIONAL {?interval time:hasBeginning ?beginningInstant.}
  OPTIONAL {?interval time:hasEnd ?endInstant.}
  
  OPTIONAL {?beginningInstant time:inXSDDate ?from}
  OPTIONAL {?endInstant time:inXSDDate ?to}   
  }", place_type_uuid)
  
  res <- SPARQL(endpoint_select, query, curl_args = list(.encoding = 'UTF-8'))
  dat <- res$results
  dat$place <- sapply(dat$place, formatUuid, USE.NAMES = F)
  return(dat)
}

getComuniOnDate <- function(date){
  date <- as.Date(date)
  # Comuni
  comuni_typo_scope <- getTypoTemporalScope('comune')
  comuni_typo_scope <- subset(comuni_typo_scope, (from_str <= date) & (is.na(to_str) | to_str >= date) )
  comuni_topo_scope <-  getTopoTemporalScope('comune')
  comuni_topo_scope <- subset(comuni_topo_scope, (from_str <= date) & (is.na(to_str) | to_str >= date) )
  comuni_heir_scope <- getHierTemporalScope('comune')
  comuni_heir_scope <- subset(comuni_heir_scope, (from_str <= date) & (is.na(to_str) | to_str >= date) )
  comuni_on_date <- merge(comuni_typo_scope, comuni_topo_scope, by = 'place', all = TRUE)
  comuni_on_date <- merge(comuni_on_date, comuni_heir_scope, by = 'place', all = TRUE)
  
  # Province
  province_typo_scope <-  getTypoTemporalScope('provincia')
  province_typo_scope <- subset(province_typo_scope, (from_str <= date) & (is.na(to_str) | to_str >= date) )
  province_topo_scope <-  getTopoTemporalScope('provincia')
  province_topo_scope <- subset(province_topo_scope, (from_str <= date) & (is.na(to_str) | to_str >= date) )
  province_heir_scope <- getHierTemporalScope('provincia')
  province_heir_scope <- subset(province_heir_scope, (from_str <= date) & (is.na(to_str) | to_str >= date) )
  province_on_date <- merge(province_typo_scope, province_topo_scope, by = 'place', all = TRUE)
  province_on_date <- merge(province_on_date, province_heir_scope, by = 'place', all = TRUE)
  
  # Regioni
  regioni_typo_scope <-  getTypoTemporalScope('regione')
  regioni_typo_scope <- subset(regioni_typo_scope, (from_str <= date) & (is.na(to_str) | to_str >= date) )
  regioni_topo_scope <-  getTopoTemporalScope('regione')
  regioni_topo_scope <- subset(regioni_topo_scope, (from_str <= date) & (is.na(to_str) | to_str >= date) )
  regioni_on_date <- merge(regioni_typo_scope, regioni_topo_scope, by = 'place', all = TRUE)

  province_on_date <- province_on_date[,c('place','topoSetLabel','partOf')]
  names(province_on_date) <- c("provinciaUuid", "provinciaLabel", "regioneUuid")
  
  regioni_on_date <- regioni_on_date[c('place','topoSetLabel')]
  names(regioni_on_date) <- c("regioneUuid", "regioneLabel")
  
  province_on_date <- merge(province_on_date, regioni_on_date, by = 'regioneUuid', all = TRUE)
  
  comuni_on_date <- comuni_on_date[,c("place","topoSetLabel","partOf")]
  names(comuni_on_date) <- c('comuneUuid', 'comuneLabel', 'provinciaUuid')
  
  comuni_on_date <- merge(comuni_on_date, province_on_date, by = 'provinciaUuid', all = TRUE)
  comuni_on_date <- 
    comuni_on_date[,c('comuneLabel', 'provinciaLabel', 'regioneLabel', 'comuneUuid', 'provinciaUuid', 'regioneUuid')]
  
  return(comuni_on_date)
  
}

getGeoTemporalScope <- function(place_type_label) {
  
  place_type_uuid <- names(place_type_dict)[match(place_type_label, place_type_dict)]
  
  if (is.na(place_type_uuid)) stop("Unrecognised label")
  
  query <- sprintf("PREFIX time: <http://www.w3.org/2006/time#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX ppdt: <http://purl.org/popdata#>
  PREFIX geo: <http://www.opengis.net/ont/geosparql#>
  SELECT DISTINCT ?place ?multipolygon ?wkt ?source (STR(?from) AS ?from_str) (STR(?to) AS ?to_str) WHERE {
  ?geoSet rdf:type ppdt:GeographicalSetting.
  ?place ppdt:scopedBy ?geoSet.

  ?typoSet rdf:type ppdt:TypologicalSetting.
  ?place ppdt:scopedBy ?typoSet.
  ?typoSet ppdt:hasType <http://purl.org/popdata/entity/%s>.
  
  ?geoSet ppdt:hasSpatialScope ?spatialExtent.
  ?spatialExtent ppdt:spatiallyDefinedBy ?feature.
  ?feature ppdt:hasExactGeometry ?multipolygon.
  ?multipolygon geo:asWKT ?wkt.
  ?multipolygon ppdt:hasSource ?source.
  
  OPTIONAL {?geoSet ppdt:hasTemporalScope ?tempExtent. }
  OPTIONAL {?tempExtent ppdt:temporallyDefinedBy ?interval. }
  
  OPTIONAL {?interval time:hasBeginning ?beginningInstant. }
  OPTIONAL {?beginningInstant time:inXSDDate ?from. }
  
  OPTIONAL {?interval time:hasEnd ?EndInstant. }
  OPTIONAL {?EndInstant time:inXSDDate ?to. }
                   }", place_type_uuid)
  
  res <- SPARQL(endpoint_select, query, curl_args = list(.encoding = 'UTF-8'))
  dat <- res$results
  dat$place <- sapply(dat$place, formatUuid, USE.NAMES = F)
  dat$source <- sapply(dat$source, formatUuid, USE.NAMES = F)
  dat$multipolygon <- sapply(dat$multipolygon, formatUuid, USE.NAMES = F)
  return(dat)
  
}

getComuniGeoOnDate <- function(date) {
  require(sp)
  require(rgeos)
  
  date <- as.Date(date)
  
  comuni_geo_scope <- getGeoTemporalScope('comune')
  comuni_geo_scope <- subset(comuni_geo_scope, (from_str <= date) & (is.na(to_str) | to_str >= date) )
  
  tmp_sp <- comuni_geo_scope
  tmp_sp$wkt <- gsub("\"<http://www\\.opengis\\.net/def/crs/EPSG/0/4326> |\"\\^\\^<http://www\\.opengis\\.net/ont/geosparql#wktLiteral>", "", tmp_sp$wkt)
  tmp_sp <- mapply(FUN = function(x,y) readWKT(x, id = y), tmp_sp$wkt, as.character(1:nrow(tmp_sp)))
  tmp_sp <- SpatialPolygons(lapply(tmp_sp, function(x){x@polygons[[1]]}))
  tmp_sp <- SpatialPolygonsDataFrame(tmp_sp, data = comuni_geo_scope[,c('place','multipolygon','source')], match.ID = FALSE)
  
  return(tmp_sp)

}

getIdeTemporalScope <- function(place_type_label, identification_type) {
  
  place_type_uuid <- names(place_type_dict)[match(place_type_label, place_type_dict)]
  if (is.na(place_type_uuid)) stop("Unrecognised label")
  if (is.na(identification_type) | !identification_type %in% getAllIdTypes())  stop("Unrecognised identification")
  
  query <- sprintf("PREFIX time: <http://www.w3.org/2006/time#>
                   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                   PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                   PREFIX ppdt: <http://purl.org/popdata#>
                   SELECT DISTINCT ?place ?istatId (STR(?from) AS ?from_str) (STR(?to) AS ?to_str) WHERE {
                   
                   ?IdSet rdf:type ppdt:IdentificationalSetting.
                   ?place ppdt:scopedBy ?IdSet.
                   
                   ?IdSet ppdt:%s ?istatId.
                   
                   ?typoSet rdf:type ppdt:TypologicalSetting.
                   ?place ppdt:scopedBy ?typoSet.
                   
                   ?typoSet ppdt:hasType <http://purl.org/popdata/entity/%s>.
                   
                   OPTIONAL {?IdSet ppdt:hasTemporalScope ?tempExtent. }
                   OPTIONAL {?tempExtent ppdt:temporallyDefinedBy ?interval.}
                   
                   OPTIONAL {?interval time:hasBeginning ?beginningInstant.}
                   OPTIONAL {?interval time:hasEnd ?endInstant.}
                   
                   OPTIONAL {?beginningInstant time:inXSDDate ?from}
                   OPTIONAL {?endInstant time:inXSDDate ?to}   
                   }", identification_type, place_type_uuid)
  
  res <- SPARQL(endpoint_select, query, curl_args = list(.encoding = 'UTF-8'))
  dat <- res$results
  dat$place <- sapply(dat$place, formatUuid, USE.NAMES = F)
  return(dat)
}

# 

# res <- getComuniGeoOnDate("2013-02-24")
# res <- getComuniTopoTemporalScope()


# res <- getGeographicalSettingByPlace('349B971A-3AB3-41D2-A090-2BE62F964CC4')


