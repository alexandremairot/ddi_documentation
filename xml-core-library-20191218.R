#############################################################
# name: xml-core-library.R                                  #
# author: Alexandre Mairot (alexandre.mairot@sciencespo.fr) #
# title: functions for manipulating xml files               #
# program: R                                                #
# version: 20191218                                         #
#############################################################
#############################################################
# Declaration of functions                                  #
#############################################################
require(xml2)
#############################################################
xmlcore.input <- function (xmlField){
  ######
  # FUNCTION: transform xml list from xml2 to matrix format
  ######
  ######
  # NOTE: subfunction
  ######
  xmlcore.parser <- function (xmlField, depth=0, xmlFrame=matrix (NA, ncol = 5, nrow = 0)){
    ######
    # FUNCTION: parser xml list from xml2
    ######
    if (nrow (xmlFrame) > 0){
      xmlMulti <- nrow (xmlFrame) + 1
    } else {
      xmlMulti <- 1
    }
    ### Gestion des attributs des balises
    if (xml_attrs (xmlField) [1] != "character(0)") {
      if (is.list (xml_attrs (xmlField))){
        for (i in 1 : length (xml_attrs (xmlField)[[1]])){
          xmlFrame <- rbind (xmlFrame, c (xml_name (xmlField), depth, xmlMulti, names (xml_attrs(xmlField)[[1]][i]), as.character (xml_attrs(xmlField)[[1]][i])))
        }
      }
      else {
        for (i in 1 : length (xml_attrs (xmlField))){
          xmlFrame <- rbind (xmlFrame, c (xml_name (xmlField), depth, xmlMulti, names (xml_attrs (xmlField)[i]), as.character (xml_attrs (xmlField)[[i]])))
        }
      }
    } else {
      if (length (xml_contents (xmlField)[which (xml_type (xml_contents (xmlField)) %in% c ("text","cdata"))]) == 0){
        if (nchar (xml_name (xmlField))>0){
          xmlFrame <- rbind (xmlFrame, c (xml_name (xmlField), depth, xmlMulti, NA, NA))
        }
      }
    }
    ### Gestion des contenus des balises
    if (length (xml_contents (xmlField))>0){
      for (i in 1 : length (xml_contents (xmlField))){
        if (xml_type (xml_contents (xmlField)[i]) == "text"){
          xmlFrame <- rbind (xmlFrame, c (xml_name (xmlField), depth, xmlMulti, NA, xml_text (xml_contents (xmlField)[i], trim = TRUE)))
        } else {
          if (nchar (xml_name (xml_contents (xmlField)[i]))==0){
            if (xml_type (xml_contents (xmlField)[i])=="cdata"){
              xmlFrame <- rbind (xmlFrame, c (xml_name (xmlField), depth, xmlMulti, NA, xml_text (xml_contents (xmlField)[i], trim = TRUE)))
            }
          } else {
            xmlFrame <- xmlcore.parser (xml_contents (xmlField)[i], (depth+1), xmlFrame)
          }
        }
      }
    }
    colnames (xmlFrame) <-c ("xml_tag", "depth", "range", "attribut","value")
    xmlFrame
  }
  ######
  # NOTE: begin the program
  ######  
  xmlInput <- xmlcore.parser(xmlField)
  xmlTab <- cbind (xmlInput, matrix (NA, ncol = 1, nrow = nrow(xmlInput)))
  rm(xmlInput)
  strPath <- ""
  for (i in 1 : nrow (xmlTab)){
    if (xmlTab[i, 2] == 0){
      strPath <- paste0 (xmlTab[i, 1])
    }else{
      testStr <- 0
      j <- 1
      while ((j < nchar (strPath)) && (testStr < xmlTab[i, 2])){
        if (substring (strPath, j, j) == '/'){
          testStr <- testStr + 1
        }
        j <- j + 1
      }
      if (j == nchar (strPath)){
        strPath <- paste0 (strPath,"/")
      } else {
        strPath <- substring (strPath,1,j-1)
      }
      strPath <- paste0 (strPath, paste0 (xmlTab[i, 1]))
    }
    xmlTab[i, 6] <- paste0 (strPath)
    if ( ! (is.na (xmlTab[i,4]))){
      xmlTab[i, 6] <- paste0 (xmlTab[i, 6], "-", xmlTab[i, 4])
    }
  }
  colnames (xmlTab) <-c ("xml_tag", "depth", "range","attribut","value", "xpath")
  xmlTab
}
#############################################################
xmlcore.new <- function (xmlScheme, xmlField, xmlValue=NA, xmlAttrib=NA, attrValues=NA){
  ######
  # FUNCTION: create a xml matrix node
  ######
  ######
  # NOTE: check if the xmlField is registred in the xmlScheme
  ######
  if (xmlField %in% xmlScheme[,2]){
    if (length(xmlAttrib)==1){
      if (!(is.na(xmlAttrib))){
        if (!(FALSE %in% (xmlAttrib %in% as.character(xmlScheme[xmlScheme[,2]==xmlField,c(3:ncol(xmlScheme))])))){
          testOK <- 1
        }
      } else {
        testOK <- 1
      }
    }else{
      if (!(FALSE %in% (xmlAttrib %in% as.character(xmlScheme[xmlScheme[,2]==xmlField,c(3:ncol(xmlScheme))])))){
        testOK <- 1
      }
    }
  }
  ######
  # NOTE: creation of the matrix variable
  ######
  if (exists("testOK")){
    kDepth <- 0
    for (k in 1 : nchar(xmlField)){
      if (substring(xmlField,k,k)=="/"){
        kDepth <- kDepth + 1
      }
    }
    if (kDepth > 0){
      k <- nchar(xmlField)
      while (substring(xmlField,k,k)!="/"){
        k <- k - 1
      }
      kTaq <- substring(xmlField, k+1, nchar(xmlField))
    } else {
      kTaq <- xmlField
    }
    if (!(is.na(xmlAttrib[1]))){
      for(k in 1:length(xmlAttrib)){
        if (!(is.na(attrValues[k]))){
          if (k == 1) {
            xmlInsert <- c(kTaq,kDepth,NA, xmlAttrib[k],attrValues[k],paste0(xmlField,"-",xmlAttrib[k]))
          } else {
            xmlInsert <- rbind(xmlInsert, c(kTaq,kDepth,NA, xmlAttrib[k],attrValues[k], paste0(xmlField,"-",xmlAttrib[k])))
          }
        }
      }
    }
    if (exists("xmlInsert")){
      if (!(is.na(xmlValue[1]))){
        xmlInsert <- rbind(xmlInsert, c(kTaq,kDepth,NA,NA, xmlValue, xmlField))
      }
    } else {
      xmlInsert <- c(kTaq,kDepth,NA,NA, xmlValue, xmlField)
    }
  } else {
    xmlInsert <- NA
  }
  if (length(xmlInsert)==6){
    xmlFrame=matrix (NA, ncol = 6, nrow = 1)
    for (i in 1:6){
      xmlFrame[1,i] <- xmlInsert[i]
    }
    xmlInsert <- xmlFrame
  }
  xmlInsert[,3] <- 1
  colnames (xmlInsert) <-c ("xml_tag", "depth", "range","attribut","value", "xpath")
  xmlInsert
}
#############################################################
xmlcore.tree <- function (xmlScheme){
  ######
  # FUNCTION: transform xmlScheme to a developped/tree xml scheme version 
  ######  
  for (i in 1:nrow(xmlScheme)){
    if (exists("xmlSchemeFull")){
      xmlSchemeFull <- rbind(xmlSchemeFull, c(xmlScheme$field[i], xmlScheme$field[i], xmlScheme$field_num[i]))
    } else {
      xmlSchemeFull <- c(xmlScheme$field[i], xmlScheme$field[i],xmlScheme$field_num[i])
    }
    for (j in 3:ncol(xmlScheme)){
      if (nchar(xmlScheme[i,j])>0){
        xmlSchemeFull <- rbind(xmlSchemeFull, c(xmlScheme$field[i], paste0(xmlScheme$field[i],"-",xmlScheme[i,j]),xmlScheme$field_num[i]))
      }
    }
  }
  xmlSchemeFull <- xmlSchemeFull[,c(2,1,3)]
  colnames(xmlSchemeFull)<- c("xpath","fieldpath","fieldnum")
  xmlSchemeFull
}
#############################################################
xmlcore.add <- function (xmlScheme, xmlTab, xmlAdd){
  ######
  # FUNCTION: insert xmlAdd in xmlTab respecting the xmlScheme 
  # DEPENDENCIES: xmlcore.tree, xmlcore.new, xmlcore.add
  ######
  ######
  # NOTE: check the "root" field
  ######
  xmlTab2 <- merge(data.frame(xmlTab, stringsAsFactors = FALSE), data.frame(xmlcore.tree(xmlScheme), stringsAsFactors = FALSE), sort=FALSE)
  xmlTab2$range <- as.numeric(xmlTab2$range) 
  xmlTab2 <- xmlTab2[order(xmlTab2$range),]
  xmlTab2 <- as.matrix(xmlTab2[, c(2:6,1,7,8)])
  xmlAdd2 <- merge(data.frame(xmlAdd, stringsAsFactors = FALSE), data.frame(xmlcore.tree(xmlScheme), stringsAsFactors = FALSE), sort=FALSE)
  xmlAdd2$range <- as.numeric(xmlAdd2$range) 
  xmlAdd2 <- xmlAdd2[order(xmlAdd2$range),]
  xmlAdd2 <- as.matrix(xmlAdd2[, c(2:6,1,7,8)]) 
  if (xmlTab2[1,8]>xmlAdd2[1,8]){
    xmlTmp <- xmlTab
    xmlTab <- xmlAdd
    xmlAdd <- xmlTmp
    xmlTab2 <- xmlAdd2
    rm("xmlTmp")
  }
  ######
  # NOTE: write the "missing" field
  ######
  if (substring(xmlAdd[1,6],1,nchar(xmlTab2[1,7]))==xmlTab2[1,7]){
    i <- nchar(xmlTab2[1,7]) + 2
    while (i <= nchar(xmlAdd[1,6])){
      j <- i
      while ((substring(xmlAdd[1,6], j, j) != '/')&&(substring(xmlAdd[1,6], j, j) != '-')&&(j<=nchar(xmlAdd[1,6]))){
        j <- j + 1
      }
      if(substring(xmlAdd[1,6],1,j-1) != xmlAdd2[1,7]){
        if ((!(substring(xmlAdd[1,6],1,j-1) %in% unique(xmlTab2[,7])))&&(j<nchar(xmlAdd[1,6]))){
          xmlTab <- xmlcore.add(xmlScheme, xmlTab, xmlcore.new(xmlScheme,substring(xmlAdd[1,6],1,j-1)))
          xmlTab2 <- merge(data.frame(xmlTab, stringsAsFactors = FALSE), data.frame(xmlcore.tree(xmlScheme), stringsAsFactors = FALSE), sort=FALSE)
          xmlTab2$range <- as.numeric(xmlTab2$range) 
          xmlTab2 <- xmlTab2[order(xmlTab2$range),]
          xmlTab2 <- as.matrix(xmlTab2[, c(2:6,1,7,8)])
        }
      }
      i <- j + 1
    }
    ######
    # NOTE: insertion xmlAdd into xmlTab
    ######
    k <- 1 
    flag <- 0
    while((as.numeric(xmlTab2[k,8]) < xmlScheme[(xmlScheme$field == xmlAdd2[1,7]),1])&&(flag==0)){
      if ((k+1)>nrow(xmlTab2)){
        flag <- 1
      } else {
        k <- k + 1
      }
    }
    if (flag == 1){
      for (i in 1:nrow(xmlAdd)){
        xmlAdd[i,3] <- as.numeric(xmlAdd[i,3]) + nrow(xmlTab)
      }
      xmlTab <- rbind(xmlTab,xmlAdd)
    } else {
      for (i in k:nrow(xmlTab)){
        xmlTab[i,3] <- as.numeric(xmlTab[i,3]) + nrow(xmlAdd)
      }
      for (i in 1:nrow(xmlAdd)){
        xmlAdd[i,3] <- as.numeric(xmlAdd[i,3]) + k - 1
      }
      xmlTab <- rbind(xmlTab[1:k-1,],xmlAdd,xmlTab[k:nrow(xmlTab),])
    } 
  }
  rownames(xmlTab) <- 1 : nrow(xmlTab)
  xmlTab
}
#############################################################
xmlcore.output <- function (xmlTab){
  ######
  # FUNCTION: transform xml matrix to xml list for xml2
  ######
  ######
  # NOTE: subfunction
  ######
  xmlcore.node  <- function (xmlTab, start, end){
    ######
    # FUNCTION: create xml2 Node
    ######
    nodeXml <- xml_new_root (xmlTab[start,1])
    for (i in  start : end){
      if (!(is.na(xmlTab[i,4]))){
        if (!(xmlTab[i,5] %in% names(xml_attrs(nodeXml)))){
          xml_attr (nodeXml, attr=paste0(xmlTab[i,4])) <- xmlTab[i,5]
        }
      } else {
        if ( ! (is.na (xmlTab[i,5]))){
          if (length(grep(pattern="\n", xmlTab[i,5])) > 0){
            xml_add_child(nodeXml, xml_cdata(xmlTab[i,5]))
          } else {
            xml_text(nodeXml) <- xmlTab[i,5]
          }
        }
      }
    }
    nodeXml
  }
  ######
  # NOTE: begin the program
  ###### 
  i <- 1
  repeat{
    if (i <= nrow (xmlTab)){
      j <- i + 1
      repeat {
        if (j > nrow(xmlTab)){
          break
        }
        if (xmlTab[j,3] == xmlTab[i,3]){
          j <- j + 1
        } else {
          break
        }
      }
      if (i == 1) {
        depthInit <- as.numeric(xmlTab[i,2])
        depthsav <- depthInit
        assign(paste0("xmlT_",xmlTab[i,2]), xmlcore.node(xmlTab, i, j-1))
      } else {
        if (as.numeric(xmlTab[i,2])> depthsav){
          assign(paste0("xmlT_",xmlTab[i,2]), xmlcore.node(xmlTab, i, j-1))
          depthsav <- as.numeric(xmlTab[i,2])
        } else {
          repeat{
            if (as.numeric(xmlTab[i,2])==depthsav){
              xml_add_child(get(paste0("xmlT_",(as.numeric(xmlTab[i,2])-1))), get(paste0("xmlT_",xmlTab[i,2])))
              assign(paste0("xmlT_",xmlTab[i,2]), xmlcore.node(xmlTab, i, j-1))
              break
            } else {
              xml_add_child(get(paste0("xmlT_",(depthsav-1))), get(paste0("xmlT_",depthsav)))
              rm(list=paste0("xmlT_",depthsav))
              depthsav <- depthsav - 1
            }
          }
        }
      }
      i <- j
    } else {
      break
    }
  }
  repeat{
    if (depthsav > depthInit) {
      xml_add_child(get(paste0("xmlT_",(depthsav-1))), get(paste0("xmlT_",depthsav)))
      depthsav <- depthsav - 1
    } else {
      break
    }
  }    
  get(paste0("xmlT_", depthInit))
}
#############################################################
xmlcore.remove <- function (xmlTab, range){
  ######
  # FUNCTION: remove the xml field designed by the range level and its children
  ######
  i <- 1
  repeat{
    if ((as.numeric(xmlTab[i,3])<range)&&(i<=nrow(xmlTab))){
      i <- i+1
    }else{
      break
    }
  }
  if (as.numeric(xmlTab[i,3])==range){
    j <- i
    depth <- as.numeric(xmlTab[i,2])
    repeat{
      if (j <= nrow(xmlTab)){
        if (as.numeric(xmlTab[j,3])==range){
          j <- j + 1
        } else {
          if (as.numeric(xmlTab[j,2])>depth){
            j <- j + 1
          } else {
            break
          }
        }
      } else {
        break
      }
    }
  }
  xmlT1 <- xmlTab[c(1:(i-1)),]
  if (j < nrow(xmlTab)){
    xmlT2 <- xmlTab[c(j:nrow(xmlTab)),]
    delta <- (as.numeric(xmlT2[1,3]) - as.numeric(xmlTab[i,3]))
    xmlT2[,3] <- as.numeric(xmlT2[ ,3]) - delta
    xmlT1 <- rbind(xmlT1,xmlT2)
  } 
  xmlT1  
}
#############################################################