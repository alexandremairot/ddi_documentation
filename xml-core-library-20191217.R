#############################################################
# name: election-ddic-generator.R                           #
# author: Alexandre Mairot (alexandre.mairot@sciencespo.fr) #
# title: program for generating ddic files                  #
# program: R                                                #
# version: 20191204                                         #
#############################################################
#############################################################
# Declaration of functions                                  #
#############################################################
require(xml2)
#############################################################
xml2Parse <- function (xmlField, depth=0, xmlFrame=matrix (NA, ncol = 5, nrow = 0)){
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
          xmlFrame <- xml2Parse (xml_contents (xmlField)[i], (depth+1), xmlFrame)
        }
      }
    }
  }
  colnames (xmlFrame) <-c ("xml_tag", "depth", "range", "attribut","value")
  xmlFrame
}
#############################################################
xml2Path <- function (xmlTab){
  xmlTab <- cbind (xmlTab, matrix (NA, ncol = 1, nrow = nrow(xmlTab)))
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
xml2Write <- function(xmlTab){
  createNode  <- function(xmlTab, start, end){
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
      if (xmlTab[i,2] == 0){
        xmlTree <- createNode(xmlTab, i, j-1)
      }
      if (xmlTab[i,2]  == 1) {
        xml_add_child (xmlTree, createNode(xmlTab, i, j-1))
        size1 <- length (xml_children (xmlTree))
      }
      if (xmlTab[i,2]  == 2) {
        xml_add_child (xml_children (xmlTree) [size1], createNode(xmlTab, i, j-1))
        size2 <- length (xml_children (xml_children (xmlTree)[size1]))
      }
      if (xmlTab[i,2]  == 3) {
        xml_add_child (xml_children (xml_children (xmlTree) [size1]) [size2], createNode(xmlTab, i, j-1))
        size3 <- length (xml_children (xml_children (xml_children (xmlTree)[size1]) [size2]))
      }
      if (xmlTab[i,2]  == 4){
        xml_add_child (xml_children (xml_children (xml_children (xmlTree)[size1]) [size2]) [size3], createNode(xmlTab, i, j-1))
        size4 <- length (xml_children (xml_children (xml_children (xml_children (xmlTree)[size1]) [size2]) [size3]))
      }
      if (xmlTab[i,2]  == 5){
        xml_add_child (xml_children (xml_children (xml_children (xml_children (xmlTree)[size1]) [size2]) [size3]) [size4], createNode(xmlTab, i, j-1))
        size5 <- length (xml_children (xml_children (xml_children (xml_children (xml_children (xmlTree)[size1]) [size2]) [size3]) [size4]))
      }  
      if (xmlTab[i,2]  == 6){
        xml_add_child (xml_children (xml_children (xml_children (xml_children (xml_children (xmlTree)[size1]) [size2]) [size3]) [size4]) [size5], createNode(xmlTab, i, j-1))
        size6 <- length (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xmlTree)[size1]) [size2]) [size3]) [size4]) [size5]))
      }
      if (xmlTab[i,2]  == 7){
        xml_add_child (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xmlTree)[size1]) [size2]) [size3]) [size4]) [size5]) [size6], createNode(xmlTab, i, j-1))
        size7 <- length (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xmlTree)[size1]) [size2]) [size3]) [size4]) [size5]) [size6]))
      }
      if (xmlTab[i,2]  == 8){
        xml_add_child (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xmlTree)[size1]) [size2]) [size3]) [size4]) [size5]) [size6]) [size7], createNode(xmlTab, i, j-1))
        size8 <- length (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xmlTree)[size1]) [size2]) [size3]) [size4]) [size5]) [size6]) [size7]))
      }
      if (xmlTab[i,2]  == 9){
        xml_add_child (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xmlTree)[size1]) [size2]) [size3]) [size4]) [size5]) [size6]) [size7]) [size8], createNode(xmlTab, i, j-1))
        size9 <- length (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xmlTree)[size1]) [size2]) [size3]) [size4]) [size5]) [size6]) [size7]) [size8]))
      }
      if (xmlTab[i,2]  == 10){
        xml_add_child (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xmlTree)[size1]) [size2]) [size3]) [size4]) [size5]) [size6]) [size7]) [size8]) [size9], createNode(xmlTab, i, j-1))
        size10 <- length (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xml_children (xmlTree)[size1]) [size2]) [size3]) [size4]) [size5]) [size6]) [size7]) [size8]) [size9]))
      }  
      i <- j
    } else {
      break
    }
  }
  xmlTree
}
#############################################################
xmlcore.new <- function (xmlScheme, xmlField, xmlValue=NA, xmlAttrib=NA, attrValues=NA){
  # Old name: xml2Create
  ######
  # Create a xml node in adapted matrix format
  ######
  # xmlScheme: scheme of XML followed by the xml field will be created by the function
  # xmlField: xpath of the xml field
  # xmlValue: value of the xml field
  # xmlAttrib: list of the attributs of the xml field
  # attrValues: list of the values of the attributs of the xml field
  ######
  ######
  # Check if the xmlField is registred in the xmlScheme
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
  # Creation of the matrix variable
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
  # old name : xml2SchemeFull
  ######
  # Transform the condenced xml scheme to a developped xml scheme
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
#xml2joinScheme <- function (xmlTab, xmlScheme){
#  xmlTab <- merge(data.frame(xmlTab, stringsAsFactors = FALSE), data.frame(xml2SchemeFull(xmlScheme), stringsAsFactors = FALSE), sort=FALSE)
#  xmlTab$range <- as.numeric(xmlTab$range) 
#  xmlTab <- xmlTab[order(xmlTab$range),]
#  xmlTab <- as.matrix(xmlTab[, c(2:6,1,7,8)])
#  xmlTab
#}
#############################################################
#xml2Add <- function (xmlTab, xmlScheme, xmlField, xmlValue=NA, xmlAttrib=NA,attrValues=NA){
xmlcore.add <- function (xmlScheme, xmlTab, xmlAdd){
  ######
  # Add children xmlField defined in the call of the function
  # Need to know the scheme of XML used and it manage it
  # Dependencies: xml2SchemeFull, xmlcore.create
  ######
  ######
  # check the "root" field
  ######
  xmlTab2 <- merge(data.frame(xmlTab, stringsAsFactors = FALSE), data.frame(xmlcore.tree(xmlScheme), stringsAsFactors = FALSE), sort=FALSE)
  xmlTab2$range <- as.numeric(xmlTab2$range) 
  xmlTab2 <- xmlTab2[order(xmlTab2$range),]
  xmlTab2 <- as.matrix(xmlTab2[, c(2:6,1,7,8)])
  ######
  xmlAdd2 <- merge(data.frame(xmlAdd, stringsAsFactors = FALSE), data.frame(xmlcore.tree(xmlScheme), stringsAsFactors = FALSE), sort=FALSE)
  xmlAdd2$range <- as.numeric(xmlAdd2$range) 
  xmlAdd2 <- xmlAdd2[order(xmlAdd2$range),]
  xmlAdd2 <- as.matrix(xmlAdd2[, c(2:6,1,7,8)]) 
  ######
  if (xmlTab2[1,8]>xmlAdd2[1,8]){
    xmlTmp <- xmlTab
    xmlTab <- xmlAdd
    xmlAdd <- xmlTmp
    xmlTab2 <- xmlAdd2
    rm("xmlTmp")
  }
  ######
  # write the "missing" field
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
    # Insertion xmlAdd
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
    #xmlAdd[,3] <- NA
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
#lcount <- 0
#lrange <- 0



#for (l in 1:nrow(xmlTab)){
#  if (is.na(xmlTab[l,3])){
#    if (lrange==0) {
#      lrange <- l
#    }
#    lcount <- lcount + 1
#    xmlTab[l,3] <- lrange
#  } else {
#    if ((lrange>0)&&(l>lrange)){
#      xmlTab[l,3] <- as.numeric(xmlTab[l,3])+lcount
#    }
#  }
#}
#############################################################
xml2Extract <- function (xmlTab, xmlPath){
  # Old name: xml2Extract
  ######
  # Extract xml part from xmlField
  ######
  i <- 1
  occ <- 1
  while (i <=nrow(xmlTab)){
    if (substring(xmlTab[i ,6] ,1, nchar(xmlPath)) == xmlPath){
      j <-  i
      escape <- 0
      while (escape == 0){
        if (j <= nrow(xmlTab)){
          if (xmlTab[j,3] == xmlTab[i,3]){
            j <- j + 1
          } else {
            escape <- 1
          }
        } else {
          escape <- 1
        }
      }
      escape <- 0
      while (escape == 0){
        if (j <= nrow(xmlTab)){
          if (xmlTab[j,2] > xmlTab [i,2]){
            j <- j + 1
          } else {
            escape <- 1
          }
        } else {
          escape <- 1
        }
      }
      if (exists("exTab")){
        exTab <- rbind(exTab, xmlTab [i:(j-1),])
      } else {
        exTab <- xmlTab [i:(j-1),]
      }
      i <- j - 1
    }
    i <- i + 1
  }
  if (!(exists("exTab"))){
    exTab <- NULL
  }
  exTab
}
#############################################################
xml2Delete <- function (xmlTab, xmlPath){
  # oldname : xml2Delete
  ######
  # Delete xml part from xmlField
  ######
  i <- 1
  while (i <= nrow(xmlTab)){
    if (substring(xmlTab[i ,6] ,1, nchar(xmlPath)) == xmlPath){
      j <-  i
      escape <- 0
      while (escape == 0){
        if (j <= nrow(xmlTab)){
          if (xmlTab[j,3] == xmlTab[i,3]){
            j <- j + 1
          } else {
            escape <- 1
          }
        } else {
          escape <- 1
        }
      }
      escape <- 0
      while (escape == 0){
        if (j <= nrow(xmlTab)){
          if (xmlTab[j,2] > xmlTab [i,2]){
            j <- j + 1
          } else {
            escape <- 1
          }
        } else {
          escape <- 1
        }
      }
      xmlTab <- xmlTab[-(i:(j-1)),]
      for(k in i : nrow(xmlTab)){
        xmlTab[k,3] <- as.numeric(xmlTab[k,3]) - j + i
      }
      
    } else {
      i <- i + 1
    }
  }
  xmlTab
}
#############################################################
#############################################################