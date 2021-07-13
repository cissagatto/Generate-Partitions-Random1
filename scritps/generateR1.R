##################################################################################################
# Random Partitions Version 1 for Multi-label Classification with CLUS                           #
# Copyright (C) 2021                                                                             #
#                                                                                                #
# This program is free software: you can redistribute it and/or modify it under the terms of the #
# GNU General Public License as published by the Free Software Foundation, either version 3 of   #  
# the License, or (at your option) any later version. This program is distributed in the hope    #
# that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for    #
# more details.                                                                                  #     
#                                                                                                #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
#                                                                                                #
##################################################################################################

##################################################################################################
# Script 3 - Miscelaneous                                                                       #
##################################################################################################

##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
sistema = c(Sys.info())
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/Generate-Partitions-Random1", sep="")
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/Generate-Partitions-Random1", sep="")
}
FolderScripts = paste(FolderRoot, "/scripts", sep="")


##################################################################################################
# FUNCTION GENERATED RANDOM PARTITIONS                                                           #
#   Objective                                                                                    #
#       generated random partitions                                                              #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       names_labels: names of the labels                                                        #
#       number_folds: number of folds created                                                    #
#       FolderRandom: path folder                                                                #
#   Return                                                                                       #
#       configuration partitions                                                                 #
##################################################################################################
generateR1 <- function(namesLabels, number_folds, dataset_name, ds, folderResults){
  
  diretorios = directories(dataset_name, folderResults)
  
  num.particoes = ds$Labels - 1
  
  cat("\nGet the labels names")
  ordem.labels = sort(namesLabels, index.return = TRUE)
  rotulos = data.frame(ordem.labels)
  names(rotulos) = c("rotulos","indice")
  
  cat("\nFrom fold = 1 to number_folds")
  f = 1
  rp1 <- foreach(f = 1:number_folds) %dopar%{  
    
    cat("\nFold = ", f)
    if(interactive()==TRUE){ 
      cat("\nteste") 
      flush.console() 
    }

    ############################################################################################################    
    cat("\nCreate data frame to save results")
    num.fold = c(0)
    num.part = c(0)
    num.group = c(0)
    names.labels = c(0)
    num.index = c(0)
    AllPartitions = data.frame(num.fold, num.part, num.group, names.labels, num.index)
    
    ############################################################################################################
    group = c(0)
    label = c(0)
    allPartitions2 = data.frame(label, group)
    
    ############################################################################################################
    # cat("\nData frame")
    fold = c(0)
    partition = c(0)
    num.groups = c(0)
    resumePartitions = data.frame(fold, partition, num.groups)
    
    FolderSplit = paste(diretorios$folderOutputDataset, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==FALSE){
      dir.create(FolderSplit)
    } 
    
    cat("\nFrom partition = 2 to the partition = l - 2")
    
    if(interactive()==TRUE){ flush.console() }
    
    k = 2
    while(k<=num.particoes){
      
      cat("\n\tPartition = ", k)
      
      cat("\nSpecifying folder")
      FolderPartition = paste(FolderSplit, "/Partition-", k, sep="")
      if(dir.exists(FolderPartition)==FALSE){
        dir.create(FolderPartition)
      } 
      
      cat("\nGet the random partition")
      particao = sample(1:k, length(namesLabels), replace = TRUE)
      
      while(sum(1:k %in% particao) != k){
        particao <- sample(1:k, length(namesLabels), replace = TRUE)
      }
      
      cat("\nOrder partition")
      particao.sorted <- sort(particao, index.return = TRUE)
      
      cat("\nTransform in data frame")
      particao2 = data.frame(particao.sorted)
      
      cat("\nChange the columns names")
      names(particao2) = c("grupo","rotulo")
      
      cat("\nOrder alphabtic partition")
      particao3 = particao2[order(particao2$rotulo, decreasing = FALSE), ] 
      
      cat("\nOrder alphabetic labels names")
      rotulos2 = rotulos[order(rotulos$indice, decreasing = FALSE), ] 
      
      cat("\nAssing labels with index and groupos")
      fold = f
      pFinal = data.frame(cbind(fold, particao3, rotulos2))
      
      cat("\nSave specific partition")
      group = pFinal$grupo
      label = pFinal$rotulos
      pFinal2 = data.frame(group, label)
      setwd(FolderPartition)
      write.csv(pFinal2, paste("partition-", k, ".csv", sep=""), row.names = FALSE)
      
      cat("\nFrequencia")
      library("dplyr")
      frequencia1 = count(pFinal2, pFinal2$group)
      names(frequencia1) = c("group", "totalLabels")
      setwd(FolderPartition)
      write.csv(frequencia1, paste("fold-", f, "-labels-per-group-partition-", k, ".csv", sep=""), row.names = FALSE)
      
      cat("\nUpdate data frame")
      num.fold = f
      num.part = k
      num.group = pFinal$grupo
      names.labels = pFinal$rotulos
      num.index = pFinal$indice
      AllPartitions = rbind(AllPartitions, 
                            data.frame(num.fold, num.part, num.group, names.labels, num.index))
      
      cat("\nSave all partition in results dataset")
      setwd(FolderSplit)
      write.csv(AllPartitions[-1,], paste("fold-", f, "-all-partitions-v2.csv", sep=""), row.names = FALSE)
      
      cat("\ntodas partições")
      pFinal = pFinal[order(pFinal$rotulos, decreasing = FALSE),]
      nomesDosRotulos = pFinal$rotulos
      group = pFinal$grupo
      allPartitions2 = cbind(allPartitions2, group)
      b = k + 1
      names(allPartitions2)[b] = paste("partition-", k, sep="")
      
      cat("\ngruops por particao")
      fold = f
      partition = k
      num.groups = k
      resumePartitions = rbind(resumePartitions, data.frame(fold, partition, num.groups))
      
      k = k + 1
      
      if(interactive()==TRUE){ flush.console() }
      gc()
      
    } # fim da partição
    
    allPartitions2 = allPartitions2[,-2]
    allPartitions2$label = nomesDosRotulos
    setwd(FolderSplit)
    write.csv(allPartitions2, paste("fold-", f, "-all-partitions.csv", sep=""), row.names = FALSE)
    
    resumePartitions = resumePartitions[-1,]
    setwd(FolderSplit)
    write.csv(resumePartitions, paste("fold-", f, "-groups-per-partition.csv", sep=""), row.names = FALSE)
    
    cat("\nIncrement split: ", f)
    
    if(interactive()==TRUE){ flush.console() }
    gc()
    
  } # fim do fold
  
  cat("\n\n################################################################################################")
  cat("\n# CLUS RANDOM 1: END FUNCTION generated Random Partitions                                        #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
  gc()
}

