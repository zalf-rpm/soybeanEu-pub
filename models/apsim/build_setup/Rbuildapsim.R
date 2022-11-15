# Build apsim simulation files from a template 
# using given soil xml files, metfiles, and other adjustments
# created 2017-11-11
# Marian Koch

# edited 2018-02-13 (error fix)


# Arguments:
# templatefile: path to template *apsim file 
# soilfiles:    paths to apsim soil xml files
# metfiles:     metfile names
# metpath:      path prefix to metfile to use for checking if the file is there
# metincludepath path prefix to metfile to use in the resulting *apsim file
# pathsep:      path separator
# insertions:   xml nodes to insert. format: list( c("Root node xpath"=filename,...) ) 
# set_nodes:    xml nodes to change. format: list( c("Root node xpath"=valuestring,...)
# target_dir:   output path
# outfilenames: output filenames
# lookup:       lookup table format: data.frame(soil=,met=,oufilename= ) [outfilename is optional]
# overwrite:    overwrite

buildapsim<-function(
  templatefile,  
  soilfiles,
  metfiles,
  metpath=".",
  metincludepath=".",
  pathsep=.Platform$file.sep,
  insertions=list(c("Soil"="initialwater.xml","bla"="sample.xml")), # list(rootnode = content)
  set_nodes=list(c("bla"="99999999")),    # list(node = value)
  target_dir="/tmp",
  outfilenames=NULL,
  lookup=NULL,
  overwrite=T,
  check_met=F
)
{
  require("XML")
  
  #check existence of metfiles
  if (check_met){
    missing<-!file.exists(file.path(target_dir,metpath,metfiles))
    if (any(missing)) stop(paste0("metfile \"", file.path(metpath,metfiles)[which(missing)[1]],"\" not found." ))
  }
  
  #read soils into list
  cat("Reading soils.")
  soillist<-unlist(lapply(soilfiles,function(s){
    s<-xmlRoot(xmlParse(s))
    sl<-getNodeSet(s,"//Soil")
    names(sl)<-unlist(lapply(sl,function(x)xmlAttrs(x)["name"]))
    return(sl)
  }))
  cat("\n",length(soillist),"Soils found")
  if (!is.null(lookup) & any(table(names(soillist))>1)) warning("Duplicate soil names. First match will be used.")

  #read template  
  template<-xmlTreeParse(templatefile,useInternal = T)

  #if no lookup table is given
  #create one by order of arguments
  if (is.null(lookup)){
    if (length(metfiles) != length(soillist)) warning("number of soils and metfiles differ")
    lookup<-data.frame(soil=names(soillist),met=unlist(metfiles),stringsAsFactors = F)
  }
  # check if all soil names present
  else if ( !all(as.character(lookup$soil) %in% names(soillist))  ){
    stop( paste("Soil not found:", as.character(lookup$soil)[which(!as.character(lookup$soil) %in% names(soillist))[1]]),call. = F)
  }
  
  #set output file names
  if (is.null(lookup$outfilename)){
    if (is.null(outfilenames)){
      lookup$outfilename<-paste0(paste(paste0("soil",lookup$soil),paste0("met",gsub("(.*)\\..*","\\1",lookup$met)),sep="_"),".apsim")
    }
    else {
      lookup$outfilename<-outfilenames
    }
  }
  lookup<-as.data.frame(lapply(lookup,as.character),stringsAsFactors = F)

  #completition of lookuptable
  lookup$insertions<-insertions
  lookup$set_nodes<-set_nodes
  
  #iterate thrpugh lookup table
  for (i in 1:nrow(lookup)){
    #tryCatch({
      apsimxml<-xmlClone(template)
      row<-lookup[i,]
      cat("\nCreating",row$outfilename)
      
      #set soil
      replaceNodes(apsimxml["//Soil"][[1]],xmlClone(soillist[[row$soil]]))
      `xmlValue<-`(apsimxml["//soilmodule"][[1]],value=row$soil)
      
      
      #insert Nodes from files
      for (j in 1:length(row$insertions[[1]])){
        ins<-row$insertions[[1]][j]
        ins.doc<-xmlRoot(xmlParse(ins))
        nodes<-getNodeSet(apsimxml,paste0("//",names(ins)))
        lapply(nodes,function(x){
          addChildren(x,ins.doc)
          })
      }
  
      #set Node values
      for (j in 1:length(row$set_nodes[[1]])){
        set.val<-row$set_nodes[[1]][j]
        nodes<-getNodeSet(apsimxml,paste0("//",names(set.val)))
        lapply(nodes,function(x)xmlValue(x)<-set.val)
      }
      
      #set metfile
      lapply(apsimxml["//metfile/filename"],function(x){
        fullmetpath<-file.path(metpath,row$met,fsep = pathsep)
        is.abspath=grepl("(^~|^/|.:)",fullmetpath)
        if (!file.exists(ifelse(is.abspath,fullmetpath,file.path(target_dir,fullmetpath)))) warning (paste0("Metfile \"",fullmetpath,"\" not there."),call.=F,immediate. = T)
        xmlValue(x)<-file.path(metincludepath,row$met)
      })
      
      #set simulation name
      lapply(apsimxml["//simulation"],function(x){
        suffix<-xmlAttrs(x)["name"]
        xmlAttrs(x)["name"]<-gsub(".apsim",suffix,row$outfilename)
      })
      
      #save
      outfilepath<-file.path(target_dir,row$outfilename)
      if (!overwrite){
        if (file.exists(outfilepath)) warning(paste0("Output file \"",outfilepath,"\" exists, skipping."))
      }
      else {
        saveXML(apsimxml,outfilepath)
      }
      rm(apsimxml)
    #}, error=function(e){cat("Error creating apsim file from (",paste(names(row),row,sep="=",collapse = " "),"). Error message: ",e$message )})
  }
}





