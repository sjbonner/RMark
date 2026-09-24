print_RMark.version <- function()
{ library(help=RMark)$info[[1]] -> version
	version <- version[pmatch("Version",version)]
	if(!is.null(version))
	{
		um <- strsplit(version," ")[[1]]
  	    version <- um[nchar(um)>0][2]
	}
	hello <- paste("This is RMark ",version,"\n"," Documentation for RMark is available at https://github.com/jlaake/RMark/blob/master/RMarkDocumentation.zip\n",
	               " Documentation for MARK and capture-recapture models is available at https://ecooch.github.io/markbook.github.io/",sep="")
	packageStartupMessage(hello)
}

.onAttach <- function(...) { 
	print_RMark.version()
	checkForMark()
}

create_markpath=function()
{
  # Default executable names in preference order
  if(R.version$arch %in% c("x86_64","aarch64","arm64"))
    execs = c("mark","mark64","mark32")
  else
    execs = \c("mark","mark32")
  
  # Add executable suffix
  suff = ifelse(R.version$os == "mingw32", ".exe", "")
  execs = paste(execs, suff, sep =)

  # Initialize path
  markpath = NULL
  
  if(exists(MarkPath)){
    # Check for mark executables in specified path
    paths = execs |> 
      sapply(\(x)file.path(MarkPath,x)) 
    
    paths = paths[file.exists(paths)]
    
    if(lengths(paths) > 0)
      markpath = paths[1] 
    else
      message("No mark executable found in specified MarkPath: ", MarkPath,".")
  }
  
  if(is.null(markpath)){
    # Check for mark excutables in user's path
    message("Checking system directories.")
  
    paths = execs |> 
      sapply(\(exec) Sys.which(exec))
    
    if(any(paths != ""))
      markpath <- paths[1]
  else
    message("No mark executable found in user's path.")
  }
  
  if(is.null(markpath)){
    # Check default directories
    if(R.version$os == "mingw32"){
      # Define default locations
      MarkPath=c("c:/Program Files/Mark","c:/Program Files (x86)/Mark")
      
      paths <- MarkPath |> 
        sapply(\(path) execs |> 
                 sapply(\(x)file.path(path,x))) |> 
        as.vector()
      
      paths <- paths[file.exists(paths)]
      
      if(length(paths) > 0)
        markpath <- paths[1]
      else
        message("No executables found in default locations.")
    }
  }
  
  if(is.null(markpath))
    stop("No executable found.")
  else{
    message("Found MARK executable: ", markpath,"\n")
    
    markpath |> 
      shQuote() |> 
      return()
  }
}

checkMarkVersion <- function(markpath=markpath)
{
  x=system2(markpath,args="-v",stdout=TRUE,stderr=TRUE)
  if(length(grep(" No input file",x[1]))>0)
    packageStartupMessage("Please update MARK to current version posted 25 January 2026 to obtain MARK version number\n")
  else
  {
    suppressWarnings(x<-as.numeric(strsplit(x[1]," ")[[1]]))
    if(x[!is.na(x)][1]<11.2)
      packageStartupMessage("Warning:Reported MARK version is less than required")
  }
  return(NULL)
}


checkForMark<-function()
{
	if(R.Version()$os=="mingw32")
	{
	   markpath=create_markpath()
	   if(is.null(markpath) || length(markpath)==0)
	   {
	     packageStartupMessage("Warning: Software mark.exe,mark32.exe or mark64.exe not found in path or in c:/Program Files/mark or c:/Program Files (x86)/mark\n. It is available at http://www.phidot.org/software/mark/\n")
	     packageStartupMessage('         If you have mark.exe, you will need to set MarkPath object to its location (e.g. MarkPath="C:/Users/Jeff Laake/Desktop"')
	   }
	   else
	   {
	     if(markpath!="mark.exe")
	        checkMarkVersion(markpath=substring(markpath,2,nchar(markpath)-1))
	     else
	       checkMarkVersion(markpath=markpath)
	   }
   }else
	   if(exists("MarkPath")) 
     {
	      isep="/"
	      if(substr(MarkPath,nchar(MarkPath),nchar(MarkPath))%in%c("\\","/")) isep=""
  		  MarkPath=paste(MarkPath,"mark",sep=isep)
	      if(!file.exists(MarkPath)) 
	        packageStartupMessage(paste("mark executable cannot be found at specified MarkPath location:",MarkPath,"\n"))	
  		  else
  		     checkMarkVersion(markpath=MarkPath)
      } else
        {
 	       if(Sys.which("mark")=="")
	       {
 	         packageStartupMessage("Warning: Software mark not found in path.\n")
 	         packageStartupMessage("If you have mark executable, you will need to set MarkPath object to its location.")
 	       } else
 	       {
 	         checkMarkVersion(markpath="mark")
 	       }
	    }
	invisible()
}


