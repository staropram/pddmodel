
check_deps <- function() {
   # only run this script once per R session, uses "check_deps_ran" flag
   if(exists("check_deps_ran")) {
      return()
   }

   installed_packages <- row.names(installed.packages())

   # if renv is already installed and all packages are there we can exit
   if("renv" %in% installed_packages) {
      library(renv)
      required_packages <- dependencies()$Package
         
      missing <- sapply(required_packages,function(p) {
         !p %in% installed_packages
      })
      if(!any(missing)) {
         check_deps_ran <- T
         return()
      }
   }

   permission <- readline("Please grant permission to install package dependencies (y/n): ")

   if(permission=="y") {
      # install required packages
      if(!"renv" %in% installed_packages) {
         print(paste0("installing renv for dependency lister"))
         install.packages("renv")
      }
      library(renv)
      required_packages <- dependencies()$Package
         
      lapply(required_packages,function(p) {
         if(!p %in% installed_packages) {
            print(paste0("Installing ",p))
            install.packages(p)
         }
      })

      check_deps_ran <- T
   }
}

check_deps()
