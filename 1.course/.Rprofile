Sys.setenv(LANG = "en")

dir.open <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows"){
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

clear.console <- function() {
  cat("\014")
}
clear.console()

print("My setup loaded")
