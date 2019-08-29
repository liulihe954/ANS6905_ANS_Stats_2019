install.packages("easypackasges",quite = T);library(easypackages)
Check_pkg = function(mypkg){
  miss = logical()
  for (i in seq_along(mypkg)){
    test = is.element(mypkg[i],installed.packages()[,1])
    if (!test){miss[i] = T} else {miss[i] = F}
  }
  miss_pkg = mypkg[miss]
  if (!length(miss_pkg) == 0){
    message("Now checking package requirements...\nOops! You are missing ",length(miss_pkg)," package(s), and they are: ",miss_pkg)
    for (j in seq_along(miss_pkg)){
      message("Now installing missing package number",j, " -", miss_pkg[j])
      install.packages(miss_pkg[j],quiet = T)
      library()
    }
  } else {message("Packages requirements checked! You are all set!")}
}
