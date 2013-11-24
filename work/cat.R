
rfiles <- list.files("../categorical/R", pattern="*\\.R$", full.names = TRUE)

for (file in rfiles) {
  source(file)
}

