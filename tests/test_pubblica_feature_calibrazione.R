source(file.path("r","pubblica_feature_calibrazione.R"),local=TRUE)
stopifnot(identical(estrai_c_caratteri(parse(text='c("a","b")')[[1]]),c("a","b")))
cat("test_pubblica_feature_calibrazione: OK\n")
