root <- pkgload:::shim_system.file("tests/testthat/", package = "dynwrap")
for (file in list.files(root, full.names = TRUE, pattern = "helper-ti_")) {
  source(file)
}
