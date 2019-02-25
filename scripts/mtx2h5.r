library(Matrix)
library(shff)

rmifthere = function(file) if (file.exists(file)) file.remove(file)

fin = "../inst/exampledata/kg2.mtx"
fout = sub("[.]mtx", ".h5", fin)
rmifthere(fout)
kg2 = Matrix::readMM(fin)
shff::write_shff(kg2, fout)

fin = "../inst/exampledata/kg4.mtx"
fout = sub("[.]mtx", ".h5", fin)
rmifthere(fout)
kg4 = Matrix::readMM(fin)
shff::write_shff(kg4, fout)
