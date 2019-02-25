is.Matrix = function(x)
{
  inherits(x, "dgTMatrix") || inherits(x, "ngTMatrix")
}



# "triplet" form (aij)
mat2mm_dgTMatrix = function(x)
{
  I = x@i
  J = x@j
  val = x@x
  if (is.integer(val))
    valtype = "I"
  else if (is.double(val))
    valtype = "R"
  else
    stop("data should be integer or real")
  
  matcode = c("M", "C", valtype, "G")
  
  list(I=I, J=J, val=val, matcode=matcode)
}

mat2mm_ngTMatrix = function(x)
{
  I = x@i
  J = x@j
  val = NULL
  matcode = c("M", "C", "P", "G")
  
  list(I=I, J=J, val=val, matcode=matcode)
}



mat2mm = function(x)
{
  M = dim(x)[1]
  N = dim(x)[2]
  nz = Matrix::nnzero(x)
  
  if (inherits(x, "dgTMatrix"))
    mm = mat2mm_dgTMatrix(x)
  else if (inherits(x, "ngTMatrix"))
    mm = mat2mm_ngTMatrix(x)
  
  Imin = min(mm$I)
  Jmin = min(mm$I)
  indexing = as.integer(min(1L, Imin, Jmin))
  
  list(M=M, N=N, nz=nz, I=mm$I, J=mm$J, val=mm$val, matcode=mm$matcode, indexing=indexing)
}



mm2mat = function(mm)
{
  symmetric = mm_is_symmetric(mm$matcode)
  
  indx = mm$indexing
  if (indx == 0L)
  {
    I = mm$I + 1L
    J = mm$J + 1L
  }
  
  if (mm_is_pattern(mm$matcode))
    Matrix::sparseMatrix(i=I, j=J, symmetric=symmetric)
  else
    Matrix::sparseMatrix(i=I, j=J, x=mm$val, symmetric=symmetric)
}
