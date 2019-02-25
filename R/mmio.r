# A fairly direct native R port of the matrix market library. See https://math.nist.gov/MatrixMarket/mmio-c.html
# 
# License for the original implementation is as follows:
# 
# This software was developed by employees of the National Institute of
# Standards and Technology (NIST), an agency of the Federal Government and is
# being made available as a public service. Pursuant to title 17 United States
# Code Section 105, works of NIST employees are not subject to copyright
# protection in the United States.  This software may be subject to foreign
# copyright.  Permission in the United States and in foreign countries, to the
# extent that NIST may hold copyright, to use, copy, modify, create derivative
# works, and distribute this software and its documentation without fee is
# hereby granted on a non-exclusive basis, provided that this notice and
# disclaimer of warranty appears in all copies. 
# 
# THE SOFTWARE IS PROVIDED 'AS IS' WITHOUT ANY WARRANTY OF ANY KIND, EITHER
# EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY
# THAT THE SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, AND FREEDOM FROM
# INFRINGEMENT, AND ANY WARRANTY THAT THE DOCUMENTATION WILL CONFORM TO THE
# SOFTWARE, OR ANY WARRANTY THAT THE SOFTWARE WILL BE ERROR FREE.  IN NO EVENT
# SHALL NIST BE LIABLE FOR ANY DAMAGES, INCLUDING, BUT NOT LIMITED TO, DIRECT,
# INDIRECT, SPECIAL OR CONSEQUENTIAL DAMAGES, ARISING OUT OF, RESULTING FROM, OR
# IN ANY WAY CONNECTED WITH THIS SOFTWARE, WHETHER OR NOT BASED UPON WARRANTY,
# CONTRACT, TORT, OR OTHERWISE, WHETHER OR NOT INJURY WAS SUSTAINED BY PERSONS
# OR PROPERTY OR OTHERWISE, AND WHETHER OR NOT LOSS WAS SUSTAINED FROM, OR AROSE
# OUT OF THE RESULTS OF, OR USE OF, THE SOFTWARE OR SERVICES PROVIDED HEREUNDER.



fprintf = function(stream, fmt, ...)
{
  s = sprintf(fmt, ...)
  cat(s, file=stream, append=TRUE)

  nchar(s)
}



# ------------------------------------------------------------------------------
# mmio.h
# ------------------------------------------------------------------------------

MM_MAX_LINE_LENGTH = 1025L
MatrixMarketBanner = "%%MatrixMarket"
MM_MAX_TOKEN_LENGTH = 64L

# MM_typecode query fucntions
mm_is_matrix = function(typecode) ((typecode)[1] == 'M')

mm_is_sparse = function(typecode)     ((typecode)[2] == 'C')
mm_is_coordinate = function(typecode) ((typecode)[2] == 'C')
mm_is_dense = function(typecode)      ((typecode)[2] == 'A')
mm_is_array = function(typecode)      ((typecode)[2] == 'A')

mm_is_complex = function(typecode)  ((typecode)[3] == 'C')
mm_is_real = function(typecode)     ((typecode)[3] == 'R')
mm_is_pattern = function(typecode)  ((typecode)[3] == 'P')
mm_is_integer = function(typecode)  ((typecode)[3] == 'I')

mm_is_symmetric = function(typecode)  ((typecode)[4] == 'S')
mm_is_general = function(typecode)    ((typecode)[4] == 'G')
mm_is_skew = function(typecode)       ((typecode)[4] == 'K')
mm_is_hermitian = function(typecode)  ((typecode)[4] == 'H')

# MM_typecode modify fucntions
mm_set_matrix = function(typecode)      {typecode[1] = 'M'; typecode}
mm_set_coordinate = function(typecode)  {typecode[2] = 'C'; typecode}
mm_set_array = function(typecode)       {typecode[2] = 'A'; typecode}
mm_set_dense = function(typecode)       mm_set_array(typecode)
mm_set_sparse = function(typecode)      mm_set_coordinate(typecode)

mm_set_complex = function(typecode) {typecode[3] = 'C'; typecode}
mm_set_real = function(typecode)    {typecode[3] = 'R'; typecode}
mm_set_pattern = function(typecode) {typecode[3] = 'P'; typecode}
mm_set_integer = function(typecode) {typecode[3] = 'I'; typecode}

mm_set_symmetric = function(typecode) {typecode[4] = 'S'; typecode}
mm_set_general = function(typecode)   {typecode[4] = 'G'; typecode}
mm_set_skew = function(typecode)      {typecode[4] = 'K'; typecode}
mm_set_hermitian = function(typecode) {typecode[4] = 'H'; typecode}

# Matrix Market error codes
MM_COULD_NOT_READ_FILE = 11L
MM_PREMATURE_EOF = 12L
MM_NOT_MTX = 13L
MM_NO_HEADER = 14L
MM_UNSUPPORTED_TYPE = 15L
MM_LINE_TOO_LONG = 16L
MM_COULD_NOT_WRITE_FILE = 17L

# Matrix Market internal definitions
MM_MTX_STR = "matrix"
MM_ARRAY_STR = "array"
MM_DENSE_STR = "array"
MM_COORDINATE_STR = "coordinate"
MM_SPARSE_STR = "coordinate"
MM_COMPLEX_STR = "complex"
MM_REAL_STR = "real"
MM_INT_STR = "integer"
MM_GENERAL_STR = "general"
MM_SYMM_STR = "symmetric"
MM_HERM_STR = "hermitian"
MM_SKEW_STR = "skew-symmetric"
MM_PATTERN_STR = "pattern"



# ------------------------------------------------------------------------------
# mmio.c
# -----------------------------------------------------------------------------

mm_is_valid = function(matcode)
{
  if (!mm_is_matrix(matcode))
    return(FALSE)
  if (mm_is_dense(matcode) && mm_is_pattern(matcode))
    return(FALSE)
  if (mm_is_real(matcode) && mm_is_hermitian(matcode))
    return(FALSE)
  if (mm_is_pattern(matcode) && (mm_is_hermitian(matcode) || mm_is_skew(matcode)))
    return(FALSE)
  
  TRUE
}



mm_read_banner = function(f)
{
  matcode = character(4)
  
  line = scan(f, what=character(), n=5, quiet=TRUE)
  if (length(line) != 5)
    stop("premature EOF")
  
  banner = line[1]
  mtx = tolower(line[2])
  crd = tolower(line[3])
  data_type = tolower(line[4])
  storage = tolower(line[5])
  
  # check for banner
  if (banner != MatrixMarketBanner)
    stop("no matrix market header")
  
  # first field should be "mtx"
  if (mtx != MM_MTX_STR)
    stop("unsupported type")
  
  matcode = mm_set_matrix(matcode)
  
  # second field describes whether this is a sparse matrix (in coordinate storgae) or a dense array
  if (crd == MM_SPARSE_STR)
    matcode = mm_set_sparse(matcode)
  else if (crd == MM_DENSE_STR)
    matcode = mm_set_dense(matcode)
  else
    stop("unsupported type")
  
  # third field
  if (data_type == MM_REAL_STR)
    matcode = mm_set_real(matcode)
  else if (data_type == MM_COMPLEX_STR)
    matcode = mm_set_complex(matcode)
  else if (data_type == MM_PATTERN_STR)
    matcode = mm_set_pattern(matcode)
  else if (data_type == MM_INT_STR)
    matcode = mm_set_integer(matcode)
  else
    stop("unsupported type")
  
  # fourth field
  if (storage == MM_GENERAL_STR)
    matcode = mm_set_general(matcode)
  else if (storage == MM_SYMM_STR)
    matcode = mm_set_symmetric(matcode)
  else if (storage == MM_HERM_STR)
    matcode = mm_set_hermitian(matcode)
  else if (storage == MM_SKEW_STR)
    matcode = mm_set_skew(matcode)
  else
    stop("unsupported type")
  
  
  matcode
}



mm_write_mtx_crd_size = function(f, M, N, nz)
{
  fprintf(f, "%d %d %d\n", M, N, nz)
}



mm_read_mtx_crd_size = function(f)
{
  line = scan(f, what=integer(), n=3, quiet=TRUE, comment.char="%")
  if (length(line) != 3)
    stop("premature EOF")
  
  list(M=line[1], N=line[2], nz=line[3])
}



mm_read_mtx_array_size = function(f)
{
  mm_read_mtx_crd_size(f)[-3]
}



mm_read_mtx_crd_data = function(f, matcode)
{
  size = mm_read_mtx_crd_size(f)
  
  if (mm_is_complex(matcode))
  {
    # TODO
    # fscanf(f, "%d %d %lg %lg", &I[i], &J[i], &val[2*i], &val[2*i+1])
    stop("complex reader not yet implemented")
  }
  else if (mm_is_real(matcode))
  {
    df = read.table(f, comment.char="%", header=FALSE)[-1, ]
    val = df[, 3]
  }
  else if (mm_is_pattern(matcode))
  {
    df = read.table(f, comment.char="%", header=FALSE)[-1, ]
    val = NULL
  }
  else
    stop("unsupported type")
  
  
  list(M=size$M, N=size$N, nz=size$nz, I=df[, 1], J=df[, 2], val=val, matcode=matcode)
}



mm_read_mtx_crd_entry = function(f, matcode)
{
  size = mm_read_mtx_crd_size(f)
  
  if (mm_is_complex(matcode))
  {
    # TODO
    # fscanf(f, "%d %d %lg %lg", I, J, real, imag)
    stop("complex reader not yet implemented")
  }
  else if (mm_is_real(matcode))
  {
    df = read.table(f, comment.char="%", header=FALSE)[-1, ]
    real = df[, 3]
    imag = NULL
  }
  else if (mm_is_pattern(matcode))
  {
    df = read.table(f, comment.char="%", header=FALSE)[-1, ]
    real = NULL
    imag = NULL
  }
  else
    stop("unsupported type")
  
  
  list(M=size$M, N=size$N, nz=size$nz, I=df[, 1], J=df[, 2], real=real, imag=imag, matcode=matcode)
}



mm_read_mtx_crd = function(f)
{
  matcode = mm_read_banner(f)
  
  if (!(mm_is_valid(matcode) && mm_is_sparse(matcode) && mm_is_matrix(matcode)))
    stop("unsupported type")
  
  mm_read_mtx_crd_data(f, matcode)
}



mm_write_banner = function(f, matcode)
{
  s = mm_typecode_to_str(matcode)
  fprintf(f, "%s %s\n", MatrixMarketBanner, s);
}



mm_write_mtx_crd = function(f, M, N, nz, I, J, val, matcode)
{
  # print banner followed by typecode
  fprintf(f, "%s ", MatrixMarketBanner)
  fprintf(f, "%s", mm_typecode_to_str(matcode))

  # print matrix sizes and nonzeros
  fprintf(f, "\n%d %d %d\n", M, N, nz)

  # print values
  if (mm_is_pattern(matcode))
    df = data.frame(I, J)
  else if (mm_is_real(matcode))
    df = data.frame(I, J, val)
  else if (mm_is_complex(matcode))
  {
    even = seq(2L, length(val), by=2L)
    odd = seq(1L, length(val), by=2L)
    df = data.frame(I, J, val[even], val[odd])
  }
  else
    stop("unsupported type")
  
  write.table(df, file=f, sep=" ", append=TRUE, row.names=FALSE, col.names=FALSE)
}



mm_typecode_to_str = function(matcode)
{
  types = character(4)
  
  # check for MTX type
  if (mm_is_matrix(matcode)) 
    types[1] = MM_MTX_STR
  else
    stop("unsupported type")
  
  # check for CRD or ARR matrix
  if (mm_is_sparse(matcode))
    types[2] = MM_SPARSE_STR
  else if (mm_is_dense(matcode))
    types[2] = MM_DENSE_STR
  else
    stop("unsupported type")
  
  # check for element data type
  if (mm_is_real(matcode))
    types[3] = MM_REAL_STR
  else if (mm_is_complex(matcode))
    types[3] = MM_COMPLEX_STR
  else if (mm_is_pattern(matcode))
    types[3] = MM_PATTERN_STR
  else if (mm_is_integer(matcode))
    types[3] = MM_INT_STR
  else
    stop("unsupported type")
  
  
  # check for symmetry type
  if (mm_is_general(matcode))
    types[4] = MM_GENERAL_STR
  else if (mm_is_symmetric(matcode))
    types[4] = MM_SYMM_STR
  else if (mm_is_hermitian(matcode))
    types[4] = MM_HERM_STR
  else if (mm_is_skew(matcode))
    types[4] = MM_SKEW_STR
  else
    stop("unsupported type")
  
  
  types
}
