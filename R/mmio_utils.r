MM_MTX_INT =        1L

MM_SPARSE_INT =     11L
MM_DENSE_INT =      12L

MM_COMPLEX_INT =    101L
MM_REAL_INT =       102L
MM_PATTERN_INT =    103L
MM_INT_INT =        104L

MM_SYMMETRIC_INT =  1001L
MM_GENERAL_INT =    1002L
MM_SKEW_INT =       1003L
MM_HERMITIAN_INT =  1004L


MM_MTX_CHAR =        'M'

MM_SPARSE_CHAR =     'C'
MM_DENSE_CHAR =      'A'

MM_COMPLEX_CHAR =    'C'
MM_REAL_CHAR =       'R'
MM_PATTERN_CHAR =    'P'
MM_INT_CHAR =        'I'

MM_SYMMETRIC_CHAR =  'S'
MM_GENERAL_CHAR =    'G'
MM_SKEW_CHAR =       'K'
MM_HERMITIAN_CHAR =  'H'


mm_matcode_char2int = function(matcode)
{
  intcode = integer(4)
  
  if (mm_is_matrix(matcode)) 
    intcode[1] = MM_MTX_INT
  else
    stop("unsupported type")
  
  if (mm_is_sparse(matcode))
    intcode[2] = MM_SPARSE_INT
  else if (mm_is_dense(matcode))
    intcode[2] = MM_DENSE_INT
  else
    stop("unsupported type")
  
  if (mm_is_real(matcode))
    intcode[3] = MM_REAL_INT
  else if (mm_is_complex(matcode))
    intcode[3] = MM_COMPLEX_INT
  else if (mm_is_pattern(matcode))
    intcode[3] = MM_PATTERN_INT
  else if (mm_is_integer(matcode))
    intcode[3] = MM_INT_INT
  else
    stop("unsupported type")
  
  if (mm_is_general(matcode))
    intcode[4] = MM_GENERAL_INT
  else if (mm_is_symmetric(matcode))
    intcode[4] = MM_SYMM_INT
  else if (mm_is_hermitian(matcode))
    intcode[4] = MM_HERM_INT
  else if (mm_is_skew(matcode))
    intcode[4] = MM_SKEW_INT
  else
    stop("unsupported type")
  
  intcode
}



mm_matcode_int2char = function(intcode)
{
  matcode = character(4)
  
  if (intcode[1] == MM_MTX_INT) 
    matcode[1] = MM_MTX_CHAR
  else
    stop("unsupported type")
  
  if (intcode[2] == MM_SPARSE_INT)
    matcode[2] = MM_SPARSE_CHAR
  else if (intcode[2] == MM_DENSE_INT)
    matcode[2] = MM_DENSE_CHAR
  else
    stop("unsupported type")
  
  if (intcode[3] == MM_REAL_INT)
    matcode[3] = MM_REAL_CHAR
  else if (intcode[3] == MM_COMPLEX_INT)
    matcode[3] = MM_COMPLEX_CHAR
  else if (intcode[3] == MM_PATTERN_INT)
    matcode[3] = MM_PATTERN_CHAR
  else if (intcode[3] == MM_INT_INT)
    matcode[3] = MM_INT_CHAR
  else
    stop("unsupported type")
  
  if (intcode[4] == MM_GENERAL_INT)
    matcode[4] = MM_GENERAL_CHAR
  else if (intcode[4] == MM_SYMM_INT)
    matcode[4] = MM_SYMM_CHAR
  else if (intcode[4] == MM_HERM_INT)
    matcode[4] = MM_HERM_CHAR
  else if (intcode[4] == MM_SKEW_INT)
    matcode[4] = MM_SKEW_CHAR
  else
    stop("unsupported type")
  
  matcode
}



write_mm = function(f, mm)
{
  mm_write_mtx_crd(f, mm$M, mm$N, mm$nz, mm$I, mm$J, mm$val, mm$matcode)
}
