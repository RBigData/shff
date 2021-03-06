#' read_h5df
#' 
#' TODO
#' 
#' @param h5in
#' Input file.
#' @param dataset
#' Dataset in input file to read or \code{NULL}. In the latter case, TODO
#' @param indices
#' TODO
#' 
#' @export
read_shff = function(h5in, dataset=NULL, indices=NULL)
{
  check.is.string(h5in)
  if (!is.null(dataset))
    check.is.string(dataset)
  if (!is.null(indices))
  {
    if (length(indices) == 0 || !all(is.inty(indices)) || any(indices < 1))
      stop("argument 'indices' must be a vector of positive integers")
  }
  
  
  fp = h5file(h5in, mode="r")
  dataset = h5_get_dataset(fp, dataset)
  
  x = read_shff_mm(fp, dataset, indices)
  
  h5close(fp)
  
  x
}



extract = function(fp, dataset, indices)
{
  if (is.null(indices))
    fp[[dataset]][]
  else
    fp[[dataset]][indices]
}



read_shff_mm = function(fp, dataset, indices)
{
  attrs = glue(dataset, ATTR_PATH)
  format = h5attr(fp[[attrs]], "format")
  M = h5attr(fp[[attrs]], "nrows")
  N = h5attr(fp[[attrs]], "ncols")
  nz = h5attr(fp[[attrs]], "nz")
  matcode = mm_matcode_int2char(h5attr(fp[[attrs]], "matcode"))
  indexing = h5attr(fp[[attrs]], "indexing")
  
  I = extract(fp, glue(dataset, "I"), indices)
  J = extract(fp, glue(dataset, "J"), indices)
  
  if (mm_is_pattern(matcode))
    val = NULL
  else
    val = extract(fp, glue(dataset, "val"), indices)
  
  mm = list(M=M, N=N, nz=nz, I=I, J=J, val=val, matcode=matcode, indexing=indexing)
}
