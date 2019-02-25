#' write_shff
#' 
#' @details 
#' TODO
#' 
#' @param x
#' Input dataset (dataframe or datatable)
#' @param file
#' Output file.
#' @param dataset
#' Dataset in input file to read or \code{NULL}. In the latter case (e.g. \code{NULL}), the dataset will be contained 
#' within a group named as the input dataset.
#' @param compression
#' HDF5 compression level. An integer, 0 (least compression) to 9 (most compression).  Default is
#' \code{compression} = 4. 
#' 
#' @export
write_shff = function(x, file, dataset=NULL, compression=4)
{
  check.is.string(file)
  if (!is.null(dataset))
    check.is.string(dataset)
  else
    dataset = deparse(substitute(x))
  check.is.natnum(compression)
  if (compression > 9)
    stop("argument 'compression' must be an integer from 0 to 9 (inclusive)")
  
  if (is.Matrix(x))
    mm = mat2mm(x)
  else
    stop("object 'x' is an unsupported type")
  
  fp = h5file(file, mode="a")
  h5_check_dataset(fp, dataset)
  
  write_shff_aij(mm, fp, dataset)
  
  h5close(fp)
  invisible()
}



write_shff_aij = function(mm, fp, dataset)
{
  createGroup(fp, dataset)
  h5attr(fp[[dataset]], "SHFF_VERSION") = SHFF_VERSION
  
  attrs = glue(dataset, ATTR_PATH)
  createGroup(fp, attrs)
  h5attr(fp[[attrs]], "format") = SHFF_FORMAT_AIJ
  h5attr(fp[[attrs]], "indexing") = mm$indexing
  h5attr(fp[[attrs]], "nrows") = mm$M
  h5attr(fp[[attrs]], "ncols") = mm$N
  h5attr(fp[[attrs]], "nz") = mm$nz
  h5attr(fp[[attrs]], "matcode") = mm_matcode_char2int(mm$matcode)
  
  fp[[glue(dataset, "I")]] = mm$I
  fp[[glue(dataset, "J")]] = mm$J
  if (!mm_is_pattern(mm$matcode))
  fp[[glue(dataset, "val")]] = mm$val
}
