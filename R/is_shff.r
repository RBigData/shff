# is_shff
#' 
#' Check if an HDF5 dataset is shff-valid.
#' 
#' @param h5in
#' Input file.
#' @param dataset
#' Dataset in input file to read or \code{NULL}. In the latter case (e.g. \code{NULL}), the dataset will be contained
#' within a group named as the input dataset
#' 
#' @return
#' Returns \code{TRUE} on success, \code{FALSE} otherwise.
#' 
#' @export
is_shff = function(h5in, dataset=NULL)
{
  check.is.string(h5in)
  if (!is.null(dataset))
    check.is.string(dataset)
  
  f = h5file(h5in, mode="r")
  dataset = h5_get_dataset(f, dataset)
  fmt = h5_detect_format(f, dataset, verbose=FALSE)
  h5close(f)
  
  fmt != "unknown"
}
