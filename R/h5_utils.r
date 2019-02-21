close_and_stop = function(h5_fp, msg)
{
  h5close(h5_fp)
  stop(msg, call.=FALSE)
}



h5_check_dataset = function(h5_fp, dataset)
{
  if (existsGroup(dataset))
    close_and_stop(h5_fp, "dataset already exists in h5 file")
  
  invisible()
}



h5_get_dataset = function(h5_fp, dataset)
{
  if (is.null(dataset))
  {
    datasets = names(h5_fp)
    if (length(datasets) != 1)
    {
      h5close(h5_fp)
      stop("multiple datasets available")
    }
    else
      dataset = datasets[1]
  }
  
  dataset
}
