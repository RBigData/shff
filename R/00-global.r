SHFF_VERSION = "0.1-0"

ATTR_PATH = "attrs"

SHFF_FORMAT_AIJ = 1L
SHFF_FORMAT_CSR = 2L
SHFF_FORMAT_CSC = 3L

SHFF_FORMAT_AIJ_STR = "AIJ"
SHFF_FORMAT_CSR_STR = "CSR"
SHFF_FORMAT_CSC_STR = "CSC"

shff_format_int2str = function(fmt)
{
  if (fmt == SHFF_FORMAT_AIJ)
    SHFF_FORMAT_AIJ_STR
  else if (fmt == SHFF_FORMAT_CSR)
    SHFF_FORMAT_CSR_STR
  else if (fmt == SHFF_FORMAT_CSC)
    SHFF_FORMAT_CSC_STR
}

shff_format_str2int = function(fmt)
{
  if (fmt == SHFF_FORMAT_AIJ_STR)
    SHFF_FORMAT_AIJ
  else if (fmt == SHFF_FORMAT_CSR_STR)
    SHFF_FORMAT_CSR
  else if (fmt == SHFF_FORMAT_CSC_STR)
    SHFF_FORMAT_CSC
}
