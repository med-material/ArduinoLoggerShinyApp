VerboseMessage = function(verbose, msg, symbol="", tab=""){
  if (verbose) {
    message(paste0(tab, symbol, " ", msg, " ", symbol))
  }
}

HandleVerboseArgument = function(HRVData, verbose) {
  if (!is.null(verbose)) {
    warning(DeprecatedArgMessage("verbose", "SetVerbose()"))
    HRVData = SetVerbose(HRVData, verbose)
  }
  HRVData  
}
