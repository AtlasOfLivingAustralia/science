# Download all Austral Ecology papers to pdfs

out_dir <- '../../austral_pdfs/'
article_path <- '~/Downloads/scopus.csv'

articles <- read.csv(article_path, stringsAsFactors = FALSE)
dois <- articles$DOI

lapply(dois, function(x) {
  download_url <- paste0(
    "https://onlinelibrary.wiley.com/doi/pdfdirect/",
    x,
    "?download=true")
  # Replace forward slash so doi can be used in file name
  doi_str <- str_replace_all(x, "/", "-")
  download.file(download_url, destfile = paste0(out_dir, doi_str, ".pdf"))
})