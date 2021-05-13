# Download all Austral Ecology papers as pdfs
# CSV of all articles comes from Scopus

out_dir <- '../../austral_pdfs/'
article_path <- '~/Downloads/scopus.csv'

articles <- read.csv(article_path, stringsAsFactors = FALSE)
dois <- articles$DOI

# Replace dodgy DOI
# See failed vector for others that need to be manually replaced/looked up
dois <- replace(dois, dois == "10.1111/j.1440-169X.2001.t01-1-.x",
                      "10.1046/j.1442-9993.2001.01152.x")
dois <- replace(dois, dois == "10.1111/j.1442-9993.2000.tb00037.x",
                "10.1046/j.1442-9993.2000.01043.x")
dois <- replace(dois, dois == "10.1111/j.1442-9993.2000.tb00046.x",
                "10.1046/j.1442-9993.2000.01052.x")
dois <- replace(dois, dois == "10.1111/j.1442-9993.2000.tb00051.x",
                "10.1046/j.1442-9993.2000.01085.x")
dois <- replace(dois, dois == "10.1046/j.1442-9993.2000.01056.x",
                "10.1111/j.1442-9993.2000.tb00062.x")

# Have done DOIs up to 1811


failed <- lapply(dois[1768:length(dois)], function(x) {
  download_url <- paste0(
    "https://onlinelibrary.wiley.com/doi/pdfdirect/",
    x,
    "?download=true")
  # Replace forward slash so doi can be used in file name
  doi_str <- str_replace_all(x, "/", "-")
  
  tryCatch(
    download.file(download_url, destfile = paste0(out_dir, doi_str, ".pdf")),
    # Some DOIs have what looks like an old prefix- change this to 10.1111 and add
    # ".pp" and it works
    error = function(e) {
      new_doi <- str_replace_all(x, c("1046"="1111", ".x"=".pp.x"))
      download_url <- paste0(
        "https://onlinelibrary.wiley.com/doi/pdfdirect/",
        new_doi,
        "?download=true")
      tryCatch(
        download.file(download_url, destfile = paste0(out_dir, doi_str, ".pdf")),
        warning = function(w) {
          return(x)
        },
        error = function(e) {
          return(x)
        })
    })
})

to_check <- unlist(failed)[unlist(failed) != "0"]
