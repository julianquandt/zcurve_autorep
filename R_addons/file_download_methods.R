download_methods_loaded <- TRUE

# make workspace overview to create list of loaded methods
before_methods <- ls()


oa_dl <- function(doi, journal_path, download_dir, oa_email_key, doi_filename) {
  print(doi)
  # get open access version from roadoi
  oadoi_fetch <- oadoi_fetch(
    dois = doi,
    email = oa_email_key
  )
  # check if open access version exists
  if(length(oadoi_fetch$best_oa_location[[1]]) == 0){
    message("No open access version found for ", doi)
    best_url = ""
  } else {
    best_url <- oadoi_fetch$best_oa_location[[1]]$url
    print(best_url)
  }

  # try download file from best url and fall back to other urls if not possible
  if(nchar(best_url) > 0){
    tryCatch(
        {
          # download file from best url
          suppressWarnings(download.file(url = best_url, destfile = paste0(download_dir, journal_path, "/", doi_filename, ".pdf"), mode = "wb"))
          # check if file is larger than 40 kb
          if (file.size(paste0(download_dir, journal_path, "/", doi_filename, ".pdf")) < 20000) {
            stop("File is too small, falling back on other urls")
          } else {
            message("Download successful from ", best_url)
            return(c("Success", "oadoi", best_url))
          }
        },
        # Handle the error
        error = function(e) {
          message("Error: ", e)
          message("Trying other urls")
          if(length(oadoi_fetch$oa_locations) == 0){
            message("no open access URLS found...")
            return(c("OA_Failed", best_url))
          }
          print(oadoi_fetch$oa_locations[[1]]$url)
          if(length(oadoi_fetch$oa_locations[[1]]$url) == 1){
            message("no open access URLS found...")
            return(c("OA_Failed", best_url))
          }
          for (i in 1:length(oadoi_fetch$oa_locations[[1]])) {
            print(paste0("Trying url: ", oadoi_fetch$oa_locations[[1]]$url[i]))
            Sys.sleep(1)
            tryCatch(
              { 
                # download file from other urls if previous ones failed
                suppressWarnings(download.file(url = oadoi_fetch$oa_locations[[1]]$url[i], destfile = paste0("./downloads/", journal_path, "/", doi_filename, ".pdf"), mode = "wb"))
                message("Download successful from ", oadoi_fetch$oa_locations[[1]]$url[i])
                if (file.size(paste0("./downloads/", journal_path, "/", doi_filename, ".pdf")) < 20000) {
                  stop("File is too small, falling back on other urls")
                } else {
                  message("Download successful from ", oadoi_fetch$oa_locations[[1]]$url[i])
                  return(c("Success", "oadoi", oadoi_fetch$oa_locations[[1]]$url[i]))
                }
              },
              error = function(e) {
                message("Error: ", e)
                message("Trying next url")
              }
            )
          }
        }
      )
  } else {
    message("no open access URLS found...")
    return(c("Failed"))
  }

}

# this method downloads pdfs from sci-hub. The mirrors might need to be checked and adjusted every now and then.
Sys.setenv(SCI_HUB_URL = "https://sci-hub.ru/")
Sys.setenv(IN_SCIHUB_LOOP = 1)
scihub_dl <- function(doi = row$doi, use_proxy = TRUE, proxy_address = "socks5://localhost:9050", journal_path = journal_path, parent_level = TRUE, source_overview = TRUE) {
  
  sci_hub_urls <- c("https://sci-hub.ru/", "https://sci-hub.st/")
  #sci-hub url from system var
  sci_hub_url <- Sys.getenv("SCI_HUB_URL")

  # The URL of the page containing the download link
  page_url <- paste0(sci_hub_url, doi)
  # The regular expression pattern to extract the download link
  pattern <- "location\\.href='(/?/?[^']+)'"

  # Send the HTTP request to the URL, if a proxy is used, set the proxy address
  if (use_proxy == TRUE) {
    response <- GET(
      url = page_url,
      add_headers(
        `User-Agent` = "Mozilla/5.0 (iPhone; CPU iPhone OS 11_3_1 like Mac OS X) AppleWebKit/603.1.30 (KHTML, like Gecko) Version/10.0 Mobile/14E304 Safari/602.1"
      ), use_proxy(proxy_address)
    )
  } else {
    response <- GET(
      url = page_url,
      add_headers(
        `User-Agent` = "Mozilla/5.0 (iPhone; CPU iPhone OS 11_3_1 like Mac OS X) AppleWebKit/603.1.30 (KHTML, like Gecko) Version/10.0 Mobile/14E304 Safari/602.1"
      )
    )
  }


  # Check for a successful response
  if (http_status(response)$category == "Success") {
    print("SUCCEED IN GETTING RESPONSE")
    # Parse the response body as XML
    document <- content(response, "text", encoding = "UTF-8")
    # Check if the article was found, and stop if not
    if (grepl("<title>Sci-Hub: article not found</title>", document)) {
      print("Article not found")
      stop("Article not found")
    }

    # for file naming format doi with only valid file name characters
    doi_filename <- gsub("[^[:alnum:]]", "", doi)


    extracted_url <- str_match(document, pattern)[, 2] # [,2] to get the contents of the first capturing group
    if (is.na(extracted_url)) {
      stop("No download link found on sci-hub")
    } else if (!grepl("^//", extracted_url)) {
      # if the extracted url starts with only one / then add another one
      pdf_url <- paste0(sci_hub_url, extracted_url)
    } else {
      if("sci-hub.ru/" %in% extracted_url){
        extracted_url <- gsub("sci-hub.ru/", substr(sci_hub_url, 9, nchar(sci_hub_url)) , extracted_url)
      }
      pdf_url <- paste0("https:", extracted_url)
    }
    print(paste0("SH URL PDF: ", pdf_url))

    # set proxy for download
    Sys.setenv(HTTPS_PROXY = proxy_address)
    Sys.setenv(http_proxy = proxy_address)
    # check if proxy is set in case it is requested
    if (Sys.getenv("HTTPS_PROXY") != proxy_address || Sys.getenv("http_proxy") != proxy_address) {
      print("stopping because proxy not set")
      stop("Proxy not set")
    } else {
      print("PROXY SET: ATTEMPTING DOWNLOAD")

      # check if file already exists and is larger than 40 kb (which hints at a valid pdf file)
      if (!file.exists(paste0("./downloads/", journal_path, "/", doi_filename, ".pdf")) || (file.size(paste0("./downloads/", journal_path, "/", doi_filename, ".pdf")) < 20000)) {
        # download file
        tryCatch(
          {
            print("STARTING SH DOWNLOAD")

            download.file(url = pdf_url, destfile = paste0("./downloads/", journal_path, "/", doi_filename, ".pdf"), mode = "wb")
            # unset proxy
            Sys.setenv(http_proxy = "")
            Sys.setenv(HTTPS_PROXY = "")
            if (!file.exists(paste0("./downloads/", journal_path, "/", doi_filename, ".pdf")) || (file.size(paste0("./downloads/", journal_path, "/", doi_filename, ".pdf")) < 20000)) {
              # retry
              message("Download failed, because file is too small; retrying")
              sci_hub_url <- sci_hub_urls[which(sci_hub_urls == Sys.getenv("SCI_HUB_URL"))%%length(sci_hub_urls) + 1]
              Sys.setenv("SCI_HUB_URL" = sci_hub_url)
              scihub_dl(doi, use_proxy = use_proxy, proxy_address = proxy_address, journal_path = journal_path, parent_level = FALSE)
             }
            message("Download successful from ", pdf_url)
            if(source_overview == TRUE){
              write.table(data.frame(doi = doi, method = "scihub"),paste0("./downloads/", journal_path, "/", "source_overview", ".csv"), append = TRUE, row.names = FALSE, col.names = FALSE)
            }
            Sys.setenv("DL_FAILED" = "FALSE")
          },
          # Handle the error
          error = function(e) {
            Sys.setenv(http_proxy = "")
            Sys.setenv(HTTPS_PROXY = "")
            stop("Error downloading file: ", e$message)
            # You can add any other code you want to run in case of an error here
          },
          warning = function(w) {
             if(grepl("downloaded length 0 != reported length", w$message)) {
                # Handle the specific warning
                # You can return a specific value or message here
                Sys.setenv("DL_FAILED" = "TRUE")
            } else {
                # If it's not the specific warning, return the warning as is
                message("Warning: ", w$message)
            }

            # You can add any other code you want to run in case of a warning here
          }
        )
      } else {
        Sys.setenv(http_proxy = "")
        Sys.setenv(HTTPS_PROXY = "")
        message("File already exists")
        return()
      }
    }
  } else {
    # Handle the error
    message("Request failed with status ", status_code(response))
    if(status_code(response) == 403){
      Sys.setenv("DL_FAILED" = "TRUE")
      message("sci-hub might have ddos protection enabled. We will try to fall back to another sci-hub url")
      sci_hub_url_at_loopstart <- sci_hub_url
      Sys.setenv(SCI_HUB_URL_START = sci_hub_url_at_loopstart)
      print(Sys.getenv("SCI_HUB_URL") != Sys.getenv("SCI_HUB_URL_START"))
      print(as.numeric(Sys.getenv("IN_SCIHUB_LOOP"))  > 3)
      if(!(Sys.getenv("SCI_HUB_URL") != Sys.getenv("SCI_HUB_URL_START") & as.numeric(Sys.getenv("IN_SCIHUB_LOOP"))  > 3 & Sys.getenv("DL_FAILED") == "TRUE")){
        print("loop started")
        Sys.setenv(IN_SCIHUB_LOOP = (as.numeric(Sys.getenv("IN_SCIHUB_LOOP")) + 1))
        sci_hub_url <- sci_hub_urls[which(sci_hub_urls == Sys.getenv("SCI_HUB_URL"))%%length(sci_hub_urls) + 1]
        Sys.setenv(SCI_HUB_URL = sci_hub_url)
        message("Trying next sci-hub url: ", sci_hub_url)
        Sys.sleep(3)
        scihub_dl(doi, use_proxy = use_proxy, proxy_address = proxy_address, journal_path = journal_path, parent_level = FALSE)
      } else {
        Sys.setenv(IN_SCIHUB_LOOP = 1)
      }
    }
  }
}


# TODO: make extra DL folder for selenium and then move files into correct folder using R. This way there will be no problems if journal switches and less complicated to achieve in python etc.

#TODO: doi does not seem to get resolved correctly with file name appended?

selenium_api_dl <- function(doi, res, download_dir, journal_path){
  # set download_dir to env if not set yet
  if(!exists("sel_download_dir_set")){
    sel_download_dir_set <<- TRUE
    sel_download_dir <- normalizePath(paste0(download_dir, journal_path))
    # write to txt file
    write(sel_download_dir, file = "./python/sel_download_dir.txt")
  }
  if(!exists("selenium_running")){
    selenium_running <<- TRUE
    p <- process$new("python3", c("./python/selenium_server.py"), 
                 stdout = "selenium.log", stderr = "selenium.log", supervise = TRUE, cleanup = FALSE)
    print("starting selenium server")
    Sys.sleep(30)
  }
  print("doing selenium again")
  if (res[1] == "Failed"){
    url <- paste0("https://annas-archive.org/scidb/", doi)
  } else if (res[1] == "OA_Failed"){
    url <- paste0("https://dx.doi.org/", doi)
  } else {
    url <- paste0("https://annas-archive.org/scidb/", doi)
  }
  doi_filename <- gsub("[^[:alnum:]]", "", doi)
  response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url), "&doi_filename=", doi_filename))
  if(content(response)$status == "Download successful"){
    message("Download successful")
    return(c("Success", "selenium", url))
  } else if (content(response)$status == "Failed: Invalid PDF file"){
    message("Download failed - PDF file corrupted")
    return(c("Failed"))
  }
  rts <- 0
  while(content(response)$status != "Download successful" && rts < 2){
    message("Download failed with selenium, rotating IP and retrying")
    response <- GET(paste0("http://127.0.0.1:8000/rotate"))
    response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url), "&doi_filename=", doi_filename))
    rts <- rts + 1
  }
  if(rts == 2){
    message("Selenium Download failed")
    return(c("Failed"))
  } else {
    message("Download successful")
    return(c("Success", "selenium", url))
  }
}


# make workspace overview to create list of loaded methods
after_methods <- ls()

# create list of loaded methods
loaded_methods <- setdiff(after_methods, before_methods)
# Filter only functions
download_methods <- loaded_methods[sapply(loaded_methods, function(x) is.function(get(x)))]
method_args <- lapply(download_methods, function(x) formals(get(x)))
# or define them manually
download_methods <- c("oa_dl", "selenium_api_dl", "scihub_dl")
download_methods <- c("oa_dl", "selenium_api_dl")

### tests for different publishers

# url_apa <- "https://dx.doi.org/10.1037/xge0001697"
# url_mdpi <- "https://dx.doi.org/10.3390/agriculture15030291"
# url_elsevier <- "https://dx.doi.org/10.1016/j.cogpsych.2025.101716"
# url_nature <- "https://dx.doi.org/10.1038/ncomms15958" # nature
# url_oxford <- "https://dx.doi.org/10.1093/jimmun/vkae005"
# url_pnas <- "https://dx.doi.org/10.1073/pnas.2306025121"
# url_sage <- "https://dx.doi.org/10.1177/09567976241263347"
# url_science <- "https://dx.doi.org/10.1126/scitranslmed.adm7580"
# url_springer <- "https://dx.doi.org/10.3758/s13428-024-02343-1" # spinger
# url_taylor <- "https://dx.doi.org/10.1080/23311908.2024.2432740"
# url_wiley <- "https://dx.doi.org/10.1002/psp4.13295"

# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_apa), "&doi_filename=test_apa"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_mdpi), "&doi_filename=test_mdpi"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_elsevier), "&doi_filename=test_elsevier"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_nature), "&doi_filename=test_nature"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_oxford), "&doi_filename=test_oxford"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_pnas), "&doi_filename=test_pnas"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_sage), "&doi_filename=test_sage"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_science), "&doi_filename=test_science"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_springer), "&doi_filename=test_springer"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_taylor), "&doi_filename=test_taylor"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_wiley), "&doi_filename=test_wiley"))



response <- GET(paste0("http://127.0.0.1:8000/rotate"))
response <- GET(paste0("http://127.0.0.1:8000/check_captcha"))
content(response)
