#########################################################################################
# This script contains methods to download files from the internet that are called on   #
# from the file_downloader.R script. The first method here is used to download files    #
# from open access sources as indexed on Unpaywall. The second method is a selenium ba- #
# method that uses a python script to download files from publishers that are not open  #
# access. For now the selenium method will only work if you have a NordVPN subscription,# 
# as I had one and it can easily be used to rotate IP addresses, something that is abso-#
# lutely necessary when downloading from publishers, as they will block your IP address #
# after a few downloads.                                                                #
#                                                                                       #                          
#                   !!!!!!!!!!!!!!!!TAKE NOTE!!!!!!!!!!!!!!!!                           #        
# IF YOU ARE NOT CERTAIN THE VPN ROTATION WORKS, DO NOT USE THIS METHOD AS YOU (OR YOUR #
# ENTIRE DEPARTMENT!) MIGHT BE BLOCKED FROM THE PUBLISHERS WEBSITES                     #
#                                                                                       #
# Despite this dramatic warning, the selenium method is                                 #
# very useful and can be used to download files from publishers who provide PDFS open   #
# access, but do not allow direct download using headless browser requests.             #
# This method is illegal as far as I know, it is just a little bit of a workaround.     #
# Then again, who knows what publishers consider legal or not.                          #
# If you're familiar with Python, you can also replace or remove the NordVPN rotation   #
# part of the script.                                                                   #
# You can also add your own methods in this file, just make sure to add them to the     #
# download_methods list at the end of the script.                                       #
#                                                                                       #
# If you decide to add your own method, just make sure that it returns a vector with    #
# the first element being either "Success" or "Failed" and the second element being the #
# method used to download the file (e.g. "oadoi" or "selenium") and the third element   #
# being the URL from which the file was downloaded, e.g.:                               #
#                                                                                       #
# return(c("Success", "mymethod", url))                                                 #
# the second and third argument are optional, you can also just return (c("Success"))   #
#                                                                                       #
#########################################################################################


download_methods_loaded <- TRUE

# make workspace overview to create list of loaded methods
before_methods <- ls()


#########################################################################################
############################# DEFINE YOUR METHODS UNDER HERE ############################
#########################################################################################



oa_dl <- function(doi, journal_path, download_dir, oa_email_key, doi_filename) {
  
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
          suppressWarnings(download.file(url = best_url, destfile = here::here(download_dir, journal_path, paste0(doi_filename, ".pdf")), mode = "wb"))
          # check if file is larger than 40 kb
          if (file.size(here::here(download_dir, journal_path, paste0(doi_filename, ".pdf"))) < 20000) {
            stop("File is too small, falling back on other urls")
          } else {
            message("Download successful from ", best_url)
            return(c("Success", "oadoi", best_url))
          }
        },
        #  Handle potential error
        error = function(e) {
          message("Error: ", e)
          message("Trying other urls")
          # check if other locations are available
          if(length(oadoi_fetch$oa_locations) == 0){
            message("no open access URLS found...")
            return(c("OA_Failed", best_url))
          }
          if(length(oadoi_fetch$oa_locations[[1]]$url) == 1){
            message("no open access URLS found...")
            return(c("OA_Failed", best_url))
          }
          # if this is reached, other urls are available and will be tried 1 by 1
          for (i in 1:length(oadoi_fetch$oa_locations[[1]])) {
            print(paste0("Trying url: ", oadoi_fetch$oa_locations[[1]]$url[i]))
            tryCatch(
              { 
                # download file from other urls if previous ones failed
                suppressWarnings(download.file(url = oadoi_fetch$oa_locations[[1]]$url[i], destfile = here::here(download_dir, journal_path, paste0(doi_filename, ".pdf")), mode = "wb"))
                message("Download successful from ", oadoi_fetch$oa_locations[[1]]$url[i])
                # sometimes a file is downloaded cannot possibly be large enough to actually contain something, so check that
                if (file.size(here::here(download_dir, journal_path, paste0(doi_filename, ".pdf"))) < 20000) {
                  stop("File is too small, falling back on other urls")
                } else {
                  message("Download successful from ", oadoi_fetch$oa_locations[[1]]$url[i])
                  # return success, to let the file_downloader.R script know that the file was downloaded
                  return(c("Success", "oadoi", oadoi_fetch$oa_locations[[1]]$url[i]))
                }
              },
              # Handle potential error
              error = function(e) {
                message("Error: ", e)
                message("Trying next url")
              }
            )
          }
        }
      )
  } else {
    # if this is reached no open access urls were found. Return failed to let the file_downloader.R script know that the file was not downloaded and other methods should be tried
    message("no open access URLS found...")
    return(c("Failed"))
  }

}

# TODO: make extra DL folder for selenium and then move files into correct folder using R. This way there will be no problems if journal switches and less complicated to achieve in python etc.
selenium_api_dl <- function(doi, res, download_dir, journal_path){
  
  # write current download dir to file. 
  # NOTE: This is sorta a bad solution and will be changed in the future, as now any change would not be recognized by the python script anyway while it is still running, unless the server is killed for each different journal.
  # Will have to be fixed in the future, but for now it works, if you just don't use different journal paths for one session
  if(!exists("sel_download_dir_set")){
    sel_download_dir_set <- TRUE
    assign("sel_download_dir_set", sel_download_dir_set, envir = .GlobalEnv)
    sel_download_dir <- here::here(download_dir, journal_path)
    # write to txt file
    write(sel_download_dir, file = here::here("python/sel_download_dir.txt"))
  }
  # if server is not started yet, do so
  if(!exists("selenium_running")){
    selenium_running <- TRUE
    assign("selenium_running", selenium_running, envir = .GlobalEnv)
    p <- process$new("python3", c(here::here("python/selenium_server.py")), 
                 stdout = "selenium.log", stderr = "selenium.log", supervise = TRUE, cleanup = FALSE)
    assign("p", p, envir = .GlobalEnv)
    print("starting selenium server")
    # wait for server to start
    Sys.sleep(30)
  }
  # handle potential reasons why we open access might have failed
  if (res[1] == "Failed"){
    url <- paste0("https://annas-archive.org/scidb/", doi)
  } else if (res[1] == "OA_Failed"){
    url <- paste0("https://dx.doi.org/", doi)
  } else {
    url <- paste0("https://annas-archive.org/scidb/", doi)
  }

  # download file using selenium
  doi_filename <- gsub("[^[:alnum:]]", "", doi)
  response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url), "&doi_filename=", doi_filename))

  # see if download was successful or failed with expected error
  if(content(response)$status == "Download successful"){
    message("Download successful")
    return(c("Success", "selenium", url))
  } else if (content(response)$status == "Failed: Invalid PDF file"){
    message("Download failed - PDF file corrupted")
    return(c("Failed"))
  } else if (content(response)$status == "Failed: Explicit Skip Requested") {
    message("Download failed - Explicit Skip Requested")
    return(c("Failed"))
  } else if (content(response)$status == "Failed: DOI Not Resolved"){
    message("Download failed - DOI Not Resolved")
    return(c("Failed"))
  }

  # retry (for now only once), because usually there should be no reason for the download to fail unless there's a captcha request, which hopefully will be resolved on reroll, otherwise this just costs lots of time
  rts <- 0
  while(content(response)$status != "Download successful" && rts < 1){
    message("Download failed with selenium, rotating IP and retrying")
    response <- GET(paste0("http://127.0.0.1:8000/rotate"))
    response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url), "&doi_filename=", doi_filename))
    rts <- rts + 1
  }

  # handle remaining potential errors or Success
  if(rts == 1){
    message("Selenium Download failed")
    return(c("Failed"))
  } else {
    message("Download successful")
    return(c("Success", "selenium", url))
  }
}

#######################################################################################################
###### THE FOLLOWING PART IS FOR DEFINING WHICH METHODS ARE USED IN THE FILE_DOWNLOADER.R SCRIPT ######
#######################################################################################################

# make workspace overview to create list of loaded methods
after_methods <- ls()

# now all defined functions can be loaded in order of their definition automatically (meaning they will be used in file_downloader.R)

# create list of loaded methods
loaded_methods <- setdiff(after_methods, before_methods)
# Filter only functions
download_methods <- loaded_methods[sapply(loaded_methods, function(x) is.function(get(x)))]


# or they can be loaded manually (which I am doing here, so the above automatic part will be overwritten)
download_methods <- c("oa_dl", "selenium_api_dl")




# These are some tests for the selenium method to see whether it (still) works for different publishers
# You usually don't need to run these, they are more here for me (or you) to see if the selenium method still works

# url_aa_avail <- "https://annas-archive.org/scidb/10.1111/j.1467-9280.2005.01597.x"  # annas archive, available
# url_aa_unavail <- "https://annas-archive.org/scidb/10.1177/09567976241263347"       # annas archive, unavailable
# url_apa <- "https://dx.doi.org/10.1037/xge0001697"                                  # apa
# url_doi_noresolve <- "https://dx.doi.org/10.1037/asdfasdf"                          # doi not resolved
# url_elsevier <- "https://dx.doi.org/10.1016/j.cogpsych.2025.101716"                 # elsevier
# url_mdpi <- "https://dx.doi.org/10.3390/agriculture15030291"                        # mdpi  
# url_nature <- "https://dx.doi.org/10.1038/ncomms15958"                              # nature
# url_oxford <- "https://dx.doi.org/10.1093/jimmun/vkae005"                           # oxford
# url_pnas <- "https://dx.doi.org/10.1073/pnas.2306025121"                            # pnas
# url_sage <- "https://dx.doi.org/10.1177/09567976241263347"                          # sage
# url_science <- "https://dx.doi.org/10.1126/scitranslmed.adm7580"                    # science
# url_springer <- "https://dx.doi.org/10.3758/s13428-024-02343-1"                     # spinger
# url_taylor <- "https://dx.doi.org/10.1080/23311908.2024.2432740"                    # taylor-francis
# url_wiley <- "https://dx.doi.org/10.1002/psp4.13295"                                # wiley

# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_aa_avail), "&doi_filename=test_aa"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_aa_unavail), "&doi_filename=test_aa"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_apa), "&doi_filename=test_apa"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_doi_noresolve), "&doi_filename=test_apa"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_elsevier), "&doi_filename=test_elsevier"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_mdpi), "&doi_filename=test_mdpi"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_nature), "&doi_filename=test_nature"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_oxford), "&doi_filename=test_oxford"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_pnas), "&doi_filename=test_pnas"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_sage), "&doi_filename=test_sage"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_science), "&doi_filename=test_science"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_springer), "&doi_filename=test_springer"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_taylor), "&doi_filename=test_taylor"))
# response <- GET(paste0("http://127.0.0.1:8000/download?url=", URLencode(url_wiley), "&doi_filename=test_wiley"))
