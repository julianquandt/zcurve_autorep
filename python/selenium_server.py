import asyncio
import time
import os
from pathlib import Path
from fastapi import FastAPI, HTTPException, Query
from selenium_driverless import webdriver
from selenium_driverless.types.options import Options
from nordvpn_switcher import initialize_VPN, rotate_VPN
import uvicorn
import aiohttp
import aiofiles
from bs4 import BeautifulSoup
from fake_useragent import UserAgent
from pathlib import Path
from pypdf import PdfReader
from pypdf.errors import PdfReadError
import re

def extract_doi(url: str) -> str:
    """
    Extracts a DOI from a given URL using a regular expression.
    Example DOI: 10.1177/09567976241263347
    """
    doi_regex = r'(10\.\d{4,9}/[-._;()/:A-Z0-9]+)'
    match = re.search(doi_regex, url, re.IGNORECASE)
    if match:
        return match.group(1)
    return ""


async def is_captcha_triggered():
    """
    Check if a CAPTCHA is actively present and visible on the page.

    This function attempts to locate common CAPTCHA elements (e.g. reCAPTCHA widgets or Cloudflare CAPTCHA)
    and then verifies that they are visible by examining their bounding rectangle dimensions.

    Returns:
        bool: True if a CAPTCHA is visibly active, False otherwise.
    """
    # Method 1: Look for a visible reCAPTCHA element.
    try:
        captcha_element = await browser.find_element("css", ".g-recaptcha")
        if captcha_element:
            rect = await browser.execute_script(
                "return arguments[0].getBoundingClientRect();", captcha_element
            )
            if rect and rect.get("width", 0) > 0 and rect.get("height", 0) > 0:
                return True
    except Exception:
        pass

    # Method 2: Look for a visible reCAPTCHA iframe.
    try:
        recaptcha_iframe = await browser.find_element(
            "xpath", "//iframe[contains(@src, 'recaptcha/api2/anchor')]"
        )
        if recaptcha_iframe:
            rect = await browser.execute_script(
                "return arguments[0].getBoundingClientRect();", recaptcha_iframe
            )
            if rect and rect.get("width", 0) > 0 and rect.get("height", 0) > 0:
                return True
    except Exception:
        pass

    # Method 3: Check for a Cloudflare CAPTCHA indicator.
    try:
        cloudflare_input = await browser.find_element("css", "input[name='cf_captcha_kind']")
        if cloudflare_input:
            # Instead of relying on the input itself (which might be hidden), check its parent.
            parent = await browser.execute_script(
                "return arguments[0].parentElement;", cloudflare_input
            )
            if parent:
                rect = await browser.execute_script(
                    "return arguments[0].getBoundingClientRect();", parent
                )
                if rect and rect.get("width", 0) > 0 and rect.get("height", 0) > 0:
                    return True
    except Exception:
        pass

    return False


async def force_pdf_download(doi_filename: str, url: str):
    """
    Attempt to fetch the PDF URL from the page and download it using requests.
    Applies publisher-specific URL rewriting if the original URL was a DOI resolver.
    
    If the browser's current URL contains "dx.doi.org", it extracts the DOI by taking the
    substring between "dx.doi.org/" and the first "&". Then, if the found PDF URL is from
    one of the supported publishers, it rewrites the URL as follows:
    
      - onlinelibrary.wiley.com becomes:
          https://[JOURNAL]onlinelibrary.wiley.com/doi/pdfdirect/[DOI]
      - journals.sagepub.com becomes:
          https://journals.sagepub.com/doi/pdf/[DOI]
      - link.springer.com becomes:
          https://link.springer.com/content/pdf/[DOI]
      - science.org becomes:
            https://www.science.org/doi/pdf/[DOI]
      - tandfonline.com becomes:
            https://www.tandfonline.com/doi/pdf/[DOI]
      - pnas.org becomes:
            https://www.pnas.org/doi/pdf/[DOI]
    
    If downloading via requests fails, the function falls back to navigating to the PDF URL.
    
    Args:
        doi_filename (str): The filename to save the downloaded PDF as.
    Returns:
        bool: True if the PDF was successfully downloaded or navigated to, False otherwise.
    """
    global current_page_url

    if "https://annas-archive.org/search" in current_page_url:
        print("not found on annas archive")
        return "SKIP_REST"

    try:
        # Find an anchor element that might be a download button.
        xpath_expr = (
            "//a["
            "contains(@href, 'pdf') or "
            "contains(text(), 'Download') or "
            "contains(text(), 'PDF') or "
            "contains(text(), 'Download PDF') or "
            "contains(text(), 'VIEW PDF') or "
            "contains(@title, 'PDF') or "
            "contains(@aria-label, 'PDF')"
            "]"
        )
        download_button = await browser.find_element("xpath", xpath_expr)

        if download_button:
            print(f"download_button: {download_button}")
            pdf_url = await download_button.get_attribute("href")
            if pdf_url:
                print(f"Found PDF link: {pdf_url}")

                # If the current URL is a DOI resolver, extract the DOI.
                if "dx.doi.org" in url:
                    start_index = url.find("dx.doi.org/") + len("dx.doi.org/")
                    end_index = url.find("&", start_index)
                    if end_index == -1:
                        doi = url[start_index:]
                    else:
                        doi = url[start_index:end_index]
                    print(f"Extracted DOI from resolver: {doi}")

                    # Apply publisher-specific rules based on the found PDF link.
                    if "onlinelibrary.wiley.com" in current_page_url:
                        # Split at '/doi/' and rebuild the URL.
                        parts = current_page_url.split("/doi/")
                        if len(parts) > 1:
                            base = parts[0] + "/doi/"
                            pdf_url = base + "pdfdirect/" + doi
                        else:
                            pdf_url = "https://onlinelibrary.wiley.com/doi/pdfdirect/" + doi
                    elif "journals.sagepub.com" in current_page_url:
                        pdf_url = "https://journals.sagepub.com/doi/pdf/" + doi
                    elif "link.springer.com" in current_page_url:
                        pdf_url = "https://link.springer.com/content/pdf/" + doi
                    elif "science.org" in current_page_url:
                        pdf_url = "https://www.science.org/doi/pdf/" + doi
                    elif "tandfonline.com" in current_page_url:
                        pdf_url = "https://www.tandfonline.com/doi/pdf/" + doi
                    elif "pnas.org" in current_page_url:
                        pdf_url = "https://www.pnas.org/doi/pdf/" + doi
                    elif "dx.doi.org" in current_page_url:
                        # If the current URL is still a DOI resolver, this means doi cannot be resolved.
                        print("DOI cannot be resolved.")
                        return "DOI_NOT_RESOLVED"
                    print(f"Rewritten PDF URL: {current_page_url}")
                elif "annas-archive.org/scidb" in url:
                    print("annas archive is here")
                    try:
                        return await download_pdf_via_requests(pdf_url, doi_filename)
                    except Exception as req_e:
                        print(f"Download via requests failed: {req_e}. Attempting to navigate to the PDF URL.")

                try:
                    await browser.get(pdf_url)
                    return True
                except Exception as nav_e:
                    print(f"Fallback navigation failed: {nav_e}")
        else:
            print("no buttons???")
    except Exception as e:
        print(f"Error finding PDF link: {e}")
    return False



async def download_pdf_via_requests(pdf_url, doi_filename):
    """
    Download a PDF file from a given URL and save it with a specified filename.

    This function uses aiohttp to perform an asynchronous HTTP GET request to download
    the PDF file from the provided URL. The downloaded file is saved in the specified
    directory with the given DOI filename.

    Args:
        pdf_url (str): The URL of the PDF file to be downloaded.
        doi_filename (str): The DOI filename to save the downloaded PDF as.

    Returns:
        str: The file path of the downloaded PDF if successful, otherwise None.

    Raises:
        Exception: If there is an error during the download process.
    """
    """Download the PDF file using aiohttp instead of Selenium click."""
    filename = os.path.join(DOWNLOAD_DIR, f"{doi_filename}.pdf")

    try:
        async with aiohttp.ClientSession() as session:
            async with session.get(pdf_url) as response:
                if response.status == 200:
                    async with aiofiles.open(filename, "wb") as f:
                        await f.write(await response.read())
                    print(f"PDF downloaded successfully: {filename}")
                    return filename
                else:
                    print(f"Failed to download PDF: HTTP {response.status}")
    except Exception as e:
        print(f"Error downloading PDF: {e}")

    return None

# FastAPI App
app = FastAPI()

# VPN Setup (if required)
vpn_instr = initialize_VPN(area_input=['random countries europe 10'], skip_settings=1)
last_vpn_rotation = time.time()

# Selenium Options
# Load Download Dir from text file that will be created by R
script_dir = Path(__file__).parent  # Gets the directory of the script
file_path = script_dir / "sel_download_dir.txt"
with file_path.open("r") as f:
    DOWNLOAD_DIR = f.read().strip()


# DOWNLOAD_DIR = "/home/julian/projects/auto_rep_dgps/downloads"
options = Options()
options.add_argument("--headless")
prefs = {
    "download.default_directory": DOWNLOAD_DIR,
    "plugins.always_open_pdf_externally": True
}
options.add_experimental_option("prefs", prefs)

browser = None

async def init_browser():
    """Initializes or reinitializes the browser after VPN rotation."""
    global browser
    global last_vpn_rotation

    if browser:
        try:
            await browser.quit()  # Close current browser session safely
        except Exception:
            pass  # Ignore if it's already closed

    # Rotate VPN
    rotate_VPN(vpn_instr, google_check=0)
    last_vpn_rotation = time.time()

    # Start a new browser session
    try:
        ua = UserAgent()
        options.add_argument(f"--user-agent={ua.random}")  # Needs double dashes
        browser = await webdriver.Chrome(options=options)
    except Exception as e:
        print(f"Error initializing browser: {e}")
        await restart_browser()  # Retry in case of failure




@app.on_event("startup")
async def startup_event():
    """Initialize Selenium when the server starts."""
    await init_browser()

async def wait_for_download(before_files, timeout=20):
    """
    Wait for a new file to appear in the download directory.

    Args:
        before_files (set): A set of filenames present in the download directory before the download starts.
        timeout (int, optional): The maximum time to wait for a new file to appear, in seconds. Defaults to 10.

    Returns:
        str or None: The name of the new file if it appears within the timeout period, otherwise None.
    """
    """Wait for a new file to appear in the download directory."""
    start_time = time.time()
    while time.time() - start_time < timeout:
        after_files = set(os.listdir(DOWNLOAD_DIR))
        # only filter pdf files in after_files
        after_files = set([file for file in after_files if file.endswith(".pdf") or file.endswith(".PDF")])
        new_files = after_files - before_files
        if new_files:
            print("new file found")
            return list(new_files)[0]  # Return the new file name immediately
        await asyncio.sleep(0.5)  # Check more frequently, reducing unnecessary delay
    return None  # Timeout


async def wait_for_page_load(timeout=10, stabilization_duration=1.0, check_interval=0.5):
    """
    Wait until the page is fully loaded and its DOM becomes stable.

    This function first waits for document.readyState to be "complete".
    Then it monitors the length of document.body.innerHTML and waits until
    it remains unchanged for stabilization_duration seconds, which can help
    ensure that inner objects (such as text or download links) have loaded.

    Args:
        timeout (int, optional): Maximum time to wait in seconds. Defaults to 10.
        stabilization_duration (float, optional): Seconds the DOM must remain unchanged. Defaults to 1.0.
        check_interval (float, optional): Interval in seconds between checks. Defaults to 0.5.

    Returns:
        bool: True if the page loads and stabilizes within timeout, False otherwise.
    """
    start_time = time.time()
    stable_start = None
    previous_length = None

    while time.time() - start_time < timeout:
        state = await browser.execute_script("return document.readyState")
        if state == "complete":
            current_length = len(await browser.execute_script("return document.body.innerHTML"))
            if previous_length is not None and current_length == previous_length:
                # If the DOM length hasn't changed, note the time
                if stable_start is None:
                    stable_start = time.time()
                elif time.time() - stable_start >= stabilization_duration:
                    print("Page fully loaded and stable")
                    return True
            else:
                stable_start = None
            previous_length = current_length
        await asyncio.sleep(check_interval)
    return False


current_page_url = None

@app.get("/download")
async def download_pdf(url: str, doi_filename: str = Query(..., description="Filename based on DOI")):
    """
    Download a PDF from the given URL and save it with a filename based on the DOI.

    Args:
        url (str): The URL from which to download the PDF.
        doi_filename (str): The filename to save the downloaded PDF, based on the DOI.

    Returns:
        dict: A dictionary containing the status of the download and the filename if successful.
        HTTPException: An HTTP exception with a status code and detail message if an error occurs.

    Raises:
        HTTPException: If the browser is not initialized, page load times out, CAPTCHA is triggered,
                       download times out, or any other exception occurs during the process.

    Notes:
        - The function waits for the page to load dynamically instead of using a fixed timeout.
        - If a CAPTCHA is triggered, the VPN is rotated and the function returns a status message.
        - The function attempts to download the PDF via a button click method, and if not found,
          relies on auto-download settings.
        - The function waits for a new file event instead of using sleep.
        - The downloaded file is renamed to match the DOI filename.
        - The VPN is rotated only if 10 minutes have passed since the last rotation.
    """
    """Download PDF and wait until it finishes before responding."""
    global last_vpn_rotation, current_page_url
    if not browser:
        return HTTPException(status_code=500, detail="Browser not initialized")

    try:
        before_files = set(os.listdir(DOWNLOAD_DIR))  # Track files before opening URL

        await browser.get(url)

        # **Wait for the page to load dynamically instead of a fixed timeout**
        if not await wait_for_page_load():
            return HTTPException(status_code=500, detail="Page load timeout")
        else:
            current_page_url = await browser.current_url
            print(f"Page loaded: {current_page_url}")

        # **Check if CAPTCHA is triggered and rotate VPN if needed**
        print("captcah")
        if await is_captcha_triggered():
            rotate_VPN(vpn_instr, google_check=1)
            last_vpn_rotation = time.time()
            return {"status": "VPN rotated due to CAPTCHA"}

        # **Try downloading via the button click method**
        print("force pdf download")
        force_dl = await force_pdf_download(doi_filename, url)
        if not force_dl:
            print("No explicit download button found, relying on auto-download settings.")
        elif force_dl == "SKIP_REST":
            return {"status": "Failed: Explicit Skip Requested", "file": f"{doi_filename}.pdf", "url": url}
        elif force_dl == "DOI_NOT_RESOLVED":
            return {"status": "Failed: DOI Not Resolved", "file": f"{doi_filename}.pdf", "url": url}

        # **Wait for new file event instead of using sleep**
        print("wait for download")
        downloaded_file = await wait_for_download(before_files)
        if not downloaded_file:
            return HTTPException(status_code=500, detail="Download timeout or file not found")

        # **Rename file to match DOI filename**
        print("rename stuff")
        original_path = os.path.join(DOWNLOAD_DIR, downloaded_file)
        new_path = os.path.join(DOWNLOAD_DIR, f"{doi_filename}.pdf")

        os.rename(original_path, new_path)
        print(f"Renamed downloaded file: {downloaded_file} → {doi_filename}.pdf")

        # **Rotate VPN only if 10 minutes have passed**
        if time.time() - last_vpn_rotation > 600:
            rotate_VPN(vpn_instr, google_check=0)
            last_vpn_rotation = time.time()

        try:
            PdfReader(new_path)
        except PdfReadError:
            print(f"Failed: Invalid PDF file: {doi_filename}.pdf")
            # delete corrupted pdf file
            os.remove(new_path)
            return {"status": "Failed: Invalid PDF file", "file": f"{doi_filename}.pdf", "url": url}
        else:
            pass

        return {"status": "Download successful", "file": f"{doi_filename}.pdf", "url": url}

    except Exception as e:
        return HTTPException(status_code=500, detail=str(e))


async def restart_browser():
    """Gracefully close and restart the browser."""
    global browser

    if browser:
        try:
            await browser.quit()  # Ensure the instance is fully closed
        except Exception:
            pass  # Ignore errors in case the browser is already dead
    ua = UserAgent()
    options.add_argument(f"--user-agent={ua.random}")  # Needs double dashes
    browser = await webdriver.Chrome(options=options)


@app.get("/check_captcha")
async def captcha_endpoint():
   await is_captcha_triggered()
    

@app.get("/rotate")
async def rotate_vpn():
    """Handles VPN rotation and browser reinitialization."""
    await init_browser()
    return {"status": "VPN rotated, browser restarted"}
    
# Run the server
if __name__ == "__main__":
    uvicorn.run(app, host="127.0.0.1", port=8001)
