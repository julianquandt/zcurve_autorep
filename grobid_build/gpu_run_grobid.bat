@echo off
echo Enter the host path for the volume, e.g., if you want to process all PDFs in the windows folder C:\p_curve_project\downloads enter it the following way: /c/p_curve_project/downloads (JUST TYPE IT IN AND PRESS ENTER/RETURN KEY):
set /p host_path=
docker run -it -v %host_path%:/mnt/downloads --rm --gpus all --init --ulimit core=0 -p 8070:8070 grobid_pcurve
