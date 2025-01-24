# Use grobid/grobid:0.8.0 as the base image
FROM grobid/grobid:0.8.0

# Install Git and Python (if Python is not already installed)
RUN apt-get update && apt-get install -y git python3 python3-pip nano glances time netcat

# Clone the repository
COPY ./src/preprocess_pdf.py /app/preprocess_pdf.py
COPY ./src/grobid_config.json /app/grobid_config.json
COPY ./src/grobid_client_python /grobid_client_python
COPY ./src/startup.sh /startup.sh

# Change directory to the cloned repository
WORKDIR /grobid_client_python

# Install the Python package
RUN python3 setup.py install

# Reset the working directory
WORKDIR /opt/grobid

RUN chmod +x /startup.sh
CMD ["/startup.sh"]
