### How to reproduce this web app?

1. Clone the repo.  
`git clone https://github.com/choux130/RPackageDownloadTracker.git`

2. Go to the project directory.  
`cd RPackageDownloadTracker`  

3. Build the docker image.  
`docker build -t rpkgdownloadtracker .` 

4. Run the image in a container.   
`docker run -p 3838:3838 rpkgdownloadtracker`

5. Open the app in localhost (It might take a few seconds for the first time load).  
http://localhost:3838/

![image](https://drive.google.com/uc?export=view&id=15i_QHIam7GqaZhbM-7v18x73YYO_l1S3)
