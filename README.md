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

![RPackageDownloadTracker_small](https://user-images.githubusercontent.com/15528061/194072390-51e5aed6-86c4-48e7-99bb-58da639f9fd9.gif)
