# Script to upload to shiny apps


rsconnect::deployApp(
  
appFileManifest  =  "files_for_upload.txt",
# appName = "" # change the name if i want instead of the folder name

account = "sglcy"
  
)
