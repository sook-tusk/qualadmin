
getwd()

#H---------------------------------------
## Create Output folders (Run once)
#H---------------------------------------

# Create 5_Output folder:
dir.create("./Output")

# Create sub-folders under 5_Output:
dir.create("./Output/01-Txt")
dir.create("./Output/02-Figure")
dir.create("./Output/03-ExcelOutput")
dir.create("./Output/04-RData")

# Confirm
list.files("./Output")

### End ###
