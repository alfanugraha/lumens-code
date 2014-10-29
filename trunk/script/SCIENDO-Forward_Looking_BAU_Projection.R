##[SCIENDO]=group
##Set_Working_Directory = folder
##Abacus_Project_File = file

Set_Working_Directory = "C:/Users/ANugraha/Desktop/AbacusScenario/" 
Abacus_Project_File = "C:/Users/ANugraha/Documents/Abacus/Tjb2005_2010.car" #work with car file and also supported text file with abacus project format

abacusExecutable = "C:/Progra~2/LUMENS/Abacus/abacus " #the default directory for abacus executable 
systemCommand <- paste(abacusExecutable, Abacus_Project_File)

system(systemCommand)

#you can modify anything from this line, for example, 
#I already added some line to read car file after editing
#and saving Abacus project file to your own working directory 
test <- paste("test.car", sep="")
abacusCar = readLines(test) 
