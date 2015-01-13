##[SCIENDO]=group
##Abacus_Project_File = file

if (file.exists("C:/Program Files (x86)/LUMENS/AbacusFull")){
  abacusExecutable = "C:/Progra~2/LUMENS/AbacusFull/abacus "
} else{
  abacusExecutable = "C:/Progra~1/LUMENS/AbacusFull/abacus "
}

systemCommand <- paste(abacusExecutable, Abacus_Project_File)

system(systemCommand)