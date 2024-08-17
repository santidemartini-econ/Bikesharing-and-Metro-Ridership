# define the function
remove.accents <- function(s) {
  
  # 1 character substitutions
  old1 <- "Á"
  new1 <- "A"
  s1 <- chartr(old1, new1, s)
  
  # 2 character substitutions 
  old2 <- c("É")
  new2 <- c("E")
  s2 <- chartr(old2, new2, s1)
  
  # 2 character substitutions 
  old2 <- c("Í")
  new2 <- c("I")
  s3 <- chartr(old2, new2, s2)
  
  # 2 character substitutions 
  old2 <- c("Ó")
  new2 <- c("O")
  s4 <- chartr(old2, new2, s3)
  
  # 2 character substitutions Ase eeee
  old2 <- c("Ú")
  new2 <- c("U")
  
  s5 <- chartr(old2, new2, s4)
  
  
  # 2 character substitutions 
  old2 <- c("Ü")
  new2 <- c("U")
  
  s6 <- chartr(old2, new2, s5)
  
  s6
#return(s5)
}
