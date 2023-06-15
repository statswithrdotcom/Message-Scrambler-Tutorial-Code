
############# Sender ############################
Message = "This is a test!"

Mess_split = strsplit(Message, split = "")[[1]]

set.seed(123)
Scrambed_Ind = sample(1:length(Mess_split),length(Mess_split),replace = FALSE)

Mess_Scrammed = Mess_split[Scrambed_Ind]

Mess_Scrammed = paste(Mess_Scrammed, collapse="")
#################################################


############# Receiver ##########################
print(Mess_Scrammed)

Message_Scrammed = strsplit(Mess_Scrammed, split = "")[[1]]

set.seed(123)
Scrambed_Ind = sample(1:length(Message_Scrammed),length(Message_Scrammed),replace = FALSE)

Mess_Unscrammed = rep(0,length(Message_Scrammed))
counter = 0
for(i in Scrambed_Ind){
  counter = counter + 1
  Mess_Unscrammed[i] <- Message_Scrammed[counter]
}

paste(Mess_Unscrammed, collapse = "")
################################################

############ Creating Functions ################
Scrambler <- function(Message, Seed = 123){
  Mess_split = strsplit(Message, split = "")[[1]]
  
  set.seed(Seed)
  Scrambed_Ind = sample(1:length(Mess_split),length(Mess_split),replace = FALSE)

  Mess_Scrammed = paste(Mess_split[Scrambed_Ind], collapse="")
  return(Mess_Scrammed)
}

Unscrambler <- function(Scrambled, Seed = 123){
  Message_Scrammed = strsplit(Scrambled, split = "")[[1]]
  
  set.seed(Seed)
  Scrambed_Ind = sample(1:length(Message_Scrammed),length(Message_Scrammed),replace = FALSE)
  
  Mess_Unscrammed = rep(0,length(Message_Scrammed))
  counter = 0
  for(i in Scrambed_Ind){
    counter = counter + 1
    Mess_Unscrammed[i] <- Message_Scrammed[counter]
  }
  
  return(paste(Mess_Unscrammed, collapse = ""))
}

rand_letters_nums <- function(n){
  Choices = c(letters,LETTERS,0:9)
  Ind = sample(1:length(Choices),n+1,replace = TRUE)
  Output = Choices[Ind]
  Indicator = c("A","E","I","O","U")
  return(c(Indicator,Output))
}

#Tests
Info_Scrambled <- Scrambler("Cryptography is Fun!", Seed = 123)
print(Info_Scrambled)

Info_Unscrambled <- Unscrambler(Info_Scrambled, Seed = 123)
print(Info_Unscrambled)

Rand_Stuff <- rand_letters_nums(10)
print(Rand_Stuff)
#######################################################

################# Modified Functions ##################
Scrambler <- function(Message, Seed = 123, Noise = 100, Indicator = TRUE){
  Mess_split = strsplit(Message, split = "")[[1]]
  
  Mess_split = c(Mess_split, rand_letters_nums(Noise))
  
  set.seed(Seed)
  Scrambed_Ind = sample(1:length(Mess_split),length(Mess_split),replace = FALSE)
  
  Mess_Scrammed = paste(Mess_split[Scrambed_Ind], collapse="")
  return(Mess_Scrammed)
}

Unscrambler <- function(Scrambled, Seed = 123){
  Message_Scrammed = strsplit(Scrambled, split = "")[[1]]
  
  set.seed(Seed)
  Scrambed_Ind = sample(1:length(Message_Scrammed),length(Message_Scrammed),replace = FALSE)
  
  Mess_Unscrammed = rep(0,length(Message_Scrammed))
  counter = 0
  for(i in Scrambed_Ind){
    counter = counter + 1
    Mess_Unscrammed[i] <- Message_Scrammed[counter]
  }
  
  Tentative_Output = paste(Mess_Unscrammed, collapse = "")
  Output = strsplit(Tentative_Output, split = paste(c("A","E","I","O","U"), collapse = ""))[[1]]
  return(Output[1])
}

#Test
Info2 = Scrambler("This is better!", Seed = 123)
print(Info2)

Info2_Unscram = Unscrambler(Info2, Seed = 123)
print(Info2_Unscram)

##########################################################


############### Adding Passwords #########################

Pass_Index <- function(Input){
  Input <- strsplit(Input,split = "")[[1]]
  
  Pos <- as.character(c(""," ", letters,LETTERS,"?",".","!",0:9,"'",
    "/","+","-","<",">","@","$","%","#","^","&","*","(",")","_","=",","))
  
  for(i in 1:length(Input)){
    Counter = 0
    for(j in 1:length(Pos)){
      if(Counter == 0){
        if(Input[i] == Pos[j]){
          Input[i] = j; Counter = 1
        }
      }
    }
  }
  
  return(Input)
}


Password_Protect <- function(Message, Password = "Crypto", Noise = 10){

  Pass_seq = Pass_Index(Password)
  
  for(i in Pass_seq){
    Message = Scrambler(Message, Seed = i, Noise = Noise)
  }
  
  return(Message)
}


Password_Remove <- function(Scrambled, Password = "Crypto"){

  Pass_seq = rev(Pass_Index(Password))

  for(i in Pass_seq){
    Scrambled = Unscrambler(Scrambled, Seed = i)
  }
  
  return(Scrambled)
}

#Test
Info2 = Password_Protect("This is better!", Password = "Test")
print(Info2)

Info2_Unscram = Password_Remove(Info2, Password = "Test")
print(Info2_Unscram)








