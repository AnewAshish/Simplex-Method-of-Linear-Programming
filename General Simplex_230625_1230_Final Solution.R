#Taking Inputs and forming the full table
eqn_no <- as.integer(readline("What are number of equations? "))
vars <- as.integer(readline(paste("What are number of variables?")))
Total_Coef <- eqn_no * vars
matrix_raw <- matrix(data=0,nrow = eqn_no,ncol = vars+eqn_no+1)
matrix_raw
xtra_vars <- vars+1
for (i in 1:eqn_no){
  for (j in 1:vars){
    matrix_raw[i,j] <- as.double(readline(paste("Enter x",j,"of eqn",i,": ")))
  }
}
for (i in 1: eqn_no){
    opt <- as.integer(readline(paste("Enter 1 if operator is < and 0 if operator is > for Equation ",i,": ")))

    if (opt ==1){
      matrix_raw[i,i+vars] <- 1
    }else{
      matrix_raw[i,i+vars] <- -1
    }
  
}
for (i in 1: eqn_no){
  matrix_raw[i,ncol(matrix_raw)] <- as.double(readline(paste("Enter the Constraints for Equation",i,": ")))
  }
  

matrix_raw
obj_row <- c(integer(ncol(matrix_raw)))
for (i in 1:vars){
  obj_row[i] <- as.double(readline(paste("Enter X",i," coefficients of Objective Function: ")))
}
full_matrix <- rbind(matrix_raw,obj_row)
full_matrix
Yb <- character(0)
for (i in 1:nrow(full_matrix)){
  if (i == nrow(full_matrix)) {
    Yb[i] <- "Z"
  }else{
  Yb[i] <- paste("x",i,sep = "")
  }
}
Yb
matrix <- data.frame(Yb,full_matrix)
matrix
simplex_funct <- function(matrix){
  max_iterations <- 100
  counter <- 1
  
  while (any(matrix[n_row, -1] < 0) && counter <= max_iterations) {

  #finding the pivot column
  n_row <- nrow(matrix)
  sl_row <- nrow(matrix)-1
  sl_row
  n_col <- ncol(matrix)-1
  last_col <- ncol(matrix)
  pvt_col <- (which(matrix[n_row,2:n_col]==min(matrix[n_row,2:n_col]))[1])+1
  pvt_col
  #after division by the coefficient 
  Coeff_div <- character(0)
  for (i in 1:sl_row){
    if (matrix[i,last_col] != 0){
      Coeff_div[i] <- matrix[i,last_col]/matrix[i,pvt_col]
    }else{
      if (matrix[i,last_col]<0){
        Coeff_div[i] <- -1
      }else{
        Coeff_div[i] <- 0
      }
    }
    
  }
  Coeff_div
  min_coef <- min(Coeff_div[which(Coeff_div>=0)])
  pvt_row <- which(Coeff_div==min_coef)[1]
  pvt_row
  pvt_ele <- matrix[pvt_row,pvt_col]
  pvt_ele
  matrix[pvt_row,2:last_col] <- matrix[pvt_row,2:last_col]/pvt_ele
  matrix
  for (i in 1:n_row){
    if (i != pvt_row){
      matrix[i,2:last_col] <- matrix[i,2:last_col]-matrix[i,pvt_col]*matrix[pvt_row,2:last_col]
    }
  }
  matrix[pvt_row,1] <- trimws(paste("x",pvt_col-1,sep = ""))
  counter <- counter + 1
  }
  return(matrix)

}
result <- simplex_funct(matrix)
result