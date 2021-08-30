#Strassen algorithm
#The Strassen algorithm is an algorithm used for matrix multiplication
#faster than standard matrix multiplication algorithm
#but slower than the fastest known algorithm (Coppersmith-Winograd algorithm)

                                #for square matrix    
A = matrix ( c (7 ,31 ,13 ,106 ,24 ,19 ,51 ,68 ,139 ,127 ,121 ,117 ,13 ,105 ,53 ,59) ,
         byrow =T , nrow =4)
A
B = matrix ( c (22 ,111 ,93 ,181 ,155 ,42 ,120 ,17 ,171 ,115 ,26 ,26 ,167 ,203 ,6 ,31) ,
         byrow =T , nrow =4)
B
                
 #step1
A11 = A [1:2 ,1:2]
A11
A12 = A [1:2 ,3:4]
A12
A21 = A [3:4 ,1:2]
A21
A22 = A [3:4 ,3:4]
A22

B11 = B [1:2 ,1:2]
B11
B12 = B [1:2 ,3:4]
B12
B21 = B [3:4 ,1:2]
B21
B22 = B [3:4 ,3:4]
B22

                               #step2
M1 = ( A11 + A22 ) %*% ( B11 + B22 )
M1
M2 = ( A21 + A22 ) %*% B11
M2
M3 = A11 %*% ( B12 - B22 )
M3
M4 = A22 %*% ( B21 - B11 )
M4
M5 = ( A11 + A12 ) %*% B22
M5
M6 = ( A21 - A11 ) %*% ( B11 + B12 )
M6
M7 = ( A12 - A22 ) %*% ( B21 + B22 )
M7

                              #Step -3 
C11 = M1 + M4 - M5 + M7
C12 = M3 + M5
C21 = M2 + M4
C22 = M1 - M2 + M3 + M6


                              #Step -4 
C = rbind ( cbind ( C11 , C12 ) , cbind ( C21 , C22 ) )
C
A %*% B
all ( C == A %*% B )
         



# this function works for only square matrics of even order
strassen.square<- function(A,B){
    A.r.split<-floor(nrow(A)/2)
    B.r.split<-floor(nrow(B)/2)
    A.c.split<-floor(ncol(A)/2)
    B.c.split<-floor(ncol(B)/2)
    
A11 = A [1:A.r.split ,1:A.c.split]
A12 = A [1:A.r.split ,(A.c.split+1):ncol(A)]
A21 = A [(A.r.split+1):nrow(A) ,1:A.c.split]
A22 = A [(A.r.split+1):nrow(A) ,(A.c.split+1):ncol(A)]

B11 = B [1:B.r.split ,1:B.c.split]
B12 = B [1:B.r.split ,(B.c.split+1):ncol(B)]
B21 = B [(B.r.split+1):nrow(B) ,1:B.c.split]
B22 = B [(B.r.split+1):nrow(B) ,(B.c.split+1):ncol(B)]
                               #step2
M1 = ( A11 + A22 ) %*% ( B11 + B22 )
M2 = ( A21 + A22 ) %*% B11
M3 = A11 %*% ( B12 - B22 )
M4 = A22 %*% ( B21 - B11 )
M5 = ( A11 + A12 ) %*% B22
M6 = ( A21 - A11 ) %*% ( B11 + B12 )
M7 = ( A12 - A22 ) %*% ( B21 + B22 )

                              #Step -3 
C11 = M1 + M4 - M5 + M7
C12 = M3 + M5
C21 = M2 + M4
C22 = M1 - M2 + M3 + M6


                              #Step -4 
C = rbind ( cbind ( C11 , C12 ) , cbind ( C21 , C22 ) )
C
#A %*% B
#all ( C == A %*% B )
}

strassen.square(A,B)

strassen.square(A,B)==A%*%B





A<-matrix(sample(100,16,replace=TRUE),nrow=4)
A

B<-matrix(sample(100,16,replace=TRUE),nrow=4)
B

strassen.square(A,B)==A%*%B

#################################################################
#Strassen algorithm for rectangular matrix

A = matrix ( c (7 ,31 ,13 ,24 ,19 ,51 ,139 ,127 ,121 ,13 ,105 ,53) , byrow =
               T , nrow =4)
A
B = matrix ( c (22 ,111 ,93 ,181 ,155 ,42 ,120 ,17 ,171 ,115 ,26 ,26) ,
             byrow =T , nrow =3)
B
A = "[<-"( matrix (0 , 4 , 4) , 1: nrow ( A ) , 1: ncol ( A ) , value = A )
A
B = "[<-" ( matrix (0 , 4 , 4) , 1: nrow ( B ) , 1: ncol ( B ) , value = B )
B


A11 <- A [1:2 ,1:2]
A11
A12 <- A [1:2 ,3:4]
A12
A21 <- A [3:4 ,1:2]
A21
A22 <- A [3:4 ,3:4]
A22

B11 <- B [1:2 ,1:2]
B11
B12 <- B [1:2 ,3:4]
B12
B21 <- B [3:4 ,1:2]
B21
B22 <- B [3:4 ,3:4]
B22

M1 <- ( A11 + A22 ) %*% ( B11 + B22 )
M1
M2 <- ( A21 + A22 ) %*% B11
M2
M3 <- A11 %*% ( B12 - B22 )
M3
M4 <- A22 %*% ( B21 - B11 )
M4
M5 <- ( A11 + A12 ) %*% B22
M5
M6 <- ( A21 - A11 ) %*% ( B11 + B12 )
M6
M7 <- ( A12 - A22 ) %*% ( B21 + B22 )
M7


C11 <- M1 + M4 - M5 + M7
C12 <- M3 + M5
C21 <- M2 + M4
C22 <- M1 - M2 + M3 + M6

C <- rbind ( cbind ( C11 , C12 ) , cbind ( C21 , C22 ) )
C

A%*%B


all ( C == A %*% B )
#####




A<-matrix(c(sample(50,35),replace = TRUE),nrow=6)
B<-matrix(c(sample(50,35),replace = TRUE),nrow=6)
A
strassen.square(A,B)


strassen.square(A,B)==A%*%B


