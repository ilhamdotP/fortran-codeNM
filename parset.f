c
c     BKA22 eta2rho Family
c
C--------------------------------------------------------------
C    A1

      SUBROUTINE SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =9.22249D0
      GD = 0.0D0 
      ETR4 = 2.5D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = 0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    A2

      SUBROUTINE A2SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =9.56555D0
      GD = 0.0D0 
      ETR4 = 5.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    A3

      SUBROUTINE A3SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =9.88976D0
      GD = 0.0D0 
      ETR4 = 7.5D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    A4

      SUBROUTINE A4SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR = 10.19638D0
      GD = 0.0D0 
      ETR4 = 10.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    A5

      SUBROUTINE A5SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =11.31353D0
      GD = 0.0D0 
      ETR4 = 20.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    A6

      SUBROUTINE A6SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =13.17712D0
      GD = 0.0D0 
      ETR4 = 40.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    A7

      SUBROUTINE A7SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =14.73284D0
      GD = 0.0D0 
      ETR4 = 60.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    B1

      SUBROUTINE B1SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =9.72514D0
      GD = 2.0D0 
      ETR4 = 2.5D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    B2

      SUBROUTINE B2SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =10.08454D0
      GD = 2.0D0 
      ETR4 = 5.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    B3

      SUBROUTINE B3SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =10.42760D0
      GD = 2.0D0 
      ETR4 = 7.5D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    B4

      SUBROUTINE B4SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR = 10.75420D0
      GD = 2.0D0 
      ETR4 = 10.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    B5

      SUBROUTINE B5SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =11.36744D0
      GD = 2.0D0 
      ETR4 = 15.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    B6

      SUBROUTINE B6SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =11.93793D0
      GD = 2.0D0 
      ETR4 = 20.D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    B7

      SUBROUTINE B7SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =12.47577D0
      GD = 2.0D0 
      ETR4 = 25D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    B8

      SUBROUTINE B8SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =13.46223D0
      GD = 2.0D0 
      ETR4 = 35.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    B9

      SUBROUTINE B9SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =14.36198D0
      GD = 2.0D0 
      ETR4 = 45.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 

c--------------------------------------------------------------------
C    B10

      SUBROUTINE B10SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR = 15.58971D0
      GD = 2.0D0 
      ETR4 = 60.D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 

C--------------------------------------------------------------
C    C1

      SUBROUTINE C1SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =11.09110D0
      GD = 4.0D0 
      ETR4 = 2.5D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 

C--------------------------------------------------------------
C    C2

      SUBROUTINE C2SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =11.50190D0
      GD = 4.0D0 
      ETR4 = 5.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    C3

      SUBROUTINE C3SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =11.89284D0
      GD = 4.0D0 
      ETR4 = 7.5D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    C4

      SUBROUTINE C4SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =12.26857D0
      GD = 4.0D0 
      ETR4 = 10.D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    C5

      SUBROUTINE C5SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =12.97857D0
      GD = 4.0D0 
      ETR4 = 15.D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    C6

      SUBROUTINE C6SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =13.64459D0
      GD = 4.0D0 
      ETR4 = 20.D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    C7

      SUBROUTINE C7SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =14.27291D0
      GD = 4.0D0 
      ETR4 = 25.D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    C8

      SUBROUTINE C8SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =14.86604D0
      GD = 4.0D0 
      ETR4 = 30.D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    C9

      SUBROUTINE C9SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =15.43530D0
      GD = 4.0D0 
      ETR4 = 35.D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    D1

      SUBROUTINE D1SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =13.05008D0
      GD = 6.0D0 
      ETR4 = 2.5D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    D2

      SUBROUTINE D2SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =13.53011D0
      GD = 6.0D0 
      ETR4 = 5.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    D3

      SUBROUTINE D3SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =13.99381D0
      GD = 6.0D0 
      ETR4 = 7.5D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    D4

      SUBROUTINE D4SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =14.44256D0
      GD = 6.0D0 
      ETR4 = 10.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    D5

      SUBROUTINE D5SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =15.29707D0
      GD = 6.0D0 
      ETR4 = 15.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    D6

      SUBROUTINE D6SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR = 16.10634D0
      GD = 6.0D0 
      ETR4 = 20.D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    D7

      SUBROUTINE D7SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =16.87289D0
      GD = 6.0D0 
      ETR4 = 25.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    D8

      SUBROUTINE D8SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =17.60677D0
      GD = 6.0D0 
      ETR4 = 30.D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    D9

      SUBROUTINE D9SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =18.30923D0
      GD = 6.0D0 
      ETR4 = 35.D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    E1

      SUBROUTINE E1SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =15.36606D0
      GD = 8.0D0 
      ETR4 = 2.5D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    E2

      SUBROUTINE E2SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =15.92966D0
      GD = 8.0D0 
      ETR4 = 5.D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    E3

      SUBROUTINE E3SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =17.01766D0
      GD = 8.0D0 
      ETR4 = 10.D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    E4

      SUBROUTINE E4SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =18.05564D0
      GD = 8.0D0 
      ETR4 = 15.D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    E5

      SUBROUTINE E5SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =19.04838D0
      GD = 8.0D0 
      ETR4 = 20.D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
C--------------------------------------------------------------
C    E6

      SUBROUTINE E6SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
C     Initialisation
      PI  = 3.14159265358979D0
      HC  = 197.327D0
      ME  = 0.511D0
      MO  = 105.6D0

      GS = 0.8462D0*4.D0*PI
      GV = 1.1089D0*4.D0*PI
c----------------------------------------------------------
      GR =20.00053D0
      GD = 8.0D0 
      ETR4 = 25.0D0
c---------------------------------------------------------- 
      K2  = -1.5500D0
      K3  =  2.13451D0
      ET2 = -0.1555D0
      ET3 = 0.0697D0

      ETR3 =  0.0D0
 
      ETR = -0.0D0

      EP3 = 5.8253D0
      EPR3= 0.0D0
      MB  = 939.0D0
  
 
      MS  =  0.5302D0*MB
      MV  =  782.5D0
      MR  =  770.D0
      MD  =  980.D0

   
  
      B2 = GS*MS*MS*K2/(2.D0*MB)
      B3 = GS*GS*MS*MS*K3/(6.D0*MB*MB)
      C1 = GV*GV*EP3/(6.D0)
      D2 = GS*MV*MV*ET2/(2.D0*MB)
      D3 = GS*GS*MV*MV*ET3/(2.D0*MB*MB)  
      F2 = ETR*GS*MR*MR/(2.D0*MB)
      G3 = GS*GS*MR*MR*ETR3/(2.D0*MB*MB)
      G4 = GV*GV*MR*MR*ETR4/(2.D0*MB*MB)
      G5 = GR*GR*EPR3/(6.D0)     


c--------------------------------------------------------------
      RETURN
      END 
c------------------------------------------------------------
