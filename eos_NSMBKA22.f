
      
      PROGRAM ASNM        
c     ****************************************************
c     verified 27 Nov 2014
c     +Delta set 1a
c     ****************************************************
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
      INTEGER I
      PI  = 3.14159265358979D0
      HC  = 197.327D0

 

C------------------------------------------------------------------------------
C nuclear matter Saturation density !!!

c     FSU 
       KFO =1.3D0*HC

C------------------------------------------------------------------------------
      RHO = 2D0*KFO*KFO*KFO/(3D0*PI*PI)
 
c
c      OPEN(unit=4,status='unknown',file='durcaBKA22_E2.dat')
c      OPEN(unit=5,status='unknown',file='fracsBKA22_E2.dat')
c      OPEN(unit=1,status='unknown',file='massBKA22_E2.dat')
      OPEN(unit=8,status='unknown',file='eosNSMe2rED_A1.dat')
      OPEN(unit=9,status='unknown',file='eosNSMe2rRHO_A1.dat')  

    



        DO 10 I=1,250
        RHBO=I * 0.004D0

         RB=RHBO*RHO    

           
         KF  = (1.5D0*PI*PI*RB)**(1.D0/3.D0)      

C     CALL ENERGY DENSITY AND BINDING ENERGY
C     
         CALL FERMIM(RB,KFP,KFN,KFE,KFM,YE,YN,YP,YM)
c         CALL FRG2(KFP,KFN,SIG,DEL,V0,B0,MN,MP)
         CALL FED(RHBO,ED,EB,EL)
        
C DIRECT URCA CONDITION  YN1P3= YDE
c         YN1P3=YN**(0.333333D0)
c         YDE=YP**(0.333333D0)+YE**(0.333333D0)
C     
C     CALL PRESSURE
C     
          PRESS= RHO*RHBO*RHBO*func1(RHBO)/(HC*HC*HC)


C     RESULTS
C     
C     ****************************************************
C     RB is Baryon density,  EB is binding energy, ED is energy density
C     PRESS is pressure, YN is neutron fraction and YP is proton fraction
C     YE is electron fraction and YM is muon fraction
c     RB in fm^-3, KF in fm^-1, EB in MeV, ED  and PRESS in MEV/fm^3
C     V0 is omega field, SIG is sigma field, MN is neutron effective mass


C     ****************************************************
c       WRITE(*,*) RHBO,ED,PRESS
c       WRITE(1,*)RHBO,MN,MP

c       WRITE(5,*) RHBO,(KFN/HC),(RB/(HC*HC*HC)),YP,YN,YE,YM

c       WRITE(8,*)RHBO,(KFN/HC),ED,YP
c       WRITE(9,*) RHBO,(RB/(HC*HC*HC)),(KFN/HC),PRESS,EB

       WRITE(8,*)PRESS,ED
       WRITE(9,*)PRESS, (RB/(HC*HC*HC))      

c       WRITE(4,*)RHBO,YP,(RB/(HC*HC*HC)),YN1P3,YDE,PRESS,ED
 10   CONTINUE

         STOP
         END


cc------------------------------------------------------------------------------
        INCLUDE "parset.f"

C------------------------------------------------------------------------------
C     PRESSURE CALCULATION    
      FUNCTION func1(xa)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL func
      h  = 1.D-4
      x1 = xa+h
      x2 = xa-h 
      x3 = xa+2.D0*h
      x4 = xa-2.D0*h    
      func1 = (func(x4)-8.D0*func(x2)+8.D0*func(x1)-func(x3))/(12.D0*h)
      RETURN
      END
      

      
      FUNCTION func(RHBO)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)      
      CALL FED(RHBO,ED,EB,EL)
      func=EB
      RETURN
      END

C------------------------------------------------------------------------------
C      

      
C     ENERGY CALCULATIONS      
      SUBROUTINE FED(RHBO,ED,EB,EL)
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
      PI  = 3.14159265358979D0
      HC  = 197.327D0


C------------------------------------------------------------------------------
C nuclear matter Saturation density !!!

c     FSU 
       KFO =1.3D0*HC

      
C-----------------------------------------------------------------------------
    
      RHO = 2.D0*KFO*KFO*KFO/(3.D0*PI*PI)   
      RB  = RHBO*RHO  
      CALL SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)

      CALL FERMIM(RB,KFP,KFN,KFE,KFM,YE,YN,YP,YM)

C     ****************************************************
C      CHEK NUCLEAR MATTER
C     **************************************************** 
c      RP = 0.0*RB
c      RN = 1.0*RB
c      RE =0.0*RB
c      RM=0.0*RB

c      KFE=(3.D0*PI*PI*RE)**(1.D0/3.D0)
c      KFM=(3.D0*PI*PI*RM)**(1.D0/3.D0)
c      KFP=(3.D0*PI*PI*RP)**(1.D0/3.D0)
c      KFN=(3.D0*PI*PI*RN)**(1.D0/3.D0)
c       EE=0.0D0
c       EM=0.0D0
c     **************************************************** 

      

      CALL FRG2(KFP,KFN,SIG,DEL,V0,B0,MN,MP)

   
       RP = KFP*KFP*KFP/(3.D0*PI*PI)
       RN = KFN*KFN*KFN/(3.D0*PI*PI)
       
   

      U = 0.5D0*MS*MS*SIG*SIG
      U = U + 0.5D0*MD*MD*DEL*DEL
      U = U - 0.5D0*MV*MV*V0*V0
      U = U - 0.5D0*MR*MR*B0*B0
      U = U + 0.333D0*B2*SIG*SIG*SIG
      U = U + 0.25D0*B3*SIG*SIG*SIG*SIG
      U = U - 0.25D0*C1*V0*V0*V0*V0
      U = U - D2*SIG*V0*V0  
      U = U - F2*SIG*B0*B0
      U = U - 0.5D0*D3*SIG*SIG*V0*V0

      U = U - 0.5D0*G3*SIG*SIG*B0*B0
      U = U - 0.5D0*G4*V0*V0*B0*B0
      U = U - 0.25D0*G5*B0*B0*B0*B0

      EP = KFP*DSQRT((KFP*KFP)+(MP*MP))
      EP = EP*(2.D0*KFP*KFP+MP*MP)
      TEMP = DLOG((KFP+DSQRT((KFP*KFP)+(MP*MP)))/MP)
      EP = EP - MP*MP*MP*MP*TEMP
      EP = EP/(8.D0*PI*PI)

      EN = KFN * DSQRT((KFN*KFN)+(MN*MN))
      EN = EN * (2.D0*KFN*KFN+MN*MN)
      TEMN = DLOG((KFN+DSQRT((KFN*KFN)+(MN*MN)))/MN)
      EN = EN - MN*MN*MN*MN*TEMN
      EN = EN/(8.D0*PI*PI)

      EE = KFE * DSQRT((KFE*KFE)+(ME*ME))
      EE = EE * (2.D0*KFE*KFE+ME*ME)
      TEME = DLOG((KFE+DSQRT((KFE*KFE)+(ME*ME)))/ME)
      EE = EE - ME*ME*ME*ME*TEME
      EE = EE /(8.D0*PI*PI)

      EM = KFM * DSQRT((KFM*KFM)+(MO*MO))
      EM = EM * (2.D0*KFM*KFM+MO*MO)
      TEMM = DLOG((KFM+DSQRT((KFM*KFM)+(MO*MO)))/MO)
      EM = EM - MO*MO*MO*MO*TEMM
      EM = EM /(8.D0*PI*PI)



      E = EP+EN+EE+EM+ GV*V0*(RP+RN)+0.5D0*GR*B0*(RP-RN)


      ED = (E + U )/(HC*HC*HC)
      
      EB = (E + U )/RB

     
      EB = EB - MB

      EL= (EE+EM)/(RB*HC*HC*HC)
c      WRITE(*,*)U,SIG,V0
      RETURN
      END

 
c------------------------------------------------------------------------------ 

      SUBROUTINE  FERMIM(RB,KFPF,KFNF,KFEF,KFMF,YE,YN,YP,YM)
C
C   Fermi momenta calculations
C

      IMPLICIT DOUBLE PRECISION (A-H,K-Z)       

      PI  = 3.14159265358979D0
      MUE = 100.D0      
      CALL SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)      
       
       YN0=0.8D0
       YP0=0.2D0
 
 50    RBN = YN0*RB
         RBP = YP0*RB
         KFP =(3.D0*PI*PI*RBP)**(1D0/3D0)
         KFN =(3.D0*PI*PI*RBN)**(1D0/3D0)
 
         CALL FRG2(KFP,KFN,SIG,DEL,V0,B0,MN,MP)        
        
         PREYP = 0.0D0
 20      YP = PREYP-(1.D0/MUE)* FYP(PREYP,MN,MP,RB,ME,MO,SIG,DEL,V0)
     
         IF(((YP-PREYP)*(YP-PREYP)).LT.1.D-24) GOTO 9
         PREYP = YP
         GOTO 20
 9       CONTINUE
             
C        
C     Fraction of each contituent
C
         A = (3.D0*PI*PI*(1-YP))**(2D0/3D0)
         B = (3.D0*PI*PI*YP)**(2D0/3D0)
         CN = (MN*MN)/RB**(2D0/3D0)
         CP = (MP*MP)/RB**(2D0/3D0)
         D = (ME*ME)/RB**(2D0/3D0)
         F = (MO*MO)/RB**(2D0/3D0)        
         G = 1.D0/(3.D0*PI*PI)
         MZ= (MR*MR+2.D0*F2*SIG+G4*V0*V0+G3*SIG*SIG)
         R = 0.5D0*GR*GR*RB**(2.D0/3.D0)*(1.D0-2.D0*YP)/MZ
         FZ=(DSQRT(A+CN)-DSQRT(B+CP)+R)*(DSQRT(A+CN)-DSQRT(B+CP)+R)
         YN= (1.D0-YP)         
         YE= G*(FZ-D)**(3D0/2D0)
         IF ((FZ-F) .GT. 0D0) THEN
            YM = G*(FZ-F)**(3D0/2D0)
         ELSE 
            YM = 0D0
         ENDIF  
    

       IF(((YP-YP0)*(YP-YP0)).LT.1.D-6 ) GOTO 40
        YP0=YP
        YN0=YN
       GOTO 50
 40    CONTINUE
 
       KFEF= (3.D0*YE*RB*PI*PI)**(1D0/3D0)
       KFPF= (3.D0*YP*RB*PI*PI)**(1D0/3D0)
       KFNF= (3.D0*YN*RB*PI*PI)**(1D0/3D0)
       KFMF= (3.D0*YM*RB*PI*PI)**(1D0/3D0)
c      WRITE(*,*)YP,YN,YE,YM,B0
      RETURN
      END



c------------------------------------------------------------------------------

      SUBROUTINE FRG2(KFP,KFN,SIG,DEL,V0,B0,MN,MP)
C
C   meson fields calculation
C
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
      PI  = 3.14159265358979D0
      MUE = 1.D+9      
      CALL SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)  


       RP = KFP*KFP*KFP/(3.D0*PI*PI)
       RN = KFN*KFN*KFN/(3.D0*PI*PI)    

      PRESIG = 0.0D0
      PREV0  = 0.0D0
      PREDEL = 0.0D0

  
 20   SIG = PRESIG-(1.D0/MUE)*FS(PRESIG,PREV0,PREDEL,MB,KFP,
     &     KFN,GS,GR,GD,MS,MR,B2,B3,D2,D3,F2,G3,G4)
      V0 = PREV0-(1.D0/MUE)* FS1(PREV0,PRESIG,MV,MR,GR,GV,KFP,
     &     KFN,C1,D2,D3,F2,G3,G4)     
      DEL = PREDEL-(1.D0/MUE)* FS4(PREDEL,PRESIG,MD,GS,GD,KFP,KFN,MB)

      IF(((SIG-PRESIG)*(SIG-PRESIG)).LT.1.D-28 .AND.
     &     ((DEL-PREDEL)*(DEL-PREDEL)).LT.1.D-28  .AND.  
     &     ((V0-PREV0)*(V0-PREV0)).LT.1.D-28) GOTO 8
      PRESIG = SIG
      PREV0 = V0
    
      PREDEL = DEL      
      GOTO 20
 8    CONTINUE 
      B0 = -0.5D0*(RN-RP)*GR/(MR*MR+2.D0*F2*SIG+G4*V0*V0+G3*SIG*SIG)          
      MN = MB+GS*SIG-GD*DEL
      MP = MB+GS*SIG+GD*DEL 

      RETURN
      END

c------------------------------------------------------------------------------

      FUNCTION FS(SIG,V0,DEL,MB,KFP,KFN,GS,GR,GD,MS,MR,B2,B3,D2,
     &            D3,F2,G3,G4)
C
C  sigma meson calculation
C
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
      PI = 3.14159265358979D0
      RP = 1.D0*KFP*KFP*KFP/(3.D0*PI*PI)
      RN = 1.D0*KFN*KFN*KFN/(3.D0*PI*PI)
      B0 = -0.5D0*(RN-RP)*GR/(MR*MR+2.D0*F2*SIG+G4*V0*V0+G3*SIG*SIG)  
      MN= MB+GS*SIG-GD*DEL
      MP = MB +GS*SIG+GD*DEL
      FS = MS*MS*SIG
      FS = FS + B2*SIG*SIG
      FS = FS + B3*SIG*SIG*SIG
      FS = FS - D2*V0*V0
      FS = FS - D3*SIG*V0*V0
      FS = FS - F2*B0*B0
      FS = FS - G3*SIG*B0*B0

      VN = KFN*DSQRT(((KFN*KFN)+(MN*MN)))
      VIN = DLOG((KFN+DSQRT((KFN*KFN)+(MN*MN)))/MN)
      VN = VN - MN*MN*VIN
      VN = VN*MN
      VN = VN/(2.D0*PI*PI)

      VP = KFP*DSQRT(((KFP*KFP)+(MP*MP)))
      VIP = DLOG((KFP+DSQRT((KFP*KFP)+(MP*MP)))/MP)
      VP = VP - MP*MP*VIP
      VP = VP*MP
      VP = VP/(2.D0*PI*PI)

      V = (VP+VN)*GS
      FS= FS + V
c      write(*,*)(RP+RN),(VP+VN)
      RETURN
      END
      
      
c------------------------------------------------------------------------------      
      
      FUNCTION FS1(V0,SIG,MV,MR,GR,GV,KFP,KFN,C1,D2,D3,F2,G3,G4)
C
C omega meson calculation
C
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
      PI  = 3.14159265358979D0
      RP = 1.D0*KFP*KFP*KFP/(3.D0*PI*PI)
      RN = 1.D0*KFN*KFN*KFN/(3.D0*PI*PI)
      B0 = -0.5D0*(RN-RP)*GR/(MR*MR+2.D0*F2*SIG+G4*V0*V0+G3*SIG*SIG)


      FS1 = MV*MV*V0
      FS1 = FS1 - GV*(RP+RN)
      FS1 = FS1 + 2.D0*D2*SIG*V0
      FS1 = FS1 + D3*SIG*SIG*V0
      FS1 = FS1 + C1*V0*V0*V0
      FS1 = FS1 + G4*V0*B0*B0
      RETURN
      END


       FUNCTION FS3(B0,V0,SIG,MV,MR,GR,GV,KFP,KFN,C1,D2,D3,F2,G3,
     %             G4,G5)
C
C rho meson calculation
C
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
      PI  = 3.14159265358979D0
      RP = 1.D0*KFP*KFP*KFP/(3.D0*PI*PI)
      RN = 1.D0*KFN*KFN*KFN/(3.D0*PI*PI)
 
      FS3 = MR*MR*B0
      FS3 = FS3 - 0.5D0*GR*(RP-RN)
      FS3 = FS3 + 2.D0*F2*SIG*B0
      FS3 = FS3 + F3*SIG*SIG*B0
      FS3 = FS3 + G5*B0*B0*B0
      FS3 = FS3 + G4*V0*V0*B0
      RETURN
      END
c-----------------------------------------------------------------------------

      FUNCTION FS4(DEL,SIG,MD,GS,GD,KFP,KFN,MB)
C
C  Delta meson calculation
C
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
      PI  = 3.14159265358979D0
      MN = MB+GS*SIG-GD*DEL
      MP = MB+GS*SIG+GD*DEL
 
      VN = KFN*DSQRT(((KFN*KFN)+(MN*MN)))
      VIN = DLOG((KFN+DSQRT((KFN*KFN)+(MN*MN)))/MN)
      VN = VN - MN*MN*VIN
      VN = VN*MN
      VN = VN /(2.D0*PI*PI)

      VP = KFP*DSQRT(((KFP*KFP)+(MP*MP)))
      VIP = DLOG((KFP+DSQRT((KFP*KFP)+(MP*MP)))/MP)
      VP = VP - MP*MP*VIP
      VP = VP*MP
      VP = VP /(2.D0*PI*PI)
 
      V = (VP-VN)*GD
      FS4 = MD*MD*DEL+V 
      RETURN
      END
c-----------------------------------------------------------------------------
      FUNCTION FYP(YP,MN,MP,RB,ME,MO,SIG,DEL,V0)
C
C   Function of fraction
C
      IMPLICIT DOUBLE PRECISION (A-H,K-Z)
      PI  = 3.14159265358979D0
      CALL SET(MB,MS,ME,MO,HC,ET2,ET3,EP3,ETR,MV,MR,GS,GV,GR,B2
     &     ,B3,C1,D2,D3,F2,G3,G4,G5,GD,MD)
 
      A = (3.D0*PI*PI*(1.D0-YP))**(2D0/3D0)
      B = (3.D0*PI*PI*YP)**(2D0/3D0)
      CN = (MN*MN)/RB**(2D0/3D0)
      CP = (MP*MP)/RB**(2D0/3D0)
      D = (ME*ME)/RB**(2D0/3D0)
      F = (MO*MO)/RB**(2D0/3D0)    
      G = 1.D0/(3.D0*PI*PI)
      MZ= (MR*MR+2.D0*F2*SIG+G4*V0*V0+G3*SIG*SIG)
      R = 0.5D0*GR*GR*RB**(2.D0/3.D0)*(1.D0-2.D0*YP)/MZ   
      H = (DSQRT(A+CN)-DSQRT(B+CP)+R)*(DSQRT(A+CN)-DSQRT(B+CP)+R)
      IF ((H-F).GE.0.D0) THEN 
         FYP = -G**(2D0/3D0)*((H-D)**(3D0/2D0)
     &        +(H-F)**(3D0/2D0))**(2D0/3D0)+YP**(2D0/3D0)
      ELSE     
         FYP =-G**(2D0/3D0)*(H-D)+YP**(2D0/3D0)
      ENDIF
      RETURN
      END

c-----------------------------------------------------------------------------
