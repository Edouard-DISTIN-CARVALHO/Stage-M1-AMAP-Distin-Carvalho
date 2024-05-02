ListX<-seq(0,100,length=1001)

myFunc<-function(x,a=0.1,A=2){ return((A*exp(-a*x)))}
plot(x=ListX,y=myFunc(x=ListX,a=2),type="l",col="green")


# Définition de la fonction sinus à oscillation amortie
damped_sinus <- function(t, A, omega, gamma, phi) {
  A * exp(-gamma * t) * sin(omega * t + phi)
}

t <- seq(0, 7, length.out = 100)  # Séquence de temps
A <- 1  # Amplitude
omega <- 1.5*pi+1  # Fréquence angulaire
gamma <- 0.5  # Facteur d'amortissement
phi <- 1-pi/3  # Phase
y <- damped_sinus(t, A, omega, gamma, phi)
plot(t, y, type = "l", main = "Modèle théorique", xlab = "Temps", ylab = "NPP")
 